*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 20/02/2013                                              &*
*& Descrição: Solicitacao de ordem de venda                           &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                         03.08.2010                            &*
*& Marcos Faneli                08.07.2014                            &*
*& Marcos Faneli                21.07.2014                            &*
*& Welgem Barbosa               06.04.2016                            &*
*&--------------------------------------------------------------------&*
*
REPORT  zsdr0022.
INCLUDE <icon>.
INCLUDE <cl_alv_control>.
TYPE-POOLS: vrm, ustyp, slis, f4typ.

TABLES vbak.

CONSTANTS: on  TYPE raw4 VALUE cl_gui_alv_grid=>mc_style_enabled,
           off TYPE raw4 VALUE cl_gui_alv_grid=>mc_style_disabled.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_itens,
         status_icon(4),
         posnr            TYPE zsdt0053-posnr,
         fixacao          TYPE zsdt0053-fixacao,
         matnr            TYPE zsdt0053-matnr,
         maktx            TYPE makt-maktx,
         werks            TYPE zsdt0053-werks,
         auart            TYPE zsdt0053-auart,
         ponto_c          TYPE zsdt0053-ponto_c,
         terminal         TYPE zsdt0053-terminal,
         lgort            TYPE zsdt0053-lgort,
         charg            TYPE zsdt0053-charg,
         zmeng            TYPE zsdt0053-zmeng,
         brgew            TYPE zsdt0053-brgew,
         zieme            TYPE zsdt0053-zieme,
         dmbtr            TYPE zsdt0053-dmbtr,
         lotes_vol(4),
         vlrtot           TYPE zsdt0053-vlrtot,
         volum            TYPE zsdt0053-volum,
         voleh            TYPE zsdt0053-voleh,
         pmein            TYPE zsdt0053-pmein,
         kursf            TYPE zsdt0053-kursf,
         valdt            TYPE zsdt0053-valdt,
         vbeln            TYPE zsdt0053-vbeln,
         container        TYPE zsdt0053-container,
         item_edit        TYPE zsdt0053-item_edit,
         kunnr            TYPE zsdt0053-kunnr,
         status           TYPE zsdt0053-status,
         status_itm       TYPE zsdt0053-status_itm,
         navio            TYPE zsdt0053-navio,
         porto            TYPE zsdt0053-porto,
         p_porto          TYPE zsdt0053-p_porto,
         vlt_porto        TYPE zsdt0053-vlt_porto,
         instrucao        TYPE zsdt0053-instrucao,
         instrucao_ant    TYPE zsdt0053-instrucao_ant,
         doc_precedente   TYPE zsdt0053-doc_precedente,
         data_lib_frame   TYPE zsdt0053-data_lib_frame,
         desc_absoluto    TYPE zsdt0053-desc_absoluto,
         kvgr3            TYPE zsdt0053-kvgr3,
         ck_troca_nota    TYPE zsdt0053-ck_troca_nota,
         numero_ruc       TYPE zsdt0053-numero_ruc,
         id_nomeacao_tran TYPE zsdt0053-id_nomeacao_tran,
         codigo_ra        TYPE zsdt0053-codigo_ra,
         kvgr5            TYPE zsdt0053-kvgr5,
         tp_ato           TYPE zsdt0053-tp_ato, "#113631-18.01.2024-JT-inicio
         nr_drawback      TYPE zsdt0053-nr_drawback, "#113631-18.01.2024-JT-inicio
         qtd_drawback     TYPE zsdt0053-qtd_drawback, "#113631-18.01.2024-JT-inicio
         style            TYPE lvc_t_styl,
         line_color(4)    TYPE c,
       END OF ty_itens,

       tty_itens  TYPE TABLE OF ty_itens,
       tty_mp_mod TYPE TABLE OF lvc_s_modi,  "#113631-18.01.2024-JT-inicio

       BEGIN OF ty_pgt_ant,
         posnr         TYPE zsdt0054-posnr,
         valdt         TYPE zsdt0054-valdt,
         dmbtr         TYPE zsdt0054-dmbtr,
         kursf         TYPE zsdt0054-kursf,
         vlr_real      TYPE zsdt0054-vlr_real,
         adiant        TYPE zsdt0054-adiant,
         augbl         TYPE bsad-augbl,
         augdt         TYPE bsad-augdt,
         perc_adiant   TYPE zsdt0054-perc_adiant,
         nr_provis_inv TYPE zsdt0054-nr_provis_inv,
         dt_emissaopi  TYPE zsdt0054-dt_emissaopi,
         dt_retornopi  TYPE zsdt0054-dt_retornopi,
         style         TYPE lvc_t_styl,
       END OF ty_pgt_ant,


       BEGIN OF ty_cond_esp,
         fixacao  TYPE zsdt0073-fixacao,
         zterm    TYPE zsdt0073-zterm,
         qte_venc TYPE zsdt0073-qte_venc,
         style    TYPE lvc_t_styl,
       END OF ty_cond_esp,

       BEGIN OF ty_logistica,
         fixacao      TYPE zsdt0055-fixacao,
         data_progr   TYPE zsdt0055-data_progr,
         id           TYPE zsdt0055-id,
         zmeng        TYPE zsdt0055-zmeng,
         zieme        TYPE zsdt0055-zieme,
         valdt_hedge  TYPE zsdt0055-valdt_hedge,
         cadencia_qte TYPE zsdt0055-cadencia_qte,
         status       TYPE zsdt0055-status,
         style        TYPE lvc_t_styl,
       END OF ty_logistica,

       BEGIN OF ty_posicao,
         vlr_f_vencida TYPE bsid-dmbtr,
         vlr_f_avencer TYPE bsid-dmbtr,
         vlr_f_adiant  TYPE bsid-dmbtr,
         vlr_total     TYPE bsid-dmbtr,
         vlr_total_mov TYPE bsid-dmbtr,
         vlr_limite    TYPE knkk-klimk,
         vlr_saldo     TYPE bsid-dmbtr,
         utilizado     TYPE bsid-dmbtr,
         sdo_ov_emit   TYPE vbak-netwr,
       END OF ty_posicao,

       BEGIN OF ty_motivo,
         id_historico TYPE zsdt0069-id_historico,
         nro_sol_ov   TYPE zsdt0069-nro_sol_ov,
         status       TYPE zsdt0069-status,
         motivo       TYPE zsdt0069-motivo,
         usnam        TYPE zsdt0069-usnam,
         data_atual   TYPE zsdt0069-data_atual,
         hora_atual   TYPE zsdt0069-hora_atual,
       END OF ty_motivo,

       BEGIN OF ty_preco.
         INCLUDE TYPE zsds006.
*TYPES:  FORMULA2 TYPE ZSDS006-FORMULA,
TYPES:  "NIVEL2     TYPE ZSDS006-NIVEL,
  formula2(20),   "TYPE BSID-DMBTR,
         waers         TYPE bsid-waers,
         ocbot         TYPE zsdt0059-ocbot,
         preco         TYPE zsdt0059-preco,
*        C_DECIMAIS TYPE ZSDT0059-C_DECIMAIS,
         cbot(6),
         posnr         TYPE zsdt0059-posnr,
         zmeng         TYPE zsdt0059-zmeng,
         valdt         TYPE zsdt0059-valdt,
         safra         TYPE zsdt0059-safra,
         monat         TYPE zsdt0059-monat,
         item_key      TYPE lvc_nkey,
         style         TYPE lvc_t_styl,
         line_color(4) TYPE c,     "Used to store row color attributes
       END OF ty_preco,

       BEGIN OF ty_preco_n.
         INCLUDE TYPE zsds006.
TYPES: posnr        TYPE zsdt0059-posnr,
         cbot(6),
         invisible(1),
         waers        TYPE bsid-waers,
         preco        TYPE zsdt0059-preco,
         ocbot        TYPE zsdt0059-ocbot,
*         safra        TYPE zsdt0059-safra,
         posnr1       TYPE zsdt0059-posnr1,
         valdt_hedge  TYPE zsdt0059-valdt_hedge,
       END OF ty_preco_n.

TYPES: BEGIN OF t_cursor,              "Typ für Cursor-Position
         fname LIKE d021s-fnam,        "Feldname
         pos   LIKE sy-stepl,            "Loop-Zeile auf akt. Seite
         value LIKE d021s-fnam,        "Inhalt des Dynprofeldes
         "Zusatzinfo für Cursor-Position in der Liste
         tc    LIKE dd04l-rollname,       "Table-Control-Name (tc+)
         tcsec LIKE dd04l-rollname,    "TC-Zusatzattr.name (tc+_sec)
         line  LIKE sy-stepl,           "Zeile in ITAB
       END OF t_cursor,

       BEGIN OF ty_bsad,
         belnr TYPE bsad-belnr,
         audgt TYPE bsad-augdt,
         augbl TYPE bsad-augbl,
       END OF ty_bsad,

       BEGIN OF ty_saldo,
         vbeln TYPE vbfa-vbeln,
         werks TYPE zsdt0053-werks,
         zmeng TYPE zsdt0053-zmeng,
         total TYPE zsdt0053-zmeng,
         saldo TYPE zsdt0053-zmeng,
       END OF ty_saldo,

       BEGIN OF ty_fix_red,
         fixacao TYPE zsdt0053-fixacao,
       END OF ty_fix_red,

       BEGIN OF ty_adto_ext,
         bukrs         TYPE zsdt0063-bukrs,
         posnr         TYPE zsdt0063-posnr,
         valdt         TYPE zsdt0063-valdt,
         dmbtr         TYPE zsdt0063-dmbtr,
         waers         TYPE zsdt0063-waers,
         lifnr         TYPE zsdt0063-lifnr,
         name1         TYPE lfa1-name1,
         banks         TYPE zsdt0063-banks,
         bankl         TYPE zsdt0063-bankl,
         bankn         TYPE zsdt0063-bankn,
         swift         TYPE zsdt0063-swift,
         adiant        TYPE zsdt0063-adiant,
         nr_provis_inv TYPE zsdt0063-nr_provis_inv,
         augbl         TYPE bsid-augbl,
         augdt         TYPE bsid-augdt,
         style         TYPE lvc_t_styl,
       END OF ty_adto_ext,

       BEGIN OF ty_002,
         verid    TYPE zppt0002-verid,
         werks    TYPE zppt0002-werks,
         cd_safra TYPE zppt0002-cd_safra,
         lgort    TYPE zppt0002-lgort,
       END OF ty_002,

       BEGIN OF ty_004,
         verid TYPE zppt0004-verid,
         tipo  TYPE zppt0004-tipo,
       END OF ty_004,

       BEGIN OF ty_form_lote,
         status(4),
         posnr           TYPE zsdt0066-posnr,
         matnr           TYPE zsdt0066-matnr,
         maktx           TYPE makt-maktx,
         werks           TYPE zsdt0066-werks,
         lgort           TYPE zsdt0066-lgort,
         charg           TYPE zsdt0066-charg,
         classificacao   TYPE zsdt0066-classificacao,
         zmeng           TYPE zsdt0066-zmeng,
         ponto_c         TYPE zsdt0066-ponto_c,
         zieme           TYPE zsdt0066-zieme,
         volum           TYPE zsdt0066-volum,
         voleh           TYPE zsdt0066-voleh,
         dmbtr           TYPE zsdt0066-dmbtr,
         pmein           TYPE zsdt0066-pmein,
         vlrtot          TYPE zsdt0066-vlrtot,
         waerk           TYPE zsdt0066-waerk,
         kunnr           TYPE zsdt0066-kunnr,
         instrucao       TYPE zsdt0066-instrucao,
         terminal        TYPE zsdt0066-terminal,
         lentrega        TYPE zsdt0066-lentrega,
         inco1           TYPE zsdt0066-inco1,
         inco2           TYPE zsdt0066-inco2,
         libra_to        TYPE zsdt0066-libra_to,
         usd_to          TYPE zsdt0066-usd_to,
         vlr_tot_frm_usd TYPE zsdt0066-vlr_tot_frm_usd,
         vbeln           TYPE zsdt0066-vbeln,
         auart           TYPE zsdt0066-auart,
         dco             TYPE zsdt0066-dco,
         aviso           TYPE zsdt0066-aviso,
         ztrocanota      TYPE zsdt0066-ck_troca_nota,
         kvgr5           TYPE zsdt0066-kvgr5,
         style           TYPE lvc_t_styl,
       END OF ty_form_lote,

       BEGIN OF ty_pedido,
         desc_material TYPE makt-maktx,
         desc_centro   TYPE t001w-name1,
         desc_pc       TYPE lfa1-name1,
         desc_forn     TYPE lfa1-name1,
         desc_le       TYPE kna1-name1,
         style         TYPE lvc_t_styl.
         INCLUDE STRUCTURE zsdt0184.
TYPES: END OF ty_pedido.

TYPES: BEGIN OF ty_screen,
         id      TYPE i,
         n_campo TYPE c LENGTH 30,
         tela    TYPE sy-dynnr,
         nome    TYPE c LENGTH 30,
       END OF ty_screen,

       BEGIN OF ty_itens_frete,
         seq        TYPE sy-tabix,
         nro_ov     TYPE zsdt0051-nro_sol_ov,
         fixacao    TYPE zsdt0053-fixacao,
         status_itm TYPE zsdt0053-status_itm,
         direcao    TYPE c,
         excluir    TYPE c,
       END OF ty_itens_frete.

TYPES: BEGIN OF ty_0045_.
         INCLUDE TYPE zsdt0045.
TYPES:   lgort TYPE zppt0002-lgort,
       END OF ty_0045_.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_col_name,
         cname TYPE c LENGTH 30,
         fname TYPE c LENGTH 30,
       END OF ty_col_name.

"Estrutura para controle de SALDO.
TYPES: BEGIN OF ty_saldo_frame,
         tabix      TYPE sy-tabix,
         nro_sol_ov TYPE zsdt0095-nro_sol_ov,
         fixacao    TYPE zsdt0095-fixacao,
         valor      TYPE zsdt0095-qtd_hedge,
         saldo      TYPE zsdt0095-qtd_hedge,
         data       TYPE zsdt0095-valdt_hedge,
       END OF ty_saldo_frame.

* Extrutura para guardar os Zmeng e PosNR
TYPES: BEGIN OF ty_var_guar,
         posnr     TYPE zsdt0053-posnr,
         fixacao   TYPE zsdt0053-fixacao,
         zmeng     TYPE zsdt0053-zmeng,
         " 21.03.2025 - RAMON -->
         charg_old TYPE zsdt0053-charg,
         charg     TYPE zsdt0053-charg,
         charg_x   TYPE flag,
         updated   TYPE flag,
         principal TYPE flag,
         " 21.03.2025 - RAMON -->
         guar      TYPE c,
         " 20.09.2024 - 147331 - RAMON -->
         difer     TYPE zsdt0053-zmeng,
         final     TYPE zsdt0053-zmeng,
         " 20.09.2024 - 147331 - RAMON --<
       END OF ty_var_guar.

TYPES: BEGIN OF ty_block,
         qtdfixada   TYPE zsdt0059-formula,
         bezei       TYPE zsdt0059-bezei,
         posnr1      TYPE zsdt0059-posnr1,
         valdt       TYPE zsdt0059-valdt,
         safra       TYPE zsdt0059-safra,
         valdt_hedge TYPE zsdt0059-valdt_hedge,
         nivel       TYPE zsdt0059-nivel,
         cod_fp      TYPE zsdt0059-cod_fp,
         field       TYPE zsdt0059-field,
         tipo_calc   TYPE zsdt0059-tipo_calc,
         posnr       TYPE zsdt0059-posnr,
         item_key    TYPE sy-tabix,
       END OF ty_block.

TYPES: BEGIN OF ty_change_ruc_item,
         nro_sol_ov       TYPE zsdt0053-nro_sol_ov,
         posnr            TYPE zsdt0053-posnr,
         numero_ruc_old   TYPE zsdt0053-numero_ruc,
         numero_ruc       TYPE zsdt0053-numero_ruc,
         id_nomeacao_tran TYPE zsdt0053-id_nomeacao_tran,
         change_item      TYPE c,
       END OF ty_change_ruc_item.

" 21.03.2025 - RAMON - 166561 -->
CONSTANTS gc_chicago_f TYPE char20 VALUE 'CHICAGO FRAME'.
CONSTANTS gc_premio_f TYPE char20 VALUE 'PREMIO FRAME'.
CONSTANTS gc_taxa_f TYPE char20 VALUE 'TAXA CAMBIO FRAME'.
CONSTANTS gc_ny_f TYPE char20 VALUE 'NYFRAME'.
CONSTANTS gc_bmef_f TYPE char20 VALUE 'BM&F FRAME'.
" 21.03.2025 - RAMON - 166561 --<

DATA: lt_saldo      TYPE TABLE OF ty_saldo_frame,
      lw_saldo      TYPE ty_saldo_frame,
      var_initial   TYPE c,
      it_var_guar   TYPE TABLE OF ty_var_guar,
      wa_var_guar   TYPE ty_var_guar,
      it_block      TYPE TABLE OF ty_block,
      wa_block      TYPE ty_block,
      error_in_data TYPE c,
      pular_lgort   TYPE c,
      erro_59(30),
      erro_ins(1),
      t_rows        TYPE lvc_t_row,
      w_rows        TYPE lvc_s_row.

TYPES:
  BEGIN OF ty_editor,
    line(255),
  END OF ty_editor.

DATA gt_col_name TYPE TABLE OF ty_col_name.
DATA gv_ultimo_nvl TYPE c LENGTH 4.

DATA: obj_custom_txt    TYPE REF TO cl_gui_custom_container,
      obj_custom_editor TYPE REF TO cl_gui_textedit,
      gt_editor         TYPE TABLE OF ty_editor,
      gt_email          TYPE TABLE OF ty_editor,
      gs_editor         TYPE ty_editor,
      instrucao         TYPE zsded030,
      matnr             TYPE zsded030,
      flag              TYPE char1,
      instrucao_tab     TYPE zsded030.

DATA lv_redist TYPE c VALUE space.

" 13.03.2025 - RAMON - 166561 ->
DATA gv_redist TYPE c.
" 13.03.2025 - RAMON - 166561 -<

" 23.09.2024 - 147331 - RAMON -->
DATA gv_redist_fix.
DATA gv_sub_total_fix.
DATA wl_setleaf TYPE setleaf.
" 20.09.2024 - 147331 - RAMON --<

DATA gv_lib_lote.

*Class definition for ALV toolbar
CLASS: lcl_alv_toolbar    DEFINITION DEFERRED.
*CLASS cl_event_receiver       DEFINITION DEFERRED.
*CLASS cl_base_event_receiver  DEFINITION DEFERRED.
*CLASS:      lcl_alv_toolbar2   DEFINITION DEFERRED.
*CLASS:      lcl_alv_toolbar3   DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Macros                                                             &*
*&--------------------------------------------------------------------&*
DEFINE f_preencher_dynpro.
  CLEAR: wg_bdc.
  MOVE &1 TO wg_bdc-dynbegin.
  IF &1 = 'X'.
    MOVE:
     &2  TO wg_bdc-program,
     &3  TO wg_bdc-dynpro.
  ELSE.
    MOVE:
      &2 TO wg_bdc-fnam,
      &3 TO wg_bdc-fval.
  ENDIF.
  APPEND wg_bdc TO tg_bdc.
END-OF-DEFINITION.
DEFINE enter.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '/00'
    EXCEPTIONS
      function_not_supported = 1.

END-OF-DEFINITION.
DEFINE acao.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = &1
    EXCEPTIONS
      function_not_supported = 1.

END-OF-DEFINITION.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ITENS_NOTA',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container1           TYPE REF TO cl_gui_custom_container,
      container2           TYPE REF TO cl_gui_custom_container,
      container3           TYPE REF TO cl_gui_custom_container,
      container4           TYPE REF TO cl_gui_custom_container,
      container5           TYPE REF TO cl_gui_custom_container,
      container6           TYPE REF TO cl_gui_custom_container,
      container7           TYPE REF TO cl_gui_custom_container,
      container8           TYPE REF TO cl_gui_custom_container,
      container9           TYPE REF TO cl_gui_custom_container,
      container11          TYPE REF TO cl_gui_custom_container,
      container_s1         TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_s2         TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      grid4                TYPE REF TO cl_gui_alv_grid,
      grid5                TYPE REF TO cl_gui_alv_grid,
      grid6                TYPE REF TO cl_gui_alv_grid,
      grid7                TYPE REF TO cl_gui_alv_grid,
      grid8                TYPE REF TO cl_gui_alv_grid,
      grid9                TYPE REF TO cl_gui_alv_grid,
      grid10               TYPE REF TO cl_gui_alv_grid,
      grid11               TYPE REF TO cl_gui_alv_grid,
      tree1                TYPE REF TO cl_gui_alv_tree,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      wg_node_key          TYPE lvc_nkey,
      obg_dialogbox        TYPE REF TO cl_gui_dialogbox_container,
*      go_event_receiver      TYPE REF TO cl_event_receiver,
*      go_base_event_receiver TYPE REF TO cl_base_event_receiver,
**      obg_toolbar2          TYPE REF TO lcl_alv_toolbar,
**      obg_toolbar3          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: it_fcat1   TYPE lvc_t_fcat,
      wa_fcat1   TYPE lvc_s_fcat,
      wa_layout1 TYPE lvc_s_layo,
      wa_alv1    TYPE REF TO cl_gui_alv_grid,
      wa_cont1   TYPE REF TO cl_gui_custom_container,
      wa_exclui  TYPE ui_func,
      it_exclui  TYPE ui_functions.

DATA: wa_emite_entrada TYPE ty_form_lote,
      index_entrada    TYPE   lvc_index.

*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.
DATA: tab_strip_tab6 TYPE char30.   "*-CS2022000332-#78223-02.06.2022-JT-inicio

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x                TYPE c VALUE 'X',
           c_c                TYPE c VALUE 'C',
           c_f                TYPE c VALUE 'F',
           c_a                TYPE c VALUE 'A',
*-CS2021000615 - 17.06.2021 - JT - inicio
           c_ax               TYPE c VALUE 'X',
*-CS2021000615 - 17.06.2021 - JT - fim
           c_b                TYPE c VALUE 'B',
           c_e                TYPE c VALUE 'E',
           c_i                TYPE c VALUE 'I',
           c_p                TYPE c VALUE 'P',
           c_m                TYPE c VALUE 'M',
           c_l                TYPE c VALUE 'L',
           c_d                TYPE c VALUE 'D',
           c_r                TYPE c VALUE 'R',
           c_n                TYPE c VALUE 'N',
           c_y                TYPE c VALUE 'Y',
           c_w                TYPE c VALUE 'W',
           c_u                TYPE c VALUE 'U',
           c_v                TYPE c VALUE 'V',
           c_z                TYPE c VALUE 'Z',
           c_add(3)           TYPE c VALUE 'ADD',
           c_del(3)           TYPE c VALUE 'DEL',
           c_obs(3)           TYPE c VALUE 'OBS',
           c_dele(4)          TYPE c VALUE 'DELE',
           c_exit(4)          TYPE c VALUE 'EXIT',
           c_0051(4)          TYPE c VALUE '0051',
           c_0052(4)          TYPE c VALUE '0052',
           c_back(4)          TYPE c VALUE 'BACK',
           c_copy(4)          TYPE c VALUE 'COPY',
           c_save(4)          TYPE c VALUE 'SAVE',
           c_bloq(4)          TYPE c VALUE 'BLOQ',
           c_vkbur(5)         TYPE c VALUE 'VKBUR',
           c_atual(5)         TYPE c VALUE 'ATUAL',
           c_atual_itens(11)  TYPE c VALUE 'ATUAL_ITENS',
           c_usd(5)           TYPE p DECIMALS 4 VALUE'22.046',
*           c_print(5)        type c value 'PRINT',
           c_modif(5)         TYPE c VALUE 'MODIF',
           c_modif_c_ov(10)   TYPE c VALUE 'MODIF_C_OV',
           c_chg_itm(7)       TYPE c VALUE 'CHG_ITM',
           c_chg_ruc(7)       TYPE c VALUE 'CHG_RUC',
           c_chg_doc(7)       TYPE c VALUE 'CHG_DOC',
           c_inf_ruc(7)       TYPE c VALUE 'INF_RUC',
           c_clear_ruc(9)     TYPE c VALUE 'CLEAR_RUC',
           c_chg_dtv(7)       TYPE c VALUE 'CHG_DTV',
           c_trace_cotton(12) TYPE c VALUE 'TRACE_COTTON',  "BUG 120423-10.08.2023-JT-inicio
           c_saldo(5)         TYPE c VALUE 'SALDO',
           c_reprov(6)        TYPE c VALUE 'REPROV',
           c_redist(6)        TYPE c VALUE 'REDIST',
           c_search(6)        TYPE c VALUE 'SEARCH',
           c_enviar(6)        TYPE c VALUE 'ENVIAR',
           c_logist(6)        TYPE c VALUE 'LOGIST',
           c_cancel(6)        TYPE c VALUE 'CANCEL',
           c_boleto(6)        TYPE c VALUE 'BOLETO',
           c_cond_pg(7)       TYPE c VALUE 'COND_PG',
           c_liberar(7)       TYPE c VALUE 'LIBERAR',
           c_frete_entrada(9) TYPE c VALUE 'LIBFREENT',
           c_estrat(6)        TYPE c VALUE 'ESTRAT',
           c_gerar(7)         TYPE c VALUE 'GERAR',
           c_col_exp(7)       TYPE c VALUE 'COL_EXP',
           c_parcelar(8)      TYPE c VALUE 'PARCELAR',
           c_estornar(8)      TYPE c VALUE 'ESTORNAR',
           c_dp_click(8)      TYPE c VALUE 'DP_CLICK',
           c_modif_qtd(9)     TYPE c VALUE 'MODIF_QTD',
           c_clos_grid(9)     TYPE c VALUE 'C_CLOS_GRID',
           c_sol_aprov(9)     TYPE c VALUE 'SOL_APROV',
           c_pf_adiant(9)     TYPE c VALUE 'PF_ADIANT',
           c_pf_totmov(9)     TYPE c VALUE 'PF_TOTMOV',
           c_show_hist(9)     TYPE c VALUE 'SHOW_HIST',
           c_atual_prec(10)   TYPE c VALUE 'ATUAL_PREC',
           c_pf_avencer(10)   TYPE c VALUE 'PF_AVENCER',
           c_show_msgre(10)   TYPE c VALUE 'SHOW_MSGRE',
           c_pf_vencidas(11)  TYPE c VALUE 'PF_VENCIDAS'.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
*** Declaracoes referente a logica do programa ***
DATA: wg_header               TYPE zsdt0051,
      wg_header_old           TYPE zsdt0051,
      tg_itens                TYPE TABLE OF ty_itens      WITH HEADER LINE,
      w_click                 TYPE ty_itens,
      tg_itens_old            TYPE TABLE OF ty_itens      WITH HEADER LINE,
      tg_itens_mail           TYPE TABLE OF ty_itens      WITH HEADER LINE,
      tg_preco_n              TYPE TABLE OF ty_preco_n    WITH HEADER LINE,
      wg_cond_pgt             TYPE zsdt0052,
      wg_cond_pgt_old         TYPE zsdt0052,
      wg_posicao              TYPE ty_posicao,
      tg_pgt_ant              TYPE TABLE OF ty_pgt_ant    WITH HEADER LINE,
      tg_pgt_ant_old          TYPE TABLE OF ty_pgt_ant    WITH HEADER LINE,
      tg_logistica            TYPE TABLE OF ty_logistica  WITH HEADER LINE,
      tg_logistica_old        TYPE TABLE OF ty_logistica  WITH HEADER LINE,
      tg_preco                TYPE TABLE OF ty_preco      WITH HEADER LINE,
      tg_preco_frame          TYPE TABLE OF ty_preco      WITH HEADER LINE,
      tg_bsad                 TYPE TABLE OF ty_bsad       WITH HEADER LINE,
*      TG_BSAk                  TYPE TABLE OF TY_BSAD WITH HEADER LINE,
      tg_historico            TYPE TABLE OF zsdt0061      WITH HEADER LINE,
      tg_saldo                TYPE TABLE OF ty_saldo      WITH HEADER LINE,
      tg_pedido               TYPE TABLE OF ty_pedido     WITH HEADER LINE,
      tg_adto_ext             TYPE TABLE OF ty_adto_ext   WITH HEADER LINE,
      tg_adto_ext_old         TYPE TABLE OF ty_adto_ext   WITH HEADER LINE,
      wl_adto_ext             TYPE ty_adto_ext,
*      TG_INSTRUCAO            TYPE TABLE OF TY_INSTRUCAO  WITH HEADER LINE,
*      TG_INSTRUCAO_OLD        TYPE TABLE OF TY_INSTRUCAO  WITH HEADER LINE,
      tg_instrucao            TYPE TABLE OF zeinstrucao  WITH HEADER LINE,
      tg_instrucao_old        TYPE TABLE OF zeinstrucao  WITH HEADER LINE,
      tl_0045                 TYPE TABLE OF ty_0045_ WITH HEADER LINE,
      it_002                  TYPE TABLE OF ty_002 WITH HEADER LINE,
      it_zppt0040             TYPE TABLE OF zppt0040,
      it_004                  TYPE TABLE OF ty_004 WITH HEADER LINE,
      it_zsdt0045             TYPE TABLE OF zsdt0045 WITH HEADER LINE,

      tg_ins_frete            TYPE TABLE OF zsdt0045,
      wg_ins_frete            TYPE zsdt0045,
      tg_ins_frete_aux        TYPE TABLE OF zsdt0045,
      wg_ins_frete_aux        TYPE zsdt0045,
      tg_form_lote            TYPE TABLE OF ty_form_lote  WITH HEADER LINE,
      tg_form_lote_old        TYPE TABLE OF ty_form_lote  WITH HEADER LINE,
      tg_cond_esp             TYPE TABLE OF ty_cond_esp   WITH HEADER LINE,
      tg_cond_esp_old         TYPE TABLE OF ty_cond_esp   WITH HEADER LINE,
      tg_0059                 TYPE TABLE OF zsdt0059      WITH HEADER LINE,
      tg_0059_old             TYPE TABLE OF zsdt0059      WITH HEADER LINE,
      tg_fix_red              TYPE TABLE OF ty_fix_red    WITH HEADER LINE,
      tg_fields               TYPE TABLE OF zsdt0072      WITH HEADER LINE,  """"
      tg_motivo               TYPE TABLE OF zsdt0069      WITH HEADER LINE,
      tg_estrat               TYPE TABLE OF zsds019       WITH HEADER LINE,
      tg_log                  TYPE TABLE OF zsdt0083      WITH HEADER LINE,
      tg_bdc                  TYPE TABLE OF bdcdata       WITH HEADER LINE,
      tg_0162                 TYPE TABLE OF zsdt0162      WITH HEADER LINE,
      wg_bdc                  TYPE bdcdata,
      wa_style                TYPE lvc_s_styl,
      style                   TYPE lvc_t_styl WITH HEADER LINE,
      tg_texto                TYPE catsxt_longtext_itab,
      tg_vbpa                 TYPE TABLE OF vbpa,
      wa_vbpa                 TYPE vbpa,
      wa_zparametros          TYPE zparametros,
      wg_texto                TYPE LINE OF catsxt_longtext_itab,
      pb_obs(30)              ,
      pb_coment_logistica(30) ,
      wg_colaps(4)            VALUE '@K2@',
      wg_sub01                TYPE sy-dynnr,
      wg_desc_status(20),
      wg_desc_kunnr(47),
      wg_desc_correto(47),
      wg_desc_auart(20),
      wg_desc_vkorg(20),
      wg_desc_vtweg(20),
      wg_desc_spart(27),
      wg_desc_vkgrp(27),
      wg_desc_vkbur(33),
      wg_desc_zlsch(50),
      v_valido                TYPE c,
      wg_desc_zterm(23),
      wg_desc_hbkid(50),
      wg_desc_tp_venda(30),
      wg_desc_matnr(40),
      wg_status(4),
      wg_cond_esp(4)          VALUE icon_enter_more, "'@6Y@',
      wg_data                 TYPE sy-datum,
      wg_hora                 TYPE sy-uzeit,
      rb_email                VALUE 'X',
      rb_imprimir,
      p_email(100),
      c_eletronica(1),
      wg_redistribuir,
      wg_redistribuir_saldo   TYPE zsdt0053-zmeng,
      wg_redistribuir_total   TYPE zsdt0053-zmeng,
      wg_redistribuir_titem   TYPE zsdt0053-zmeng,
      wg_valor(30),
      wg_valor_aux(30),
      wg_valor_aux2(30),
      wg_verifica_sacado      TYPE c,
      wg_verifica_incoterm    TYPE c LENGTH 3,
      wg_zsdt0075             TYPE zsdt0075,
      it_zsdt0100             TYPE TABLE OF zsdt0100,
      wa_zsdt0100             TYPE zsdt0100,
      tp_venda_error          TYPE c LENGTH 1,
      it_screen               TYPE TABLE OF ty_screen,
      wa_screen               TYPE ty_screen,
      cont                    TYPE sy-tabix,
      block_botao             TYPE c,
      convert_texto           TYPE sytitle,
      tl_itens_frete          TYPE TABLE OF ty_itens_frete,
      wl_itens_frete          TYPE ty_itens_frete,
      gwa_change_ruc_item     TYPE ty_change_ruc_item,
      git_parametros_user     TYPE ustyp_t_parameters,
      posnr                   TYPE posnr_va.

TYPES: BEGIN OF ty_0213.
         INCLUDE TYPE zsdt0213.
TYPES:   flag  TYPE char1,
         style TYPE lvc_t_styl,
       END OF ty_0213.

DATA it_0213 TYPE TABLE OF ty_0213.
DATA it_0235 TYPE TABLE OF zsdt0235.
DATA t_0213 TYPE TABLE OF ty_0213.
DATA it_0213_aux TYPE TABLE OF zsdt0213.
DATA wa_0213_aux TYPE zsdt0213.
DATA t_0213_orig TYPE TABLE OF ty_0213. "*-CS2023000189-#126959-07.11.2023-JT-inicio

DATA it_0213_aux_old TYPE TABLE OF zsdt0213.
DATA wa_0213_aux_old TYPE zsdt0213.

DATA: contador TYPE sy-tabix.

DATA gv_0235_db TYPE flag. "#PERFORMANCE
DATA gs_0101 TYPE zsdt0101. "#PERFORMANCE

FIELD-SYMBOLS <itens> TYPE ty_itens.

*      Dados para Controle de T
DATA: var_len1     TYPE i,
      gt_zsdt0056  TYPE TABLE OF zsdt0056,
      gw_zsdt0056  TYPE zsdt0056,

      r_bezei      TYPE RANGE OF zsdt0056-bezei,
      c_bezei      TYPE RANGE OF zsdt0056-bezei,
      p_bezei      TYPE RANGE OF zsdt0056-bezei,
      n_bezei      TYPE RANGE OF zsdt0056-bezei,
      b_bezei      TYPE RANGE OF zsdt0056-bezei, " 29.10.2024 - 147331 - RAMON

      r_bezei_line LIKE LINE OF r_bezei,
      c_bezei_line LIKE LINE OF c_bezei,
      p_bezei_line LIKE LINE OF p_bezei,
      n_bezei_line LIKE LINE OF n_bezei,
      b_bezei_line LIKE LINE OF n_bezei,

      ant_var      TYPE c VALUE '',
      l_itens      TYPE sy-tabix,

      g_monat_c(2),
      g_monat_p(2) TYPE n,
      g_monat_t(2) TYPE n,
      g_monat_n(2) TYPE n,
      g_monat_b(2) TYPE n.


*** Declaracoes referente ao template do programa ***
DATA: ok-code         TYPE sy-ucomm,
      wg_display,
      wg_acao(10),
      wg_prec(10),
      wg_flag,
      init,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      wg_st_change    TYPE c LENGTH 1,
      wg_zmeng        TYPE vbap-kwmeng,
      wg_dmbtr        TYPE vbap-netpr,
      wg_charg        TYPE vbap-charg,
      wg_matnr        TYPE vbap-matnr.

****Exception
DATA: obj_exc  TYPE REF TO cx_sy_conversion_no_number,
      exc_text TYPE string.

***** Funcao de Z_DOC_CHECK_NEW
DATA: x_field(30),
      wg_mensagem(30).
DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      it_msg_ret TYPE zfitrs0002,
      wg_cell    TYPE lvc_s_cell,
      tg_cell    TYPE lvc_t_cell.

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      t_fieldcat_edt TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      t_new_table    TYPE REF TO data,
      t_new_line     TYPE REF TO data,
      t_new_line2    TYPE REF TO data,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl VALUE 'XX'.

FIELD-SYMBOLS: <fs_table>       TYPE STANDARD TABLE,
               <fs_table_frame> TYPE STANDARD TABLE,
               <fs_line>        TYPE any,
               <fs_line_var>    TYPE any,
               <fs_line_aux>    TYPE any,
               <fs_campo>       TYPE any,
               <fs_campo2>      TYPE any,
               <fs_campo3>      TYPE any,
               <fcat>           TYPE lvc_s_fcat.

DATA: it_0059_aux TYPE TABLE OF zsdt0059,
      wa_0059_aux TYPE zsdt0059.

DATA: seq_alv TYPE i.

DATA: txt_warning TYPE c LENGTH 200,
      txt_kunnr   TYPE name1,
      txt_bukrs   TYPE butxt.

TYPES: ty_bezei_tab TYPE RANGE OF zsdt0056-bezei.


*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_FC3',
             tab4 LIKE sy-ucomm VALUE 'TAB_STRIP_FC4',
             tab5 LIKE sy-ucomm VALUE 'TAB_STRIP_FC5',
             tab6 LIKE sy-ucomm VALUE 'TAB_STRIP_FC6',
             tab7 LIKE sy-ucomm VALUE 'TAB_STRIP_FC7',
             tab8 LIKE sy-ucomm VALUE 'TAB_STRIP_FC8',
             tab9 LIKE sy-ucomm VALUE 'TAB_STRIP_FC9',
           END OF c_tab_strip.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP'
CONTROLS:  tab_strip TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0022',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip-tab1,
      END OF g_tab_strip.
DATA:      ok_code LIKE sy-ucomm.
DATA: var_edit     TYPE c,
      modred_hedge TYPE c LENGTH 6,
      var_modred   TYPE c LENGTH 6.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid VALUE sy-repid,
      tg_sort      TYPE lvc_t_sort,
      wa_sort      LIKE LINE OF tg_sort,
      it_dta       TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE.

*DATA: BEGIN OF IT_MSG OCCURS 0.
*        INCLUDE STRUCTURE BDCMSGCOLL.
*DATA: END OF IT_MSG.

DATA: it_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_msg TYPE bdcmsgcoll,

      BEGIN OF it_msgtext OCCURS 0,
        texto TYPE t100-text,
      END OF it_msgtext.

DATA: obj_frete TYPE REF TO zcl_solicitacao_ov.

*      it_msgtext TYPE TABLE OF T100 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
*CALL SCREEN 100.

" Parâmetro 'PSOLICI' utilizado pelo report ZSDR0076 para passar o número da solicitação.
" Não alterar
PARAMETER: psolici TYPE zsded013 NO-DISPLAY.

INITIALIZATION.

START-OF-SELECTION.

  IF psolici IS NOT INITIAL.
    wg_header-nro_sol_ov = psolici.
  ENDIF.

  CALL SCREEN 050.

  DEFINE enter.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '/00'
      EXCEPTIONS
        function_not_supported = 1.

  END-OF-DEFINITION.
  DEFINE acao.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = &1
      EXCEPTIONS
        function_not_supported = 1.

  END-OF-DEFINITION.

*----------------------------------------------------------------------*
*       CLASS LCL_BUSCA_TELA DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_busca_tela DEFINITION.
  PUBLIC SECTION.
    METHODS: busca_tela.
    CLASS-METHODS exit_tela IMPORTING ucomm        TYPE sy-ucomm
                            RETURNING VALUE(check) TYPE char1.

ENDCLASS.                    "LCL_BUSCA_TELA DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_BUSCA_TELA IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_busca_tela IMPLEMENTATION.
*  Method para buscar as telas ñ tem funcionalidade na 62 pode passar sem analizar senhor ABAP.
  METHOD busca_tela.
    CLEAR wa_screen.

    LOOP AT SCREEN.
      cont = cont + 1.
      wa_screen-id      = cont.
      wa_screen-n_campo = screen-name.
      wa_screen-tela    = sy-dynnr.

      APPEND wa_screen TO it_screen.
    ENDLOOP.

  ENDMETHOD.                    "BUSCA_TELA

  METHOD exit_tela.

    CASE ucomm.
      WHEN 'ESTRAT'.
        check = abap_true.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.                    "LCL_BUSCA_TELA IMPLEMENTATION

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS: handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_hotspot_100 FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      on_hotspot_click_form FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.
*
    CLASS-METHODS:
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_adto_ext FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_adto FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_log FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_log FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_preco FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_preco FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_pgto FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_pgto FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_ins FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_ins FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_pedido FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_form FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished_form FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_data_changed_finished_ped FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

    CLASS-METHODS:
      changed_cond_esp FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      changed_finished_cond_esp FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_close FOR EVENT close OF cl_gui_dialogbox_container
        IMPORTING sender.

  PRIVATE SECTION.
    METHODS:
      check_lifnr IMPORTING ps_good_lifnr TYPE lvc_s_modi pr_data_changed TYPE REF TO cl_alv_changed_data_protocol,
      check_lgort IMPORTING ps_good_lgort TYPE lvc_s_modi pr_data_changed TYPE REF TO cl_alv_changed_data_protocol,
      calculo_desconto IMPORTING linha TYPE int4,
      get_cell IMPORTING er_data        TYPE REF TO cl_alv_changed_data_protocol
                         i_good         TYPE lvc_s_modi
                         i_field        TYPE lvc_fname
               RETURNING VALUE(e_value) TYPE lvc_value.
*    CLASS-METHODS:
*      on_onf4_ins FOR EVENT onf4 OF cl_gui_alv_grid
*                     IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
*                               et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
**----------------------------------------------------------------------*
**       CLASS cl_event_receiver DEFINITION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS cl_event_receiver DEFINITION INHERITING FROM cl_gui_alv_grid_base.
*
*  PUBLIC SECTION.
*    METHODS handle_left_click_run               " LEFT_CLICK_RUN
*        FOR EVENT  left_click_design OF cl_gui_alv_grid. " cl_gui_alv_tree.
*
*ENDCLASS.                    "cl_event_receiver DEFINITION
**----------------------------------------------------------------------*
**       CLASS cl_event_receiver IMPLEMENTATION
**----------------------------------------------------------------------*
*CLASS cl_event_receiver IMPLEMENTATION.
*
*  METHOD handle_left_click_run.
*    .
*  ENDMETHOD.                    "handle_right_click
*ENDCLASS.                    "cl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS tree_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
        IMPORTING
          node_key,
      handle_expand_no_children FOR EVENT expand_nc OF cl_gui_alv_tree
        IMPORTING node_key,
      handle_item_double_click FOR EVENT item_double_click OF cl_gui_alv_tree
        IMPORTING fieldname node_key.

*    METHODS handle_left_click_run               " LEFT_CLICK_RUN
*     FOR EVENT left_click_run OF cl_gui_alv_grid.
  PRIVATE SECTION.
ENDCLASS.                    "tree_event_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar_pgt_ant FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_0301 FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_0301 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      on_data_ch_0301 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,

      on_data_ch_f_0301 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells,

      on_f4_0301 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display,

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_form FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_itens FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_logistica FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_p_fram FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      on_toolbar_ins FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command_itens FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_pgt_ant FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_adto_ext FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_hotspot_click_adto_ext FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_user_command_logistica FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_ins FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_form FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_pedido FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_p_fram FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor
  METHOD on_toolbar_itens.
    DATA: wl_desactive,
          wl_itens     LIKE LINE OF tg_itens,
          wl_lines     TYPE sy-tabix,
          wl_texto     LIKE ty_toolbar-text.

    PERFORM fm_get_par_user.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif
    OR wg_acao EQ c_modif_c_ov.
      ty_toolbar-disabled  = space.
      LOOP AT tg_itens INTO wl_itens
       WHERE item_edit EQ c_x.

      ENDLOOP.
      IF sy-subrc IS INITIAL
      AND wg_header-param_espec NE c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
      AND wg_header-param_espec NE c_ax
*-CS2021000615 - 17.06.2021 - JT - fim
      AND wg_header-param_espec NE c_p
      AND wg_header-param_espec NE c_m
      AND  wg_header-param_espec NE c_z .  " CS2020000373 23/08/2022 -LP

        MOVE 1 TO ty_toolbar-disabled.
*      MODIFY E_OBJECT->MT_TOOLBAR FROM TY_TOOLBAR.
      ENDIF.
    ELSE.
      IF wg_redistribuir EQ c_x
     AND wg_acao         EQ c_modif_qtd.
        ty_toolbar-disabled  = space.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.
    ENDIF.
    ty_toolbar-butn_type = 0.

    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      IF ( wg_redistribuir EQ c_x
     AND wg_acao         EQ c_modif_qtd )
       OR wg_acao EQ c_modif_c_ov.
        ty_toolbar-disabled  = space.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF wg_cond_pgt-qte_venc GT 1.
      ty_toolbar-icon      =  icon_move.
      ty_toolbar-function  =  c_parcelar.
      IF wg_acao EQ c_add
      OR wg_acao EQ c_modif.
        ty_toolbar-disabled  = space.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.
      ty_toolbar-butn_type = 0.
      DESCRIBE TABLE tg_itens LINES wl_lines.
      IF wl_lines GT wg_cond_pgt-qte_venc.
        ty_toolbar-disabled  = 1.
      ENDIF.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z .
      ty_toolbar-icon      =  icon_active_inactive.
      ty_toolbar-function  =  c_chg_itm.
      ty_toolbar-butn_type = 0.
      IF wg_acao EQ c_add
      OR wg_acao EQ c_modif
      OR wg_acao EQ c_modif_c_ov.
        ty_toolbar-disabled  = space.
        APPEND ty_toolbar TO e_object->mt_toolbar.
      ELSE.
*        Botão C_CHG_ITM não pode ser Visivel na ação Redistribuição
*        IF WG_REDISTRIBUIR EQ C_X
*       AND WG_ACAO         EQ C_MODIF_QTD.
*          TY_TOOLBAR-DISABLED  = SPACE.
*          APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
**        ELSE.
**          TY_TOOLBAR-DISABLED  = 1.
*        ENDIF.
      ENDIF.

      CLEAR ty_toolbar.
    ENDIF.

    IF wg_header-param_espec EQ c_a OR
       wg_header-param_espec EQ c_ax "*-CS2021000615 - 17.06.2021 - JT - inicio
       OR wg_header-param_espec EQ c_z. " CS2020000373 23/08/2022 -LP
      READ TABLE git_parametros_user INTO DATA(lwa_par_user) WITH KEY parid = 'ZSDT0062_ADM' .
      IF sy-subrc EQ 0.
        ty_toolbar-icon      =  icon_active_inactive.
        ty_toolbar-text      = 'Alterar RUC'.
        ty_toolbar-function  =  c_chg_ruc.
        ty_toolbar-butn_type = 0.
        APPEND ty_toolbar TO e_object->mt_toolbar.
        CLEAR ty_toolbar.
      ENDIF.
    ENDIF.

    IF wg_header-param_espec EQ c_p.
      ty_toolbar-icon      =  icon_fast_entry.
      ty_toolbar-text      = 'Informar RUC'.
      ty_toolbar-function  =  c_inf_ruc.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_erase.
      ty_toolbar-text      = 'Remover RUC Item'.
      ty_toolbar-function  =  c_clear_ruc.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE git_parametros_user INTO lwa_par_user WITH KEY parid = 'ZSDT0062_ADM' .
    IF sy-subrc IS INITIAL.
      ty_toolbar-icon      =  icon_system_undo.
      ty_toolbar-text      = 'Limpar Doc. Exp'.
      ty_toolbar-function  =  c_chg_doc.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    LOOP AT tg_itens TRANSPORTING NO FIELDS
       WHERE vbeln IS NOT INITIAL.

    ENDLOOP.

    IF sy-subrc IS INITIAL.
      ty_toolbar-icon      =  icon_import_all_requests.
      ty_toolbar-function  =  c_saldo.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

*-CS2023000189-#126959-07.11.2023-JT-inicio
      ty_toolbar-icon      = icon_transfer.
      ty_toolbar-function  = c_trace_cotton.
      ty_toolbar-text      = 'Reenviar Trace Cotton'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
*-CS2023000189-#126959-07.11.2023-JT-fim
    ENDIF.

*    ty_toolbar-butn_type = 3.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.

    " 06.02.2024 - RAMON - 133197 -->
*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.
    " 06.02.2024 - RAMON - 133197 --<

    READ TABLE e_object->mt_toolbar WITH KEY function = '&DETAIL' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_select_detail.
      ty_toolbar-function  =  '&DETAIL'.
      ty_toolbar-quickinfo = 'Detalhes'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = '&SORT_ASC' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_sort_up.
      ty_toolbar-function  =  '&SORT_ASC'.
      ty_toolbar-quickinfo = 'Ordenação crescente'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = '&SORT_DSC' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_sort_down.
      ty_toolbar-function  =  '&SORT_DSC'.
      ty_toolbar-quickinfo = 'Ordenação decrescente'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = '&FIND' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_search.
      ty_toolbar-function  =  '&FIND'.
      ty_toolbar-quickinfo = 'Procur...'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = '&MB_SUM' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_sum.
      ty_toolbar-function  =  '&MB_SUM'.
      ty_toolbar-quickinfo = 'Total'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = '&MB_SUBTOT' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_intermediate_sum.
      ty_toolbar-function  =  '&MB_SUBTOT'.
      ty_toolbar-quickinfo = 'Subtotais'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    READ TABLE e_object->mt_toolbar WITH KEY function = '&PRINT_BACK' TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ty_toolbar-icon      =  icon_print.
      ty_toolbar-function  =  '&PRINT_BACK'.
      ty_toolbar-quickinfo = 'Imprim.'.
      ty_toolbar-disabled  = space.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.


    IF wg_redistribuir IS NOT INITIAL.
      PERFORM busca_saldo.
      PERFORM calcula_redistribuicao.
      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      IF wg_redistribuir_saldo IS INITIAL.
        ty_toolbar-icon = '@5B@'.
        CLEAR: ty_toolbar-text.
      ELSE.
        ty_toolbar-icon = '@5C@'.
        WRITE wg_redistribuir_saldo TO wl_texto.
        CONDENSE wl_texto NO-GAPS.
        ty_toolbar-text = wl_texto.
      ENDIF.
      ty_toolbar-quickinfo = TEXT-b01."'Saldo a redistribuir'.
      ty_toolbar-disabled = space.

      ty_toolbar-butn_type = 4.
      ty_toolbar-checked = 1.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      IF wg_redistribuir_total IS INITIAL.
        ty_toolbar-icon = '@5B@'.
        CLEAR: ty_toolbar-text.
      ELSEIF wg_redistribuir_total LT 0.
        ty_toolbar-icon = '@5C@'.
        WRITE wg_redistribuir_total TO wl_texto.
        CONDENSE wl_texto NO-GAPS.
        ty_toolbar-text = wl_texto.
      ELSE.
        ty_toolbar-icon = '@5D@'.
        WRITE wg_redistribuir_total TO wl_texto.
        CONDENSE wl_texto NO-GAPS.
        ty_toolbar-text = wl_texto.
      ENDIF.
      ty_toolbar-quickinfo = TEXT-b02."'Total a ser redistribuido'.
      ty_toolbar-disabled = space.

      ty_toolbar-butn_type = 0.
      ty_toolbar-checked = 1.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.
  ENDMETHOD.                    "ON_TOOLBAR_itens

  METHOD on_toolbar_logistica.
    DATA: wl_desactive,
          wl_desabitar,
          wl_logistica LIKE LINE OF tg_logistica.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    LOOP AT tg_logistica INTO wl_logistica
       WHERE zmeng IS NOT INITIAL.

    ENDLOOP.
    IF sy-subrc IS INITIAL.

      MOVE 1 TO ty_toolbar-disabled.
*      MODIFY E_OBJECT->MT_TOOLBAR FROM TY_TOOLBAR.
    ENDIF.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*    IF wg_docs-docnum IS INITIAL.
*      READ TABLE tg_fields TRANSPORTING NO FIELDS
*        WITH KEY group1 = 'GR1'.
*      IF sy-subrc IS INITIAL.
*        wl_desactive = space.
*      ELSE.
*        wl_desactive = 1.
*      ENDIF.
*    ELSE.
*      wl_desactive = 1.
*    ENDIF.
    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
*    ty_toolbar-icon      =  icon_view_close.
*    ty_toolbar-function  =  c_clos_msg.
*    ty_toolbar-disabled  = space.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar

    " 06.02.2024 - RAMON - 133197 -->
*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.
    " 06.02.2024 - RAMON - 133197 --<

  ENDMETHOD.                    "ON_TOOLBAR_logistica

  METHOD on_toolbar_0301.

    e_object->mt_toolbar =
    VALUE #(
              ( icon = '@17@' function = c_add        butn_type = 0 disabled = SWITCH #( wg_acao WHEN c_add OR c_modif OR c_modif_c_ov THEN space ELSE 1 ) )
              ( icon = '@18@' function = c_del        butn_type = 0 disabled = SWITCH #( wg_acao WHEN c_add OR c_modif OR c_modif_c_ov THEN space ELSE 1 ) )
              (               function = '&&SEP04'    butn_type = 3 )
              ( icon = '@3E@' function = '&SORT_ASC'  butn_type = 0 )
              ( icon = '@3F@' function = '&SORT_DSC'  butn_type = 0 )
              (               function = '&&SEP04'    butn_type = 3 )
              ( icon = '@13@' function = '&FIND'      butn_type = 0 )
              ( icon = '@4E@' function = '&FIND_MORE' butn_type = 0 )
              (               function = '&&SEP04'    butn_type = 3 )
              ( icon = '@3Z@' function = '&MB_SUM'    butn_type = 1 )
              ( icon = '@5V@' function = '&MB_SUBTOT' butn_type = 1 )
           ).
  ENDMETHOD.

  METHOD handle_user_0301.

    CASE e_ucomm.
      WHEN c_add.
        IF NOT line_exists( t_0213[ lgort = ' ' ] ).
          APPEND VALUE #(
                          nro_sol_ov = wg_header-nro_sol_ov
                          posnr = w_click-posnr
                          flag = 'I'
                          style = VALUE #( ( fieldname = 'VOLUM' style = on )  )
                        ) TO t_0213.
        ENDIF.
      WHEN c_del.

        CALL METHOD grid11->get_selected_cells
          IMPORTING
            et_cell = DATA(t_cell).

        LOOP AT t_cell INTO DATA(_cell).
          DATA(_linha) = t_0213[ _cell-row_id-index ].

          _linha-status = abap_true.

          APPEND CORRESPONDING #( _linha ) TO it_0213_aux.

          DELETE t_0213 WHERE nro_sol_ov EQ _linha-nro_sol_ov
                           AND posnr EQ _linha-posnr
                           AND lgort EQ _linha-lgort.

          DELETE it_0213 WHERE nro_sol_ov EQ _linha-nro_sol_ov
                           AND posnr EQ _linha-posnr
                           AND lgort EQ _linha-lgort.
        ENDLOOP.

    ENDCASE.

    CALL METHOD grid11->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD on_f4_0301.

    TYPES: BEGIN OF y_f4,
             lgort TYPE lgort_d,
           END OF y_f4.

    DATA: t_f4  TYPE TABLE OF y_f4,
          t_ret TYPE TABLE OF ddshretval.

    IF w_click-instrucao_ant IS NOT INITIAL.
      instrucao_tab = w_click-instrucao_ant.

    ELSE.
      instrucao_tab = w_click-instrucao.
    ENDIF.

    LOOP AT tg_instrucao INTO DATA(_inst) WHERE instrucao EQ instrucao_tab AND matnr EQ w_click-matnr.
      IF NOT line_exists( t_0213[ lgort = _inst-charg ] ).
        APPEND VALUE #( lgort = _inst-charg ) TO t_f4.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'LGORT'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        value_org       = 'S'
      TABLES
        value_tab       = t_f4
        return_tab      = t_ret
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    IF sy-subrc IS INITIAL.
      TRY .
          t_0213[ es_row_no-row_id ]-lgort = t_ret[ 1 ]-fieldval.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.
    ENDIF.

    CALL METHOD grid11->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD on_data_ch_0301.

  ENDMETHOD.

  METHOD on_data_ch_f_0301.
*
*    CHECK E_MODIFIED IS INITIAL.
*    CHECK ET_GOOD_CELLS[ 1 ]-FIELDNAME EQ 'VOLUM'.
*
*    IT_0213[ ET_GOOD_CELLS[ 1 ]-ROW_ID ]-FLAG = 'U'.

  ENDMETHOD.

  METHOD on_toolbar_pgt_ant.
    DATA: wl_desactive.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif
    OR wg_acao EQ c_modif_c_ov
    OR wg_acao EQ c_modif_qtd.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


*    IF wg_docs-docnum IS INITIAL.
*      READ TABLE tg_fields TRANSPORTING NO FIELDS
*        WITH KEY group1 = 'GR1'.
*      IF sy-subrc IS INITIAL.
*        wl_desactive = space.
*      ELSE.
*        wl_desactive = 1.
*      ENDIF.
*    ELSE.
*      wl_desactive = 1.
*    ENDIF.
    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif
    OR wg_acao EQ c_modif_c_ov
    OR wg_acao EQ c_modif_qtd.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_storno.
    ty_toolbar-function  =  c_estornar.
    IF wg_acao EQ  c_modif.
      ty_toolbar-disabled  = space.
    ELSEIF wg_acao EQ c_modif_c_ov
    AND ( wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
          wg_header-param_espec EQ c_ax ).
*-CS2021000615 - 17.06.2021 - JT - fim
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
      IF wg_header-param_espec NE c_p.
        ty_toolbar-disabled = 1.
      ENDIF.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
       wg_header-param_espec EQ c_ax OR
        wg_header-param_espec EQ c_z .
*-CS2021000615 - 17.06.2021 - JT - fim
      ty_toolbar-icon      =  icon_generate.
      ty_toolbar-function  =  c_gerar.
      ty_toolbar-text  =  TEXT-b03."'Gerar Adiantamento'.
      IF wg_acao EQ c_add
      OR wg_acao EQ c_modif_c_ov
      OR wg_acao EQ c_modif.
        ty_toolbar-disabled  = space.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    ENDIF.

    " 06.02.2024 - RAMON - 133197 -->
*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.
    " 06.02.2024 - RAMON - 133197 --<
  ENDMETHOD.                    "ON_TOOLBAR_PGT_ANT

  METHOD on_toolbar_p_fram.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_grid.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

  ENDMETHOD.                    "on_toolbar_P_FRAM

  METHOD on_toolbar_ins.


    FREE: ty_toolbar.

    FIELD-SYMBOLS: <ls_toolbar>  TYPE stb_button.

    DEFINE toobar.
      ty_toolbar-icon      = &1.
      ty_toolbar-function  = &2.
      ty_toolbar-quickinfo = &3.
      ty_toolbar-text      = &4.
      ty_toolbar-butn_type = &5.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
    END-OF-DEFINITION.

    toobar:
*    ''     ''      ''              ''              3,
*    '@17@' C_ADD   'Adicionar'     ''              0,
*    ''     ''      ''              ''              3,
*    '@18@' C_DEL   'Deletar'       ''              0,
    ''     ''      ''              ''              3,
    '@K5@' 'FRETE' 'Cotação Frete' 'Cotação Frete' 0,
    ''     ''      ''              ''              3.

    LOOP AT e_object->mt_toolbar ASSIGNING <ls_toolbar>.

*      CASE WG_ACAO.

*        WHEN 'MODIF'.
*          CASE <LS_TOOLBAR>-FUNCTION.
*            WHEN 'FRETE'.
*              <LS_TOOLBAR>-DISABLED = ABAP_TRUE.
*          ENDCASE.
*        WHEN 'ATUAL'.
*          CASE <LS_TOOLBAR>-FUNCTION.
*            WHEN C_DEL OR C_ADD.
*              <LS_TOOLBAR>-DISABLED = ABAP_TRUE.
*          ENDCASE.
*      ENDCASE.

      CASE <ls_toolbar>-function.
        WHEN '&LOCAL&INSERT_ROW' OR '&LOCAL&DELETE_ROW' OR 'FRETE'.
*          DELETE E_OBJECT->MT_TOOLBAR INDEX SY-TABIX.
          <ls_toolbar>-disabled = abap_true.
      ENDCASE.

    ENDLOOP.

*    CALL METHOD C_ALV_TOOLBARMANAGER->REORGANIZE
*      EXPORTING
*        IO_ALV_TOOLBAR = E_OBJECT.


  ENDMETHOD.                    "ON_TOOLBAR_INS

  METHOD on_toolbar.
    DATA: wl_desactive.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    IF wg_acao EQ c_add
    OR wg_acao EQ c_modif.
      ty_toolbar-disabled  = space.
    ELSE.
      ty_toolbar-disabled  = 1.
    ENDIF.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    " 06.02.2024 - RAMON - 133197 -->
*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.
    " 06.02.2024 - RAMON - 133197 --<

  ENDMETHOD.                    "ON_TOOLBAR

  METHOD on_toolbar_form.
*  EVENT HANDLER METHOD FOR EVENT TOOLBAR.

    CONSTANTS:
*  CONSTANTS FOR BUTTON TYPE
      c_button_normal           TYPE i VALUE 0,
      c_menu_and_default_button TYPE i VALUE 1,
      c_menu                    TYPE i VALUE 2,
      c_separator               TYPE i VALUE 3,
      c_radio_button            TYPE i VALUE 4,
      c_checkbox                TYPE i VALUE 5,
      c_menu_entry              TYPE i VALUE 6.

    DATA: ls_toolbar  TYPE stb_button.

*    CLEAR LS_TOOLBAR.
*    MOVE  C_ADD                 TO LS_TOOLBAR-FUNCTION.
*    MOVE  ICON_INSERT_ROW       TO LS_TOOLBAR-ICON.
*    MOVE text-B04               TO LS_TOOLBAR-QUICKINFO.
*    MOVE ' '                    TO LS_TOOLBAR-DISABLED.
*    MODIFY E_OBJECT->MT_TOOLBAR FROM LS_TOOLBAR INDEX 5.
*
*    CLEAR LS_TOOLBAR.
*    MOVE  C_DEL                 TO LS_TOOLBAR-FUNCTION.
*    MOVE  ICON_DELETE_ROW       TO LS_TOOLBAR-ICON.
*    MOVE text-B04               TO LS_TOOLBAR-QUICKINFO.
*    MOVE  ' '                   TO LS_TOOLBAR-DISABLED.
*    MODIFY E_OBJECT->MT_TOOLBAR FROM LS_TOOLBAR INDEX 6.

    CLEAR ls_toolbar.
    MOVE c_separator TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE  c_liberar                 TO ls_toolbar-function.
    MOVE  icon_release              TO ls_toolbar-icon.
    MOVE TEXT-b05                   TO ls_toolbar-quickinfo.
    MOVE TEXT-b05                   TO ls_toolbar-text.
    MOVE ' '                        TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.


*    CLEAR ls_toolbar.
*    MOVE  c_frete_entrada           TO ls_toolbar-function.
*    MOVE  icon_active_inactive      TO ls_toolbar-icon.
*    MOVE text-b08                   TO ls_toolbar-quickinfo.
*    MOVE text-b08                   TO ls_toolbar-text.
*    MOVE ' '                        TO ls_toolbar-disabled.
*    APPEND ls_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<ls>).

*      CASE WG_ACAO.

*        WHEN 'MODIF'.
*          CASE <LS_TOOLBAR>-FUNCTION.
*            WHEN 'FRETE'.
*              <LS_TOOLBAR>-DISABLED = ABAP_TRUE.
*          ENDCASE.
*        WHEN 'ATUAL'.
*          CASE <LS_TOOLBAR>-FUNCTION.
*            WHEN C_DEL OR C_ADD.
*              <LS_TOOLBAR>-DISABLED = ABAP_TRUE.
*          ENDCASE.
*      ENDCASE.

*      IF <ls>-function EQ c_frete_entrada.
*        IF wg_header-tp_venda NE '00042'.
*          <ls>-disabled = abap_true.
*        ENDIF.
*      ENDIF.

      CASE <ls>-function.
        WHEN '&LOCAL&INSERT_ROW' OR '&LOCAL&DELETE_ROW' OR c_liberar.
          <ls>-disabled = abap_true.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.                    "ON_TOOLBAR

  METHOD handle_user_command_itens.
    DATA: tl_itens_aux  LIKE TABLE OF tg_itens,
          wl_itens_aux  LIKE LINE OF tg_itens,
          wl_itens      LIKE LINE OF tg_itens,
          wl_preco      LIKE LINE OF tg_preco,
          wl_logistica  LIKE LINE OF tg_logistica,
          tl_good_cells TYPE lvc_t_modi,
          wl_good_cells TYPE lvc_s_modi,
          wl_lines      TYPE sy-tabix,
          wl_cont       TYPE sy-tabix,
          wl_zmeng      TYPE zsdt0053-zmeng,
          it_sel_rows   TYPE lvc_t_row,
          wa_sel_rows   TYPE lvc_s_row,
          wl_erro.

    DATA: l_matnr  TYPE matnr,
          lr_matnr TYPE RANGE OF matnr.
    DATA: lobj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.
    DATA: cx_exception TYPE REF TO zcx_webservice.
    DATA: var_msg   TYPE string.


    REFRESH: tl_itens_aux.
*   User Command Botões Incluidos

    UNASSIGN <fs_line>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.
    CASE e_ucomm.
      WHEN c_inf_ruc.

        DATA: wa_dados_geracao_ruc TYPE zde_dados_geracao_ruc.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.

          READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<lfs_item_sol>) INDEX wg_selectedcell-row_id-index.
          CHECK ( sy-subrc EQ 0 ) AND ( wg_header-nro_sol_ov IS NOT INITIAL ).

          CLEAR: wa_dados_geracao_ruc.

          IF <lfs_item_sol>-codigo_ra IS INITIAL.
            MESSAGE 'Terminal não foi informado para o item da solicitação!' TYPE 'I'.
            RETURN.
          ENDIF.

          IF <lfs_item_sol>-id_nomeacao_tran IS INITIAL.
            MESSAGE 'Id. Nomeação não foi informado para o item da solicitação!' TYPE 'I'.
            RETURN.
          ENDIF.

          IF <lfs_item_sol>-kunnr IS NOT INITIAL.
            wa_dados_geracao_ruc-kunnr_exp = <lfs_item_sol>-kunnr.
          ELSEIF wg_header-kunnr IS NOT INITIAL.
            wa_dados_geracao_ruc-kunnr_exp = wg_header-kunnr.
          ENDIF.

          IF wa_dados_geracao_ruc-kunnr_exp IS INITIAL.
            MESSAGE 'Cliente não foi informado para a solicitação/item!' TYPE 'I'.
            RETURN.
          ENDIF.

          IF <lfs_item_sol>-matnr IS INITIAL.
            MESSAGE 'Material não foi informado para o item da solicitação!' TYPE 'I'.
            RETURN.
          ENDIF.

          IF <lfs_item_sol>-numero_ruc IS NOT INITIAL.
            SELECT SINGLE *
              FROM zsdt0170 INTO @DATA(lwa_zsdt0170)
             WHERE numero_ruc EQ @<lfs_item_sol>-numero_ruc
               AND tp_due     EQ '2' "DU-e com NF-e(Retificação)
               AND loekz      EQ @abap_false.

            IF sy-subrc EQ 0.
              MESSAGE |DU-e já lançada para a RUC { <lfs_item_sol>-numero_ruc } ! Id. DU-e { lwa_zsdt0170-id_due } !| TYPE 'I'.
              RETURN.
            ENDIF.
          ENDIF.

          wa_dados_geracao_ruc-ano              = sy-datum(4).
          wa_dados_geracao_ruc-codigo_ra        = <lfs_item_sol>-codigo_ra.
          wa_dados_geracao_ruc-edit_fields      = abap_false.
          wa_dados_geracao_ruc-id_nomeacao_tran = <lfs_item_sol>-id_nomeacao_tran.
          wa_dados_geracao_ruc-kunnr_exp        = wa_dados_geracao_ruc-kunnr_exp.
          wa_dados_geracao_ruc-matnr            = <lfs_item_sol>-matnr.

          DATA(lva_numero_ruc) = zcl_due=>gerar_nr_ruc_with_screen( i_dados_geracao_ruc = wa_dados_geracao_ruc ).

          IF lva_numero_ruc IS NOT INITIAL.

            <lfs_item_sol>-numero_ruc = lva_numero_ruc.

            IF <lfs_item_sol>-posnr IS NOT INITIAL.
              UPDATE zsdt0053 SET numero_ruc = <lfs_item_sol>-numero_ruc
               WHERE nro_sol_ov EQ wg_header-nro_sol_ov
                 AND posnr      EQ <lfs_item_sol>-posnr.
            ENDIF.
          ENDIF.

        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_clear_ruc.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.

          READ TABLE tg_itens ASSIGNING <lfs_item_sol> INDEX wg_selectedcell-row_id-index.
          CHECK ( sy-subrc EQ 0 ) AND ( wg_header-nro_sol_ov IS NOT INITIAL ).

          CHECK <lfs_item_sol>-numero_ruc IS NOT INITIAL.

          SELECT SINGLE *
            FROM zsdt0170 INTO @lwa_zsdt0170
           WHERE numero_ruc EQ @<lfs_item_sol>-numero_ruc
             AND tp_due     EQ '2' "DU-e com NF-e(Retificação)
             AND loekz      EQ @abap_false.

          IF sy-subrc EQ 0.
            MESSAGE |DU-e já lançada para a RUC { <lfs_item_sol>-numero_ruc } ! Id. DU-e { lwa_zsdt0170-id_due } !| TYPE 'I'.
            RETURN.
          ENDIF.

          CLEAR: <lfs_item_sol>-numero_ruc.

          IF <lfs_item_sol>-posnr IS NOT INITIAL.
            UPDATE zsdt0053 SET numero_ruc = space
             WHERE nro_sol_ov EQ wg_header-nro_sol_ov
               AND posnr      EQ <lfs_item_sol>-posnr.
          ENDIF.


        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.


      WHEN c_chg_doc.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_itens INTO wl_itens INDEX wg_selectedcell-row_id-index.
          IF sy-subrc IS INITIAL.
            CHECK wl_itens-vbeln IS NOT INITIAL.
            zcl_solicitacao_ov=>ck_doc_exp( wl_itens-vbeln ).
          ENDIF.

        ENDLOOP.

      WHEN c_chg_ruc.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.

          READ TABLE tg_itens ASSIGNING <lfs_item_sol> INDEX wg_selectedcell-row_id-index.
          CHECK ( sy-subrc EQ 0 ) AND ( wg_header-nro_sol_ov IS NOT INITIAL ).
          CHECK ( <lfs_item_sol>-posnr IS NOT INITIAL ).

          CLEAR: gwa_change_ruc_item.

          gwa_change_ruc_item-nro_sol_ov     = wg_header-nro_sol_ov.
          gwa_change_ruc_item-posnr          = <lfs_item_sol>-posnr.
          gwa_change_ruc_item-numero_ruc     = <lfs_item_sol>-numero_ruc.
          gwa_change_ruc_item-numero_ruc_old = <lfs_item_sol>-numero_ruc.

          CALL SCREEN 0110 STARTING AT 02 02.

          IF gwa_change_ruc_item-change_item EQ abap_true.
            <lfs_item_sol>-numero_ruc = gwa_change_ruc_item-numero_ruc.
          ENDIF.

        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.


      WHEN c_chg_itm.
        contador = contador + 1.

        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          IF wg_header-param_espec NE c_m.
            READ TABLE tg_itens INTO wl_itens INDEX wg_selectedcell-row_id-index.
            IF wl_itens-vbeln IS INITIAL.
              IF wl_itens-status_itm IS INITIAL.

                wl_itens-status_itm = c_f.
                wl_itens-line_color = 'C510'.
              ELSE.
                wl_itens-status_itm = space.
                wl_itens-line_color = space.

              ENDIF.
              MODIFY tg_itens FROM wl_itens INDEX wg_selectedcell-row_id-index
                  TRANSPORTING status_itm line_color.

            ELSE.
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m01."'Item contém ordem. Não é possivel modificar o status do item.'.
            ENDIF.
          ELSE.
            READ TABLE tg_itens INTO wl_itens_aux INDEX wg_selectedcell-row_id-index.
            LOOP AT tg_itens INTO wl_itens
              WHERE fixacao EQ wl_itens_aux-fixacao
                AND posnr   EQ wl_itens_aux-posnr
                AND fixacao IS NOT INITIAL
                AND posnr   IS NOT INITIAL.
              IF wl_itens-vbeln IS INITIAL.
                IF  wl_itens-status_itm IS INITIAL.
*                  PERFORM RETORNA_PRECO_ITEM USING   '4'
*                                           WL_ITENS-FIXACAO
*                                  CHANGING WL_ZMENG.
                  CLEAR: wl_zmeng.
                  IF <fs_table> IS ASSIGNED.
                    UNASSIGN <fs_line>.

                    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

                    ASSIGN t_new_line->* TO <fs_line>.

                    LOOP AT tg_preco_n INTO tg_preco_n
                          WHERE preco EQ '4'.

                      LOOP AT <fs_table> INTO <fs_line>.
                        IF wl_itens-fixacao NE space.
                          CLEAR: wg_valor.
                          PERFORM get_set_valores USING 'POSNR'
                                                        'G'
                                               CHANGING wg_valor.
                          IF wg_valor NE wl_itens-fixacao.
                            CONTINUE.
                          ENDIF.
                        ENDIF.

                        CLEAR: wg_valor.
                        PERFORM get_set_valores USING 'COD_FP'
                                                      'G'
                                             CHANGING wg_valor.

                        IF wg_valor EQ tg_preco_n-cod_fp.
                          CLEAR: wg_valor.
                          PERFORM get_set_valores USING tg_preco_n-field
                                                        'G'
                                                 CHANGING wg_valor.

                          TRANSLATE wg_valor USING '. '.
                          TRANSLATE wg_valor USING ',.'.
                          CONDENSE wg_valor NO-GAPS.

                          IF wg_valor IS NOT INITIAL.
                            wl_zmeng = wg_valor.
                            EXIT.
                          ENDIF.
                        ENDIF.
                      ENDLOOP.
                      IF wl_zmeng IS NOT INITIAL.
                        EXIT.
                      ENDIF.
                    ENDLOOP.
                  ENDIF.
                ENDIF.

                IF  wl_zmeng IS INITIAL .
                  DATA: up_tabix TYPE sy-tabix.
                  LOOP AT tg_itens INTO wl_itens WHERE fixacao EQ wl_itens_aux-fixacao.
                    up_tabix = sy-tabix.

                    IF wl_itens-status_itm IS INITIAL.

*                     Inclui a data que o usuario Liberou o Frete
                      wl_itens-data_lib_frame = sy-datum.
                      wl_itens-status_itm = c_f.
                      wl_itens-line_color = 'C510'.

                      wl_itens_frete-direcao = 'L'.
                      wl_itens_frete-nro_ov = wg_header-nro_sol_ov.
                      wl_itens_frete-seq = contador.

                      MOVE-CORRESPONDING wl_itens TO wl_itens_frete.
                      APPEND wl_itens_frete TO tl_itens_frete.

                      CASE wg_header-param_espec.
                        WHEN: 'M' OR 'Z'.
                          CLEAR: wa_style, wl_itens-style.
                          REFRESH: style.

                          wa_style-fieldname = 'MATNR'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'FIXACAO'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'WERKS'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'PONTO_C'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'TERMINAL'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'LIFNR'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'LGORT'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'CHARG'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'ZMENG'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'BRGEW'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'ZIEME'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'PMEIN'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'KURSK'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'VOLUM'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'VOLEH'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          wa_style-fieldname = 'VALDT'.
                          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                          INSERT  wa_style INTO TABLE style .

                          INSERT LINES OF style INTO TABLE wl_itens-style.
                      ENDCASE.

                    ELSE.
                      IF wg_redistribuir IS INITIAL .

                        wl_itens-data_lib_frame = space.
                        wl_itens-status_itm     = space.
                        wl_itens-line_color     = space.
                        wl_itens_frete-direcao = 'E'.
                        wl_itens_frete-nro_ov = wg_header-nro_sol_ov.
                        wl_itens_frete-seq = contador.

                        MOVE-CORRESPONDING wl_itens TO wl_itens_frete.
                        APPEND wl_itens_frete TO tl_itens_frete.

                      ENDIF.

                    ENDIF.
                    MODIFY tg_itens FROM wl_itens INDEX up_tabix TRANSPORTING status_itm data_lib_frame line_color style.

                  ENDLOOP.
                ELSE.

                  IF wl_erro IS INITIAL.
                    wl_erro = c_x.
                    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m02 TEXT-m30."'Qtd Fixada diferente da Qtda Prevista. Não é possivel modificar o status do item.'.
                  ENDIF.
                ENDIF.
              ELSE.
                IF wl_erro IS INITIAL.
                  wl_erro = c_x.
                  MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m01."'Item contém ordem. Não é possivel modificar o status do item.'.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.


*        IF ( WL_ERRO IS INITIAL ) AND ( WG_REDISTRIBUIR IS INITIAL )." AND WL_ITENS_AUX-STATUS_ITM  IS INITIAL .
*
*          CASE WG_HEADER-WAERK.
*            WHEN: 'BRL'.
*              FREE: LOBJ_ZCL_WEBSERVICE_TAXA_CURVA.
*              CREATE OBJECT LOBJ_ZCL_WEBSERVICE_TAXA_CURVA.
*              TRY.
*                  LOBJ_ZCL_WEBSERVICE_TAXA_CURVA->EXECUTAR( I_NUMERO  =  WG_HEADER-NRO_SOL_OV
*                                                            I_TIPO    = 'FRE'
*                                                            I_FIXACAO = WL_ITENS-FIXACAO
*                                                            I_STATUS  = WL_ITENS-STATUS_ITM
*                                                            I_TCODE   = SY-TCODE
*                                                            ).
*
*                CATCH ZCX_WEBSERVICE INTO CX_EXCEPTION.
*                  VAR_MSG = CX_EXCEPTION->GET_TEXT( ).
*                  MESSAGE E007(ZWEBSERVICE) DISPLAY LIKE 'W' WITH VAR_MSG.
*              ENDTRY.
*          ENDCASE.
*        ENDIF.




      WHEN c_atual_itens.
        LOOP AT tg_itens INTO wl_itens.
          IF wg_header-param_espec EQ c_m .
            PERFORM retorna_preco_item USING   '1'
                                           wl_itens-fixacao
                                     CHANGING wl_itens-dmbtr.
          ELSE.
            IF ( wg_header-param_espec NE c_a AND
                 wg_header-param_espec NE c_p AND           "US #73163
*-CS2021000615 - 17.06.2021 - JT - inicio
                 wg_header-param_espec NE c_ax  ")
              AND  wg_header-param_espec NE c_z )" CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
            AND wg_acao NE c_modif_c_ov.
              PERFORM retorna_preco_item USING '1'
                                               space
                                       CHANGING wl_itens-dmbtr.
            ENDIF.

          ENDIF.

          MODIFY tg_itens FROM wl_itens.
          wl_good_cells-row_id = sy-tabix.
          APPEND wl_good_cells TO tl_good_cells.
        ENDLOOP.

        " 29.11.2023 - 128467 - RBL -->
        lcl_event_handler=>on_data_changed_finished( et_good_cells = tl_good_cells e_modified = 'I' ). " Modifica só item
        " 29.11.2023 - 128467 - RBL <--

      WHEN c_add.

        tl_itens_aux[] = tg_itens[].
        REFRESH: tg_itens.

        LOOP AT tl_itens_aux INTO wl_itens.
*          WL_ITENS-POSNR = SY-TABIX * 10.
          APPEND wl_itens TO tg_itens.
        ENDLOOP.

        DESCRIBE TABLE tg_itens LINES wl_lines.
        CLEAR: wl_itens, posnr.


        SORT tg_itens BY posnr fixacao.                    " Ordena
        READ TABLE tg_itens INTO wl_itens INDEX wl_lines.  " Pega o utima pos NR
        posnr  =  wl_itens-posnr.                          " Guarda na variavel temporaria
        CLEAR: wl_itens.                                   " Limpa
        wl_itens-posnr = posnr + 10 .                      " Armazena adicionando 10

*        WL_ITENS-POSNR = ( WL_LINES + 1 ) * 10 .

        IF wg_header-param_espec EQ c_m.
          PERFORM retorna_preco_item USING '1'
                                           wl_itens-fixacao
                                  CHANGING wl_itens-dmbtr.

        ELSE.
          PERFORM retorna_preco_item USING '1'
                                           space
                                  CHANGING wl_itens-dmbtr.

        ENDIF.
        IF wg_cond_pgt-valdt IS NOT INITIAL.
          wl_itens-valdt = wg_cond_pgt-valdt.
        ENDIF.

        IF wg_redistribuir IS INITIAL
        OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
        OR wg_header-param_espec EQ c_ax
*-CS2021000615 - 17.06.2021 - JT - fim
        OR wg_header-param_espec EQ c_p
        OR wg_header-param_espec EQ c_m
        OR wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
          wl_itens-item_edit = c_x.
          IF wg_header-param_espec EQ c_m " or wg_header-param_espec EQ c_z " CS2020000373 23/08/2022 -LP Não bloquear campos
          AND wg_redistribuir IS INITIAL.
            READ TABLE tg_itens TRANSPORTING NO FIELDS
              WITH KEY item_edit = c_x.
            IF sy-subrc IS INITIAL.
**          Bloquear campos
              CALL METHOD grid1->get_frontend_fieldcatalog
                IMPORTING
                  et_fieldcatalog = t_fieldcatalog.
              REFRESH: style.
              LOOP AT t_fieldcatalog INTO w_fieldcatalog
               WHERE edit EQ c_x
                 AND fieldname NE 'ZMENG'
                 AND fieldname NE 'VALDT'
                 AND fieldname NE 'FIXACAO'
                 AND fieldname NE 'CHARG'
                 AND fieldname NE 'VLRTOT'.
                wa_style-fieldname = w_fieldcatalog-fieldname.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_no_delete_row.
                INSERT  wa_style INTO TABLE style .
              ENDLOOP.

              CLEAR wl_itens-item_edit.
              INSERT LINES OF style INTO TABLE wl_itens-style.
            ELSE.
              REFRESH: style.
            ENDIF.

          ELSEIF wg_header-param_espec EQ c_m." OR wg_header-param_espec EQ c_z.
            READ TABLE tg_itens INTO wl_itens_aux
            WITH KEY item_edit = c_x.

            wl_itens_aux-posnr = wl_itens-posnr.
            MOVE-CORRESPONDING: wl_itens_aux TO wl_itens.
            wl_itens-item_edit = c_r.
            wl_itens-status = space.
            wl_itens-line_color = space.
            wl_itens-status_itm = space.
            CLEAR: wl_itens-zmeng, wl_itens-vbeln, wl_itens-fixacao.

          ELSEIF ( wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
                   wg_header-param_espec EQ c_ax ).
            " OR wg_header-param_espec EQ c_z ) ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
            FREE: style, wa_style.
            wa_style-fieldname = 'LOTES_VOL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_button.
            INSERT  wa_style INTO TABLE style .
            INSERT LINES OF style INTO TABLE wl_itens-style.

          ELSEIF wg_header-param_espec EQ c_z.
*            READ TABLE tg_itens INTO wl_itens_aux
*                       WITH KEY item_edit = c_x.

            wl_itens_aux-posnr = wl_itens-posnr.
            MOVE-CORRESPONDING: wl_itens_aux TO wl_itens.
            wl_itens-item_edit = c_r.
            wl_itens-status = space.
            wl_itens-line_color = space.
            wl_itens-status_itm = space.
            CLEAR: wl_itens-zmeng, wl_itens-vbeln, wl_itens-fixacao.

            FREE: style, wa_style.
            wa_style-fieldname = 'LOTES_VOL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_button.
            INSERT  wa_style INTO TABLE style .
            INSERT LINES OF style INTO TABLE wl_itens-style.

          ENDIF.
        ELSE.
          READ TABLE tg_itens INTO wl_itens_aux
            WITH KEY item_edit = c_x.

          wl_itens_aux-posnr = wl_itens-posnr.
          MOVE-CORRESPONDING: wl_itens_aux TO wl_itens.
          wl_itens-item_edit = c_r.
          wl_itens-status = space.
          wl_itens-line_color = space.
          wl_itens-status_itm = space.

          CLEAR: wl_itens-zmeng, wl_itens-vbeln, wl_itens-fixacao.
        ENDIF.

        IF wg_header-matnr IS NOT INITIAL AND ( wg_header-param_espec NE c_a AND
*-CS2021000615 - 17.06.2021 - JT - inicio
                                                wg_header-param_espec NE c_ax ).
*-CS2021000615 - 17.06.2021 - JT - fim


          IF strlen( wg_header-matnr ) EQ 40.
            l_matnr = wg_header-matnr+22(18).
          ELSE.
            l_matnr = wg_header-matnr.
          ENDIF.

          APPEND INITIAL LINE TO lr_matnr ASSIGNING FIELD-SYMBOL(<mat>).
          <mat>-sign = 'I'.
          <mat>-option = 'EQ'.
          <mat>-low = l_matnr.

          APPEND INITIAL LINE TO lr_matnr ASSIGNING <mat>.
          <mat>-sign = 'I'.
          <mat>-option = 'EQ'.
          <mat>-low = wg_header-matnr.

          SELECT SINGLE matnr, maktx
             FROM makt
             INTO @DATA(l_maktx)
              WHERE matnr IN @lr_matnr
                  AND spras EQ @sy-langu.


          IF sy-subrc IS INITIAL.
            IF l_maktx-matnr = l_matnr.
              wg_header-matnr = l_matnr.
            ENDIF.
          ENDIF.

          wl_itens-matnr = wg_header-matnr.
          wl_itens-maktx = l_maktx-maktx.

        ENDIF.

*         wl_itens-pmein = 'TO'.  "Está

        wl_itens-lotes_vol = '@1F@'.

        APPEND wl_itens TO tg_itens.


        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
        IF wg_redistribuir IS NOT INITIAL.
          enter.
        ENDIF.
      WHEN c_del.
        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_itens INTO wl_itens INDEX  wg_selectedcell-row_id-index.
*          IF WL_ITENS-STYLE[] IS INITIAL.

          IF wg_redistribuir IS INITIAL.
            CLEAR t_0213.
            w_click = tg_itens[ wg_selectedcell-row_id ].

            IF w_click-instrucao_ant IS NOT INITIAL. " Alterado 28/11/2019 CS2019001087

              DATA(r_lgort_) =
              VALUE zrsdsselopts( FOR ls IN tg_instrucao
              WHERE ( instrucao EQ w_click-instrucao_ant )
              (
                option = 'EQ'
                sign = 'I'
                low = ls-charg
              ) ).

            ENDIF.
            t_0213 = it_0213.
            DELETE t_0213 WHERE posnr NE w_click-posnr.
            DELETE t_0213 WHERE lgort NOT IN r_lgort_.

            TRY .
                DATA(_linhas) = t_0213[ 1 ].
              CATCH cx_sy_itab_line_not_found.
                CLEAR _linhas.
            ENDTRY.

            IF _linhas IS NOT INITIAL.

              _linhas-status = abap_true.

              APPEND CORRESPONDING #( _linhas ) TO it_0213_aux.
              "MODIFY IT_0213_AUX FROM _LINHAS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
              DELETE t_0213 WHERE nro_sol_ov EQ _linhas-nro_sol_ov
                             AND posnr EQ _linhas-posnr
                             AND lgort EQ _linhas-lgort.

              DELETE it_0213 WHERE nro_sol_ov EQ _linhas-nro_sol_ov
                             AND posnr EQ _linhas-posnr
                             AND lgort EQ _linhas-lgort.
            ENDIF.

            IF wg_header-param_espec EQ c_p.
              IF wl_itens-item_edit EQ c_x.
                IF wl_itens-vbeln IS INITIAL.
                  DELETE tg_itens INDEX wg_selectedcell-row_id-index.
                ELSE.
                  MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m03."'A linha selecionada nao pode ser eliminada, a linha já contem Ordem de venda'.
                ENDIF.
              ENDIF.
            ELSE.
              IF wl_itens-item_edit EQ c_x.
                IF wl_itens-vbeln IS INITIAL.

                  DELETE tg_itens INDEX wg_selectedcell-row_id-index.
                ENDIF.
                DELETE tg_itens WHERE matnr EQ wl_itens-matnr
                                  AND vbeln EQ space.
                IF wg_acao EQ c_modif_c_ov.
                  IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z .
                    PERFORM remove_preco_frame.
                  ENDIF.
                ENDIF.
              ELSE.
                MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m04."'A linha selecionada nao pode ser eliminada, eliminar a linha principal do item'.
              ENDIF.
            ENDIF.
          ELSE.
            IF wl_itens-item_edit EQ c_r.
              DELETE tg_itens INDEX wg_selectedcell-row_id-index.
              IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z .
                PERFORM remove_preco_frame.
              ENDIF.
              CALL METHOD grid1->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.
              enter.
            ELSE.
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m05."'A linha selecionada nao pode ser eliminada, eliminar apenas linhas de redistribuição.'.
            ENDIF.
          ENDIF.




        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_parcelar.
        IF wg_cond_pgt-qte_venc GT 1.
          READ TABLE tg_itens INTO wl_itens INDEX 1.
          IF sy-subrc IS INITIAL.
            IF wg_header-param_espec NE c_m ."or wg_header-param_espec NE c_z .
              PERFORM retorna_preco_item USING '1'
                                               space
                                      CHANGING wl_itens-dmbtr.
            ENDIF.
            MOVE wg_cond_pgt-qte_venc TO wl_cont.
            DESCRIBE TABLE tg_itens LINES wl_lines.
            SUBTRACT wl_lines FROM wl_cont.
            tl_itens_aux[] = tg_itens[].
            REFRESH: tg_itens.
            LOOP AT tl_itens_aux INTO wl_itens_aux.
              wl_itens_aux-posnr = sy-tabix * 10.
              APPEND wl_itens_aux TO tg_itens.
            ENDLOOP.
**          Bloquear campos
            CALL METHOD grid1->get_frontend_fieldcatalog
              IMPORTING
                et_fieldcatalog = t_fieldcatalog.
            IF wg_header-param_espec EQ c_p.
              CLEAR: wa_style.
              REFRESH: style.
              LOOP AT t_fieldcatalog INTO w_fieldcatalog
               WHERE edit EQ c_x
                 AND fieldname NE 'ZMENG'
                 AND fieldname NE 'VALDT'
                 AND fieldname NE 'VLRTOT'.
                wa_style-fieldname = w_fieldcatalog-fieldname.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_no_delete_row.
                INSERT  wa_style INTO TABLE style .
              ENDLOOP.
            ELSE.
              CLEAR: wa_style.
              REFRESH: style.
              LOOP AT t_fieldcatalog INTO w_fieldcatalog WHERE edit EQ c_x.
                CASE w_fieldcatalog-fieldname.
                  WHEN: 'ZMENG' OR 'VALDT'.
                    wa_style-fieldname = w_fieldcatalog-fieldname.
                    wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                    INSERT  wa_style INTO TABLE style .
                  WHEN OTHERS.
                    wa_style-fieldname = w_fieldcatalog-fieldname.
                    wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_no_delete_row.
                    INSERT  wa_style INTO TABLE style .
                ENDCASE.
              ENDLOOP.
            ENDIF.
            INSERT LINES OF style INTO TABLE wl_itens-style.
***         FIM DE BLOQUEI DE CAMPOS
            DO wl_cont TIMES.
              DESCRIBE TABLE tg_itens LINES wl_lines.
              CLEAR: wl_itens-zmeng, wl_itens-valdt, wl_itens-vlrtot,
                     wl_itens-item_edit.
              wl_itens-posnr = ( wl_lines + 1 ) * 10 .
              IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
                PERFORM retorna_preco_item USING '1'
                                                 wl_itens-posnr
                                        CHANGING wl_itens-dmbtr.
              ENDIF.
              APPEND wl_itens TO tg_itens.
            ENDDO.
          ENDIF.
        ENDIF.
        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_saldo.
        PERFORM busca_saldo.
        IF tg_saldo[] IS NOT INITIAL.
          PERFORM montar_layout USING 'SALDO'.
          PERFORM exibe_saldo.
        ENDIF.

*-CS2023000189-#126959-07.11.2023-JT-inicio
      WHEN c_trace_cotton.
        PERFORM f_reenvia_trace_ordens.

*-CS2023000189-#126959-07.11.2023-JT-fim
    ENDCASE.

  ENDMETHOD.                    "zm_handle_user_command
  METHOD handle_user_command_ins.

    DATA: it_sel_rows TYPE lvc_t_row,
          wa_sel_rows TYPE lvc_s_row,
          linhas      TYPE TABLE OF zsdt0045.

    CASE e_ucomm.

      WHEN c_add.

        DATA: wa_ins TYPE zeinstrucao.

        CLEAR wa_ins.
        wa_ins-contrato = wg_header-bstkd.
        APPEND wa_ins TO tg_instrucao.

        CALL METHOD grid6->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.

        CALL METHOD grid6->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        IF lines( it_sel_rows ) EQ 1.
          READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
          DELETE tg_instrucao INDEX wa_sel_rows-index.
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m29.
        ENDIF.

        CALL METHOD grid6->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN 'FRETE'.

        CALL METHOD grid6->get_selected_rows
          IMPORTING
            et_index_rows = it_sel_rows.

        CHECK NOT it_sel_rows IS INITIAL.

        FREE: tg_ins_frete_aux, linhas.

        LOOP AT it_sel_rows INTO wa_sel_rows.

          READ TABLE tg_instrucao INTO tg_instrucao INDEX wa_sel_rows-index.
          MOVE-CORRESPONDING tg_instrucao TO wg_ins_frete_aux.
*          READ TABLE TG_INS_FRETE INTO WG_INS_FRETE INDEX WA_SEL_ROWS-INDEX.
*          MOVE-CORRESPONDING WG_INS_FRETE TO WG_INS_FRETE_AUX.

          APPEND wg_ins_frete_aux TO tg_ins_frete_aux.
          CLEAR: wg_ins_frete_aux.

        ENDLOOP.

        MOVE tg_ins_frete_aux TO linhas.
        SORT linhas BY instrucao.

        DELETE ADJACENT DUPLICATES FROM linhas COMPARING instrucao.

        IF lines( linhas ) EQ 1.
          CALL SCREEN 300 ENDING AT 60 7 STARTING AT 3 3.
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m27.
        ENDIF.


    ENDCASE.

  ENDMETHOD.                    "HANDLE_USER_COMMAND_ins
  METHOD handle_user_command_form.
    DATA: tl_form_lote_aux LIKE TABLE OF tg_form_lote,
          wl_form_lote_aux LIKE LINE OF tg_form_lote,
          wl_form_lote     LIKE LINE OF tg_form_lote,
          wl_lines         TYPE sy-tabix.

    CASE e_ucomm.
      WHEN c_add.

        tl_form_lote_aux[] = tg_form_lote[].
        REFRESH: tg_form_lote.
        LOOP AT tl_form_lote_aux INTO wl_form_lote.
*          WL_FORM_LOTE-POSNR  = SY-TABIX * 10.
          APPEND wl_form_lote TO tg_form_lote.
        ENDLOOP.

        DESCRIBE TABLE tg_form_lote LINES wl_lines.

        CLEAR: posnr.
        SORT tg_form_lote BY posnr.                    " Ordena
        READ TABLE tg_form_lote INTO wl_form_lote INDEX wl_lines.  " Pega o utima pos NR
        posnr  =  wl_form_lote-posnr.                          " Guarda na variavel temporaria
        CLEAR: wl_form_lote.                                   " Limpa
        wl_form_lote-posnr = posnr + 10 .                      " Armazena adicionando 10

*        CLEAR: WL_FORM_LOTE.

*        WL_FORM_LOTE-POSNR = ( WL_LINES + 1 ) * 10.
        wl_form_lote-classificacao = 'C'.

        APPEND wl_form_lote TO tg_form_lote.

        CALL METHOD grid7->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.
        CALL METHOD grid7->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_form_lote INTO wl_form_lote INDEX  wg_selectedcell-row_id-index.
          IF wl_form_lote-vbeln IS INITIAL.
*            DELETE TG_FORM_LOTE INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
            tg_form_lote[ wg_selectedcell-row_id-index ]-status = '@F1@'.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m03."'A linha selecionada nao pode ser eliminada, a linha já contem Ordem de venda'.
          ENDIF.

        ENDLOOP.

        enter.
        wg_acao = c_modif_c_ov.

        CALL METHOD grid7->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_frete_entrada.

        CALL METHOD grid7->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        CHECK tg_selectedcell IS NOT INITIAL.

        CLEAR index_entrada.
        index_entrada = tg_selectedcell[ 1 ]-row_id-index.

        READ TABLE tg_form_lote INTO wa_emite_entrada INDEX index_entrada.

        PERFORM pf_modify_frete_entrada.

        CALL METHOD grid7->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_liberar.
        CALL METHOD grid7->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_form_lote INTO wl_form_lote INDEX wg_selectedcell-row_id-index.
          wl_form_lote-status = icon_release.
          MODIFY tg_form_lote FROM wl_form_lote INDEX wg_selectedcell-row_id-index .
          UPDATE  zsdt0066 SET status = c_l
                         WHERE nro_sol_ov EQ wg_header-nro_sol_ov
                           AND instrucao  EQ wl_form_lote-instrucao
                           AND posnr      EQ wl_form_lote-posnr.
        ENDLOOP.

        CALL METHOD grid7->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
    ENDCASE.
  ENDMETHOD.                    "HANDLE_USER_COMMAND_form

  METHOD handle_user_command_pedido.

    CASE e_ucomm.
      WHEN c_add.

    ENDCASE.
  ENDMETHOD.

  METHOD handle_user_command_p_fram.
    CASE e_ucomm.
      WHEN c_clos_grid.
        CALL METHOD splitter->set_column_sash
          EXPORTING
            id    = 1
            type  = splitter->type_movable
            value = splitter->false.

*    posiciona spliter na altura x
        CALL METHOD splitter->set_column_width
          EXPORTING
            id    = 1
            width = 100.
    ENDCASE.
  ENDMETHOD.                    "handle_user_command_P_FRAM
  METHOD handle_user_command_pgt_ant.
    DATA: wl_par      TYPE ctu_params,
          wl_shdbnr   TYPE zshdbt0001-shdbnr,
          wl_pgto_ant LIKE LINE OF tg_pgt_ant,
          wl_dmbtr    TYPE dmbtr,
          vxreversal  TYPE bkpf-xreversal,
          wl_zsdt0051 TYPE zsdt0051,
          wl_data(10),
          it_sel_rows TYPE lvc_t_row,
          wa_sel_rows TYPE lvc_s_row.

    CASE e_ucomm.
      WHEN c_add.
        CLEAR: tg_pgt_ant.
        IF wg_header-param_espec EQ c_p
        AND ( wg_cond_pgt-pgto_ant EQ c_x
        OR   wg_cond_pgt-pgto_ant EQ c_n ).
          PERFORM retorna_preco_item USING '3'
                                           space
                                  CHANGING wl_dmbtr.

          tg_pgt_ant-dmbtr = wl_dmbtr.
          tg_pgt_ant-valdt = wg_cond_pgt-valdt.
          APPEND tg_pgt_ant TO tg_pgt_ant.
          CLEAR: tg_pgt_ant.
        ELSE.
          APPEND INITIAL LINE TO tg_pgt_ant.
        ENDIF.
        CALL METHOD grid2->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.
        CALL METHOD grid2->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_pgt_ant INTO wl_pgto_ant INDEX wg_selectedcell-row_id-index.
          IF wl_pgto_ant-adiant IS INITIAL.
            DELETE tg_pgt_ant INDEX wg_selectedcell-row_id-index.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m06."'A linha selecionada nao pode ser eliminada, a linha já contem documento.'.
          ENDIF.
        ENDLOOP.

        CALL METHOD grid2->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_gerar.

        DATA: lv_bktxt   TYPE bkpf-bktxt,
              lv_mode    TYPE c,
              wl_pgt_ant LIKE LINE OF tg_pgt_ant,
              lv_dmbtr   TYPE c LENGTH 13, "BSEG-WRBTR,
              lv_kunnr   TYPE c LENGTH 17,
              wl_0036    TYPE zfit0036,
              wl_0054    TYPE zsdt0054.

*-#82210-jaime
        CALL METHOD grid2->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.
*        CALL METHOD grid2->refresh_table_display.
*        CALL METHOD grid2->check_changed_data.
*
*        CALL METHOD grid2->get_selected_rows
*          IMPORTING
*            et_index_rows = it_sel_rows.

        "Busca tipos de ordem
        SELECT *
          FROM setleaf INTO TABLE @DATA(it_tpoderm)
         WHERE setname = 'ZSDR0022_TP_OV_PAG_ANT'.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          REFRESH: tg_bdc, it_dta.
          CLEAR: wg_bdc.
          READ TABLE tg_pgt_ant INTO wl_pgt_ant INDEX wg_selectedcell-row_id-index.
          IF wl_pgt_ant-adiant IS INITIAL.
            CONCATENATE 'Sol.' wg_header-nro_sol_ov INTO lv_bktxt.

            READ TABLE it_tpoderm  INTO DATA(wg_tpoderm)
                                                       WITH KEY valfrom = wg_header-auart.

            IF sy-subrc IS INITIAL.

              IF wl_pgt_ant-dt_emissaopi IS NOT INITIAL.
                CONCATENATE wl_pgt_ant-dt_emissaopi+6(2) '.' wl_pgt_ant-dt_emissaopi+4(2) '.' wl_pgt_ant-dt_emissaopi+0(4) INTO wl_data.
              ELSE.
                CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO wl_data.
              ENDIF.

            ELSE.

              CONCATENATE sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum+0(4) INTO wl_data.

            ENDIF.

            MOVE wl_pgt_ant-dmbtr TO lv_dmbtr.
            TRANSLATE lv_dmbtr USING '.,'.
            CONDENSE lv_dmbtr NO-GAPS.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wg_header-kunnr
              IMPORTING
                output = lv_kunnr.

            CONCATENATE 'P.I.' wl_pgt_ant-nr_provis_inv INTO DATA(l_zuonr).
            CONDENSE l_zuonr NO-GAPS.

            PERFORM f_bdc_data USING:
                  ' '         ' '         'T'     'F-37'        ' ',
                  'SAPMF05A'  '0113'      'X'     ' '           ' ',
                  ' '         ' '         ' '     'BKPF-BLDAT'  wl_data,
                  ' '         ' '         ' '     'BKPF-BLART'  'DZ',
                  ' '         ' '         ' '     'BKPF-BUKRS'  wg_header-vkorg,
                  ' '         ' '         ' '     'BKPF-BUDAT'  wl_data,
                  ' '         ' '         ' '     'BKPF-MONAT'  sy-datum+4(2),
                  ' '         ' '         ' '     'BKPF-WAERS'  wg_header-waerk,
                  ' '         ' '         ' '     'BKPF-BKTXT'  lv_bktxt,
                  ' '         ' '         ' '     'BKPF-XBLNR'  lv_bktxt,
                  ' '         ' '         ' '     'RF05A-NEWKO' wg_header-kunnr,
                  ' '         ' '         ' '     'RF05A-ZUMSK' 'A',
                  ' '         ' '         ' '     'BDC_OKCODE'  '/00',
                  'SAPMF05A'  '0304'      'X'     ' '           ' ' ,
                  ' '         ' '         ' '     'BSEG-WRBTR'  lv_dmbtr,
                  ' '         ' '         ' '     'BSEG-GSBER'  wg_header-vkbur,
                  ' '         ' '         ' '     'BSEG-ZUONR'  l_zuonr. "'ALGODAO'.
            CONCATENATE wl_pgt_ant-valdt+6(2) '.' wl_pgt_ant-valdt+4(2) '.' wl_pgt_ant-valdt+0(4) INTO wl_data.
            PERFORM f_bdc_data USING:
                  ' '         ' '         ' '     'BSEG-ZFBDT'  wl_data,
                  ' '         ' '         ' '     'BDC_OKCODE'  '=ZK',
                  'SAPMF05A'  '0331'      'X'     ' '           ' ',
                  ' '         ' '         ' '     'BSEG-HBKID'  wg_cond_pgt-hbkid,
                  ' '         ' '         ' '     'BDC_OKCODE'  '=BU'.

            lv_mode = 'N'.

            PERFORM f_call_transaction USING 'F-37'
                                             lv_mode "'N'
                                             'X'.

            IF it_msg[] IS NOT INITIAL.
              READ TABLE it_msg INTO wa_msg WITH KEY msgtyp = 'S'
                                                     msgnr  = '312'.
              IF sy-subrc IS INITIAL.
                CONDENSE it_msg-msgv1 NO-GAPS.
                wl_pgt_ant-adiant = wa_msg-msgv1.
                MODIFY tg_pgt_ant FROM wl_pgt_ant INDEX wg_selectedcell-row_id-index TRANSPORTING adiant.

                CLEAR wl_0036.
                CONCATENATE 'PB' wl_pgt_ant-adiant INTO wl_0036-obj_key.
                wl_0036-bukrs     = wg_header-vkorg.
                wl_0036-invoice   = wg_header-nro_sol_ov.
                SHIFT wl_0036-invoice LEFT DELETING LEADING '0'.
                wl_0036-moeda_pgto = wg_header-waerk.
                wl_0036-data_atual = sy-datum.
                wl_0036-hora_atual = sy-uzeit.
                wl_0036-usuario    = sy-uname.

                MODIFY zfit0036 FROM wl_0036.

              ELSE.
                READ TABLE it_msg INTO wa_msg INDEX 1.
                MESSAGE ID wa_msg-msgid TYPE 'I' NUMBER wa_msg-msgnr WITH wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4.
                "READ TABLE it_msgtext INTO it_msgtext INDEX 1.
                "MESSAGE it_msgtext-texto TYPE 'I'.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.

        CALL METHOD grid2->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_estornar.
        CALL METHOD grid2->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          CLEAR vxreversal.
          SELECT SINGLE *
            FROM zsdt0051
            INTO wl_zsdt0051
            WHERE nro_sol_ov = wg_header-nro_sol_ov.

          READ TABLE tg_pgt_ant INTO tg_pgt_ant
              INDEX wg_selectedcell-row_id-index.

          SELECT SINGLE xreversal
            INTO vxreversal
            FROM bkpf
            WHERE bukrs = wl_zsdt0051-vkorg
            AND   belnr = tg_pgt_ant-adiant
            AND   gjahr = wl_zsdt0051-data_venda(4).

          IF vxreversal = 1.
            UPDATE zsdt0054 SET adiant = ''
            WHERE nro_sol_ov = wg_header-nro_sol_ov
            AND   posnr      = tg_pgt_ant-posnr
            AND   valdt      = tg_pgt_ant-valdt.

            wl_pgto_ant-adiant = ''.
            MODIFY tg_pgt_ant FROM wl_pgto_ant INDEX wg_selectedcell-row_id-index TRANSPORTING adiant.
            CALL METHOD grid2->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
          ELSE.
            READ TABLE tg_pgt_ant INTO tg_pgt_ant
              INDEX wg_selectedcell-row_id-index.

            IF sy-subrc IS INITIAL
            AND tg_pgt_ant-adiant IS NOT INITIAL
            AND tg_pgt_ant-augbl IS INITIAL
            AND (    wg_header-param_espec EQ c_p
                  OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
                  OR wg_header-param_espec EQ c_ax
                  OR  wg_header-param_espec EQ c_z )." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
              REFRESH: tg_bdc.
              CLEAR: wg_bdc.
              WRITE sy-datum TO wl_data.

              f_preencher_dynpro:
               'X' 'SAPMF05A'             '0105',
               ' ' 'RF05A-BELNS'          tg_pgt_ant-adiant,
               ' ' 'BKPF-BUKRS'           wg_header-vkorg,
               ' ' 'RF05A-GJAHS'          wl_zsdt0051-data_venda(4),
               ' ' 'UF05A-STGRD'          '02',
               ' ' 'BSIS-BUDAT'           wl_data,
               ' ' 'BDC_OKCODE'           '=BU'.

              CALL FUNCTION 'ZSHDB_CRIA_EXECUTA'
                EXPORTING
                  tcode               = 'FB08'
                  params              = wl_par
                  i_callback_program  = 'ZSDR0026'
                  i_callback_formname = 'X_ESTORNA_ADIANT'
                IMPORTING
                  shdbnr              = wl_shdbnr
                TABLES
                  t_bdc               = tg_bdc.

              wl_pgto_ant-adiant = ''.

              MODIFY tg_pgt_ant FROM wl_pgto_ant INDEX wg_selectedcell-row_id-index TRANSPORTING adiant.
              CALL METHOD grid2->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

            ENDIF.
          ENDIF.
        ENDLOOP.
        acao c_search.
    ENDCASE.
  ENDMETHOD.                    "handle_user_command_pgt_ant
  METHOD handle_user_command_adto_ext.
    DATA: wl_par      TYPE ctu_params,
          wl_shdbnr   TYPE zshdbt0001-shdbnr,
          wl_preco    LIKE LINE OF tg_preco,
          wl_adto_ext LIKE LINE OF tg_adto_ext,
          wl_zsdt0051 TYPE zsdt0051,
          vxreversal  TYPE bkpf-xreversal,
          wl_dmbtr    TYPE bsid-dmbtr,
          wl_data(10).

    CASE e_ucomm.
      WHEN c_add.
        CLEAR: tg_adto_ext, wl_preco, wl_dmbtr.

        PERFORM retorna_preco_item USING   '1'
                                           space
                                  CHANGING wl_dmbtr.

        CASE wg_header-param_espec.
          WHEN 'A'.
            MOVE: wg_header-vkorg TO tg_adto_ext-bukrs.
*           MOVE: '0015' TO tg_adto_ext-bukrs.
          WHEN 'P'.
            MOVE: '0004'          TO tg_adto_ext-bukrs.
          WHEN OTHERS.
        ENDCASE.

        MOVE:  wl_dmbtr TO tg_adto_ext-dmbtr,
              'USD'     TO tg_adto_ext-waers.
        APPEND tg_adto_ext TO tg_adto_ext.

        CALL METHOD grid5->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.
        CALL METHOD grid5->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        CALL METHOD grid5->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_adto_ext INTO wl_adto_ext INDEX wg_selectedcell-row_id-index.
          IF wl_adto_ext-adiant IS INITIAL.
            DELETE tg_adto_ext INDEX wg_selectedcell-row_id-index.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m06."'A linha selecionada nao pode ser eliminada, a linha já contem documento.'.
          ENDIF.
        ENDLOOP.

        CALL METHOD grid5->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

*-CS2022000332-#78223-02.06.2022-JT-inicio
      WHEN c_gerar.
        PERFORM  liberacao_performance_adiant.
*-CS2022000332-#78223-02.06.2022-JT-fim

        CALL METHOD grid5->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_estornar.
        CALL METHOD grid5->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        CALL METHOD grid5->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          CLEAR vxreversal.
          READ TABLE tg_adto_ext INTO tg_adto_ext
              INDEX wg_selectedcell-row_id-index.

          SELECT SINGLE *
            FROM zsdt0051
            INTO wl_zsdt0051
            WHERE nro_sol_ov = wg_header-nro_sol_ov.

          SELECT SINGLE xreversal
            INTO vxreversal
            FROM bkpf
            WHERE bukrs = tg_adto_ext-bukrs
            AND   belnr = tg_adto_ext-adiant
            AND   gjahr = wl_zsdt0051-data_venda(4).

          IF vxreversal = 1.
            UPDATE zsdt0063 SET adiant = ''
            WHERE nro_sol_ov = wg_header-nro_sol_ov
            AND   posnr      = tg_adto_ext-posnr
            AND   valdt      = tg_adto_ext-valdt.

            wl_adto_ext-adiant = ''.
            MODIFY tg_adto_ext FROM wl_adto_ext INDEX wg_selectedcell-row_id-index TRANSPORTING adiant.
            CALL METHOD grid5->refresh_table_display
              EXPORTING
                is_stable = wa_stable.
          ELSE.
            READ TABLE tg_adto_ext INTO tg_adto_ext
              INDEX wg_selectedcell-row_id-index.
            IF sy-subrc IS INITIAL
            AND ( wg_header-param_espec EQ c_a OR
                  wg_header-param_espec EQ c_p  OR
                 wg_header-param_espec EQ c_z )" CS2020000373 23/08/2022 -LP
            AND tg_adto_ext-adiant IS NOT INITIAL
            AND tg_adto_ext-augbl IS INITIAL.
              REFRESH: tg_bdc.
              CLEAR: wg_bdc.
              WRITE sy-datum TO wl_data.

              f_preencher_dynpro:
               'X' 'SAPMF05A'             '0105',
               ' ' 'RF05A-BELNS'          tg_adto_ext-adiant,
               ' ' 'BKPF-BUKRS'           tg_adto_ext-bukrs,
               ' ' 'RF05A-GJAHS'          wl_zsdt0051-data_venda(4),
               ' ' 'UF05A-STGRD'          '01',
*         ' ' 'BSIS-BUDAT'           WL_DATA,
               ' ' 'BDC_OKCODE'           '=BU'.


              CALL FUNCTION 'ZSHDB_CRIA_EXECUTA'
                EXPORTING
                  tcode               = 'FB08'
                  params              = wl_par
                  i_callback_program  = 'ZSDR0026'
                  i_callback_formname = 'X_ESTORNA_ADIANT_EXT'
                IMPORTING
                  shdbnr              = wl_shdbnr
                TABLES
                  t_bdc               = tg_bdc.

              wl_adto_ext-adiant = ''.
              MODIFY tg_adto_ext FROM wl_adto_ext INDEX wg_selectedcell-row_id-index TRANSPORTING adiant.
              CALL METHOD grid5->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

            ENDIF.
          ENDIF.
        ENDLOOP.
        acao c_search.
    ENDCASE.

*-jaime
    wg_selectedcell-col_id-fieldname = 'POSNR'.
    wg_selectedcell-row_id-index     = 0.

    CALL METHOD grid5->set_selected_cells
      EXPORTING
        it_cells = tg_selectedcell.

  ENDMETHOD.                    "HANDLE_USER_COMMAND_adto_ext
  METHOD handle_hotspot_click_adto_ext.

    IF e_column_id-fieldname EQ 'ADIANT'.
      READ TABLE tg_adto_ext INTO tg_adto_ext
        INDEX e_row_id-index.
      IF sy-subrc IS INITIAL.
        SET PARAMETER ID 'BLN' FIELD tg_adto_ext-adiant.
        SET PARAMETER ID 'BUK' FIELD tg_adto_ext-bukrs.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD handle_user_command_logistica.

    DATA: wl_logistica LIKE LINE OF tg_logistica,
          wl_itens     LIKE LINE OF tg_itens.

    CASE e_ucomm.
      WHEN c_add.

*        APPEND INITIAL LINE TO TG_LOGISTICA.
        LOOP AT tg_logistica INTO wl_logistica WHERE zmeng IS NOT INITIAL.

        ENDLOOP.

        IF sy-subrc IS NOT INITIAL.

          CLEAR: wl_logistica.
          READ TABLE tg_itens INTO wl_itens INDEX 1.
          wl_logistica-zieme = wl_itens-zieme.

          APPEND wl_logistica TO tg_logistica.
        ENDIF.

        CALL METHOD grid3->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

      WHEN c_del.

        CALL METHOD grid3->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_logistica INTO wl_logistica INDEX wg_selectedcell-row_id-index.
          IF ( wl_logistica-style IS INITIAL ).
            DELETE tg_logistica INDEX wg_selectedcell-row_id-index.
          ENDIF.
        ENDLOOP.

        CALL METHOD grid3->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

    ENDCASE.
  ENDMETHOD.                    "handle_user_command_logistica
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD check_lifnr.

    DATA: l_lifnr       TYPE lifnr,
          wa_zsdt0108   TYPE zsdt0108,
          wa_good_lgort TYPE lvc_s_modi,
          lv_value      TYPE lvc_value,
          wl_mara       TYPE mara.

    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_row_id    = ps_good_lifnr-row_id
        i_fieldname = ps_good_lifnr-fieldname
      IMPORTING
        e_value     = l_lifnr.

    READ TABLE tg_itens ASSIGNING <itens> INDEX ps_good_lifnr-row_id.
    IF sy-subrc IS INITIAL.
      SELECT SINGLE * FROM mara INTO wl_mara WHERE matnr = <itens>-matnr.
    ENDIF.

    IF wl_mara-matkl EQ '700140' AND l_lifnr IS NOT INITIAL.
      SELECT SINGLE * FROM zsdt0108 INTO wa_zsdt0108 WHERE terminal EQ l_lifnr.
      IF sy-subrc IS INITIAL.

        lv_value = wa_zsdt0108-lgort.
        CLEAR pular_lgort.
        CALL METHOD pr_data_changed->modify_cell
          EXPORTING
            i_row_id    = ps_good_lifnr-row_id
            i_fieldname = 'LGORT'
            i_value     = lv_value.
        pular_lgort = abap_true.
      ENDIF.
    ENDIF.

    CASE wl_mara-matkl.
      WHEN '700110' OR '700170'.
        IF <itens>-lgort IS INITIAL.
          SELECT SINGLE * FROM zsdt0108 INTO wa_zsdt0108 WHERE terminal EQ l_lifnr.
        ELSE.
          SELECT SINGLE * FROM zsdt0108 INTO wa_zsdt0108 WHERE terminal EQ l_lifnr AND lgort EQ <itens>-lgort.
        ENDIF.

        IF NOT sy-subrc IS INITIAL.
          SHIFT l_lifnr LEFT DELETING LEADING '0'.
          CALL METHOD pr_data_changed->add_protocol_entry
            EXPORTING
              i_msgid     = '0K'
              i_msgno     = '000'
              i_msgty     = 'E'
              i_msgv1     = TEXT-003
              i_msgv2     = l_lifnr
              i_msgv3     = <itens>-lgort
              i_msgv4     = TEXT-004
              i_fieldname = ps_good_lifnr-fieldname
              i_row_id    = ps_good_lifnr-row_id.

          error_in_data = abap_true.

        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "CHECK_LIFNR

  METHOD: check_lgort.

    DATA: l_lgort     TYPE lgort_d,
          wa_zsdt0108 TYPE zsdt0108,
          ls_good     TYPE lvc_s_modi.

    CALL METHOD pr_data_changed->get_cell_value
      EXPORTING
        i_row_id    = ps_good_lgort-row_id
        i_fieldname = ps_good_lgort-fieldname
      IMPORTING
        e_value     = l_lgort.

    READ TABLE tg_itens ASSIGNING <itens> INDEX ps_good_lgort-row_id.
    IF <itens>-terminal IS INITIAL.
      SELECT SINGLE * FROM zsdt0108 INTO wa_zsdt0108 WHERE lgort EQ l_lgort.
    ELSE.
      SELECT SINGLE * FROM zsdt0108 INTO wa_zsdt0108 WHERE lgort EQ l_lgort AND terminal EQ <itens>-terminal.
    ENDIF.

    IF NOT sy-subrc IS INITIAL.

      CALL METHOD pr_data_changed->add_protocol_entry
        EXPORTING
          i_msgid     = '0K'
          i_msgno     = '000'
          i_msgty     = 'E'
          i_msgv1     = TEXT-003
          i_msgv2     = <itens>-terminal
          i_msgv3     = l_lgort
          i_msgv4     = TEXT-004
          i_fieldname = ps_good_lgort-fieldname
          i_row_id    = ps_good_lgort-row_id.

      error_in_data = abap_true.
    ENDIF.

*    READ TABLE IT_SAIDA ASSIGNING <SAIDA> INDEX PS_GOOD-ROW_ID.
*    MOVE WA_T001L-LGOBE TO <SAIDA>-DESC_D.
*
*    CALL METHOD PR_DATA_CHANGED->MODIFY_CELL( I_ROW_ID = PS_GOOD-ROW_ID
*      I_TABIX = PS_GOOD-TABIX
*      I_FIELDNAME = 'DESC_D'
*      I_VALUE = <SAIDA>-DESC_D ).

  ENDMETHOD.                    "CHECK_LGORT


  METHOD on_close.
    IF NOT sender IS INITIAL.
      CALL METHOD sender->free
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0.
*      ERROR HANDLING
      ENDIF.
      FREE obg_dialogbox.
      CLEAR obg_dialogbox.
    ENDIF.
  ENDMETHOD.                    "on_close
* Método de  execução para Duplo-click
  METHOD on_double_click.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD handle_button_click.
    CHECK es_col_id EQ 'LOTES_VOL'.

    TRY .
        w_click = tg_itens[ es_row_no-row_id ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    CHECK wg_header-nro_sol_ov IS NOT INITIAL.
    CHECK w_click-posnr IS NOT INITIAL.

    IF w_click-instrucao_ant IS NOT INITIAL. " Alterado 28/11/2019 CS2019001087

      DATA(r_lgort) =
      VALUE zrsdsselopts( FOR ls IN tg_instrucao
      WHERE ( instrucao EQ w_click-instrucao_ant )
      (
        option = 'EQ'
        sign = 'I'
        low = ls-charg
      ) ).

    ENDIF.

*    SELECT *
*      FROM ZSDT0213
*      INTO CORRESPONDING FIELDS OF TABLE IT_0213
*        WHERE NRO_SOL_OV EQ WG_HEADER-NRO_SOL_OV
*          AND POSNR EQ W_CLICK-POSNR
*          AND LGORT IN R_LGORT
*          AND STATUS EQ ABAP_FALSE.

    t_0213 = it_0213.
    DELETE t_0213 WHERE posnr NE w_click-posnr.
    DELETE t_0213 WHERE lgort NOT IN r_lgort.

    it_0213_aux_old = VALUE #( FOR ls_0213 IN t_0213 ( CORRESPONDING #( ls_0213 ) ) ).

    t_0213_orig[] = t_0213[]. "*-CS2023000189-#126959-07.11.2023-JT-inicio

    CALL SCREEN 0301 ENDING AT 60 10 STARTING AT 3 3.

  ENDMETHOD.

  METHOD on_hotspot_100.
    IF e_row_id-index GT 0.
      IF e_column_id-fieldname EQ 'VBELN'.
        READ TABLE it_zsdt0100 INTO wa_zsdt0100 INDEX e_row_id-index.
        SET PARAMETER ID 'AUN' FIELD wa_zsdt0100-vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  "ON_DOUBLE_CLICK
* Método de  execução para hotspot
  METHOD on_hotspot_click.
    DATA: wl_itens TYPE ty_itens.
    DATA: wl_pgto_ant LIKE LINE OF tg_pgt_ant.
    DATA(obj_auart) = NEW zcl_taxa_curva( ).
    DATA: r_comp TYPE RANGE OF auart.
    DATA: r_devo_recu TYPE RANGE OF auart.

    CLEAR: wa_zsdt0100.

    IF e_row_id-index GT 0.
      IF e_column_id-fieldname EQ 'VBELN'.

        READ TABLE tg_itens INTO wl_itens INDEX e_row_id-index.
        CASE wl_itens-status.
          WHEN c_w OR c_y.

            r_comp = obj_auart->get_auart( 'ZHEDGECOMP' ). " Get SET de AUART de Complemento
            r_devo_recu = obj_auart->get_auart( 'ZHEDGEDEVO/RECU' ). " Get SET de AUART de Devolução/Recusa

            SELECT * FROM zsdt0100
              INTO TABLE it_zsdt0100
                WHERE nro_sol_ov EQ wg_header-nro_sol_ov
                  AND fixacao EQ wl_itens-fixacao
                  AND ( posnr EQ wl_itens-posnr OR posnr IS NULL )
                  AND status NE 'C'.

*            DELETE IT_ZSDT0100 WHERE STATUS EQ 'C'.


            IF it_zsdt0100 IS NOT INITIAL.
              IF wl_itens-status EQ c_w.
                DELETE it_zsdt0100 WHERE auart IN r_devo_recu.
*                DELETE IT_ZSDT0100 WHERE AUART EQ 'ZREB'.
*                DELETE IT_ZSDT0100 WHERE AUART EQ 'ZROB'.
              ELSEIF wl_itens-status EQ c_y.
                DELETE it_zsdt0100 WHERE auart IN r_comp.
*                DELETE IT_ZSDT0100 WHERE AUART EQ 'ZCPV'.
*                DELETE IT_ZSDT0100 WHERE AUART EQ 'ZCOP'.
              ENDIF.

              CALL SCREEN 0410 STARTING AT 80 9.
            ELSE.

              IF wl_itens-vbeln IS NOT INITIAL.

                " 26.11.2024 - 159383 - RAMON -->
                IF wg_header-auart = 'ZUB'.

                  SET PARAMETER ID 'BES' FIELD wl_itens-vbeln.

                  CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
                    EXPORTING
                      i_ebeln              = wl_itens-vbeln
                    EXCEPTIONS
                      not_found            = 1
                      no_authority         = 2
                      invalid_call         = 3
                      preview_not_possible = 4
                      OTHERS               = 5.

                  IF sy-subrc <> 0.
                  ENDIF.

                ELSE.

                  SET PARAMETER ID 'AUN' FIELD wl_itens-vbeln.
                  CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

                ENDIF.
                " 26.11.2024 - 159383 - RAMON --<

              ENDIF.
            ENDIF.

          WHEN OTHERS.

            IF wl_itens-vbeln IS NOT INITIAL.

              " 26.11.2024 - 159383 - RAMON -->
              IF wg_header-auart = 'ZUB'.

                SET PARAMETER ID 'BES' FIELD wl_itens-vbeln.

                CALL FUNCTION 'ME_DISPLAY_PURCHASE_DOCUMENT'
                  EXPORTING
                    i_ebeln              = wl_itens-vbeln
                  EXCEPTIONS
                    not_found            = 1
                    no_authority         = 2
                    invalid_call         = 3
                    preview_not_possible = 4
                    OTHERS               = 5.

                IF sy-subrc <> 0.
                ENDIF.

              ELSE.

                SET PARAMETER ID 'AUN' FIELD wl_itens-vbeln.
                CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

              ENDIF.
              " 26.11.2024 - 159383 - RAMON --<

            ENDIF.

        ENDCASE.

      ELSEIF e_column_id-fieldname EQ 'ADIANT'.

        READ TABLE tg_pgt_ant INTO wl_pgto_ant INDEX e_row_id-index.
        IF sy-subrc IS INITIAL  AND wl_pgto_ant-adiant IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wl_pgto_ant-adiant.
          SET PARAMETER ID 'BUK' FIELD wg_header-vkorg.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
* Método de  execução para hotspot
  METHOD on_hotspot_click_form.
    DATA: wl_form_lote TYPE ty_form_lote.
    IF e_row_id-index GT 0.
      READ TABLE tg_form_lote INTO wl_form_lote INDEX e_row_id-index.

      IF sy-subrc IS INITIAL
      AND wl_form_lote-vbeln IS NOT INITIAL.
        SET PARAMETER ID 'AUN' FIELD wl_form_lote-vbeln.
** Chamo a transação
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

      ENDIF.

    ENDIF.

  ENDMETHOD.                    "on_hotspot_click_form

  METHOD on_data_changed.

    DATA: ls_good               TYPE lvc_s_modi,
          ls_good_aux           TYPE lvc_s_modi,
          lv_value              TYPE lvc_value,
          lv_value_aux          TYPE lvc_value,
          vl_tabix              TYPE sy-tabix,
          vl_value              TYPE lvc_value,
          wl_mara               TYPE mara,
          wl_makt               TYPE makt,
          wl_dmbtr              TYPE bsid-dmbtr,
          wl_posnr              TYPE zsdt0053-posnr,
          wl_vlrtot             TYPE bsid-dmbtr,
          wl_zmeng              TYPE zsdt0053-zmeng,
          wl_preco              LIKE LINE OF tg_preco,
          wl_itens              LIKE LINE OF tg_itens,
          wl_itens_aux          LIKE LINE OF tg_itens,
          wl_atualiza_logistica,
          wl_matnr              TYPE mara-matnr,
          wl_zieme              TYPE mara-meins,
          wl_pmein              TYPE mara-meins,
          wl_menge              TYPE ekpo-menge,
          p_2(13)               TYPE p DECIMALS 2,
          p_5(13)               TYPE p DECIMALS 5,
          p_4(13)               TYPE p DECIMALS 4,
          f_g                   TYPE zsded038,
          tl_good_cells         TYPE lvc_t_modi,
          wl_premio             TYPE dmbtr.

    DATA: obg_event TYPE REF TO lcl_event_handler.
    CREATE OBJECT obg_event.
    CREATE OBJECT obj_frete.

    REFRESH: tl_good_cells.

    UNASSIGN <fs_line>.

    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.

    CLEAR: wl_atualiza_logistica.
    DATA: it_mp_mod TYPE tty_itens. "#113631-18.01.2024-JT-inicio
    FIELD-SYMBOLS: <mr>     TYPE tty_itens,
                   <mp_mod> TYPE tty_mp_mod, "#113631-18.01.2024-JT-inicio
                   <tr>     TYPE ty_itens.
    ASSIGN er_data_changed->mp_mod_rows->* TO <mr>.

*    TRY .
*        DATA(_FIELDNAME) = ER_DATA_CHANGED->MT_GOOD_CELLS[ 1 ]-FIELDNAME.
*      CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*    ENDTRY.

    IF wg_header-param_espec NE c_m.
      PERFORM retorna_preco_item USING '1' space
                              CHANGING wl_dmbtr.
    ENDIF.

    " 18.03.2025 - RAMON -->

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                                WHERE fieldname = 'PMEIN'.

      IF ls_good-value IS NOT INITIAL.

        READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<fs_item1>) INDEX ls_good-row_id.

        CHECK sy-subrc EQ 0.

        DATA(lt_unidade) = zcl_material=>get_instance( )->get_zsdt0371_table_by_matnr( <fs_item1>-matnr ).

        READ TABLE lt_unidade TRANSPORTING NO FIELDS
          WITH KEY mseh3 = ls_good-value.

        IF sy-subrc NE 0.

          MESSAGE |Unidade de medida: { ls_good-value } não encontrada| TYPE 'S' DISPLAY LIKE 'E'.

          CLEAR: <fs_item1>-pmein, lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'PMEIN'
              i_value     = lv_value.

          EXIT.

        ENDIF.
*
*        CLEAR: <fs_item>-terminal, lv_value.
*
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'TERMINAL'
*            i_value     = lv_value.

      ENDIF.

    ENDLOOP.
    " 18.03.2025 - RAMON --<


    error_in_data = abap_false.
    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                            WHERE fieldname = 'TERMINAL' OR fieldname = 'LGORT'.

      CASE ls_good-fieldname.
        WHEN 'TERMINAL'.

          IF ls_good-value IS NOT INITIAL.
            obg_event->check_lifnr( ps_good_lifnr = ls_good pr_data_changed = er_data_changed ).
          ENDIF.

          IF wg_header-param_espec EQ c_p.

            READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX ls_good-row_id.

            CHECK sy-subrc EQ 0.

            CALL METHOD er_data_changed->get_cell_value
              EXPORTING
                i_row_id    = ls_good-row_id
                i_tabix     = ls_good-tabix
                i_fieldname = ls_good-fieldname
              IMPORTING
                e_value     = lv_value.

            CLEAR: <fs_item>-porto, <fs_item>-codigo_ra.
            IF ( lv_value IS INITIAL ) OR ( lv_value EQ '0000000000' ).
              CLEAR: <fs_item>-terminal.
            ELSE.

              SELECT SINGLE *
                FROM zsdt0168 INTO @DATA(lwa_zsdt0168)
               WHERE lifnr EQ @lv_value.

              IF sy-subrc EQ 0.

                SELECT SINGLE *
                  FROM zsdt0169 INTO @DATA(lwa_zsdt0169)
                 WHERE codigo_ra EQ @lwa_zsdt0168-codigo_ra.

                IF sy-subrc EQ 0.
                  SELECT SINGLE *
                    FROM zsdt0167 INTO @DATA(lwa_zsdt0167)
                   WHERE codigo_urf EQ @lwa_zsdt0169-codigo_urf.

                  IF sy-subrc EQ 0.
                    <fs_item>-codigo_ra = lwa_zsdt0168-codigo_ra.
                    <fs_item>-porto     = lwa_zsdt0167-ds_urf.
                  ENDIF.
                ENDIF.

              ENDIF.

              IF  <fs_item>-porto IS INITIAL .
                MESSAGE |Recinto não encontrado para o fornecedor: { lv_value } na transação ZSDT0137!| TYPE 'S'.
                CLEAR: <fs_item>-terminal, lv_value.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'TERMINAL'
                    i_value     = lv_value.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN 'LGORT'.
*          OBG_EVENT->CHECK_LGORT( PS_GOOD_LGORT = LS_GOOD PR_DATA_CHANGED = ER_DATA_CHANGED ).
      ENDCASE.

    ENDLOOP.
    IF error_in_data EQ abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

*-#113631-18.01.2024-JT-inicio
* RJF - Ini - 2023.09.13 - #113631 - CS2023000420 Incluir colunas de drawback na ZSDT0062 para exportação de algodão
    it_mp_mod = <mr>.
    LOOP AT it_mp_mod INTO DATA(ls_good1).
      READ TABLE tg_itens INTO DATA(ls_itens) WITH KEY posnr = ls_good1-posnr.
      DATA(lv_tabix) = sy-tabix.
      IF sy-subrc IS INITIAL AND ls_itens-tp_ato NE ls_good1-tp_ato.
        ls_itens-tp_ato = ls_good1-tp_ato.
        MODIFY tg_itens FROM ls_itens INDEX lv_tabix TRANSPORTING tp_ato.
      ENDIF.
    ENDLOOP.
* RJF - Fim - 2023.09.13 - #113631 - CS2023000420 Incluir colunas de drawback na ZSDT0062 para exportação de algodão
*-#113631-18.01.2024-JT-fim

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MATNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'MATNR'
        IMPORTING
          e_value     = lv_value_aux.


      SELECT SINGLE maktx
        FROM makt
        INTO lv_value
         WHERE matnr EQ lv_value
        AND spras EQ sy-langu.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: lv_value.
      ENDIF.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MAKTX'
          i_value     = lv_value.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wl_mara-matnr = lv_value.
      SELECT SINGLE meins
        FROM mara
        INTO lv_value
         WHERE matnr EQ lv_value.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: lv_value.
      ENDIF.

*      IF wl_mara-matnr NE LV_VALUE_AUX.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'ZIEME'
          i_value     = lv_value.

      IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'FIXACAO'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_posnr = lv_value_aux.

        PERFORM retorna_preco_item USING   '1'
                                         wl_posnr
                                CHANGING wl_dmbtr.
      ENDIF.
      IF wg_header-param_espec EQ c_p.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'VLRTOT'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_dmbtr = lv_value_aux.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'ZMENG'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_menge = lv_value_aux.

        TRY .
            DIVIDE wl_dmbtr BY wl_menge.
          CATCH cx_sy_zerodivide .

        ENDTRY.
        lv_value = wl_dmbtr.
        CONDENSE lv_value NO-GAPS.
      ELSE.
        lv_value = wl_dmbtr.
        CONDENSE lv_value NO-GAPS.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DMBTR'
          i_value     = lv_value.
    ENDLOOP.

    DATA: var_len         TYPE sy-tabix,
          var_tabix       TYPE sy-tabix,
          var_temp        TYPE sy-tabix,
          var_up          TYPE sy-tabix,
          var_dife        TYPE zsdt0053-zmeng,
          var_valor_frame TYPE p LENGTH 7 DECIMALS 3,
          var_valor_char  TYPE c LENGTH 128,
          var_valor_c_p   TYPE c LENGTH 128,
          var_negativa    TYPE i,
          var_taxa        TYPE c LENGTH 30.

    DATA: var_offset TYPE i.

    IF it_block IS INITIAL.
      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR wg_valor.
        PERFORM get_set_valores USING 'BEZEI'       'G' CHANGING wg_valor.
        IF wg_valor IN r_bezei.
          CLEAR wg_valor.
          PERFORM get_set_valores USING 'FIELD'       'G' CHANGING wg_valor.
          IF wg_valor EQ 'QTDFIXADA'.
            CLEAR wg_valor.
            PERFORM get_set_valores USING 'POSNR1'      'G' CHANGING wg_valor.
            IF wg_valor NE 0.

              CLEAR wg_valor. PERFORM get_set_valores USING 'QTDFIXADA'   'G' CHANGING wg_valor. wa_block-qtdfixada   = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'BEZEI'       'G' CHANGING wg_valor. wa_block-bezei       = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'POSNR1'      'G' CHANGING wg_valor. wa_block-posnr1      = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'VALDT'       'G' CHANGING wg_valor. wa_block-valdt       = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'VALDT_HEDGE' 'G' CHANGING wg_valor. wa_block-valdt_hedge = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'NIVEL'       'G' CHANGING wg_valor. wa_block-nivel       = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'COD_FP'      'G' CHANGING wg_valor. wa_block-cod_fp      = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'FIELD'       'G' CHANGING wg_valor. wa_block-field       = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'TIPO_CALC'   'G' CHANGING wg_valor. wa_block-tipo_calc   = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'POSNR'       'G' CHANGING wg_valor. wa_block-posnr       = wg_valor.
              CLEAR wg_valor. PERFORM get_set_valores USING 'ITEM_KEY' 	  'G' CHANGING wg_valor. wa_block-item_key    = wg_valor.
              APPEND wa_block TO it_block.
            ENDIF.

          ENDIF.

        ENDIF.


      ENDLOOP.
    ENDIF.


*    L_ITENS = LINES( TG_ITENS ).
    IF wg_redistribuir IS NOT INITIAL.

      IF it_var_guar IS NOT INITIAL.

        IF lines( tg_itens ) NE lines( it_var_guar ).
          LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'ZMENG'.
*            READ TABLE TG_ITENS INTO WL_ITENS INDEX LS_GOOD-ROW_ID.

            LOOP AT tg_itens INTO wl_itens WHERE item_edit EQ 'R' AND fixacao IS NOT INITIAL.

              READ TABLE it_var_guar TRANSPORTING NO FIELDS WITH KEY posnr = wl_itens-posnr.

              IF sy-subrc IS NOT INITIAL
               AND wl_itens-fixacao IS NOT INITIAL.

                wa_var_guar-posnr   = wl_itens-posnr.
                wa_var_guar-fixacao = wl_itens-fixacao.
                wa_var_guar-zmeng   = wl_itens-zmeng.

                " 21.03.2025 - RAMON -->
                wa_var_guar-charg_old = wl_itens-charg.
                wa_var_guar-charg = wl_itens-charg.
                " 21.03.2025 - RAMON --<

                APPEND wa_var_guar TO it_var_guar.
              ENDIF.

            ENDLOOP.

          ENDLOOP.

        ENDIF.
      ENDIF.


      LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'ZMENG'.

        "  Guarda os Zmeng para realizar o Calculo do Preço.
        IF ( wg_header-param_espec EQ 'M' OR wg_header-param_espec  EQ 'Z').

          IF it_var_guar IS INITIAL.

            LOOP AT tg_itens INTO wl_itens.

              IF wl_itens-fixacao IS NOT INITIAL.

                " 29.10.2024 - 147331 - RAMON -->
                CLEAR wa_var_guar.
                " 29.10.2024 - 147331 - RAMON --<

                wa_var_guar-posnr   = wl_itens-posnr.
                wa_var_guar-fixacao = wl_itens-fixacao.
                wa_var_guar-zmeng   = wl_itens-zmeng.

                " 21.03.2025 - RAMON -->
                wa_var_guar-charg_old = wl_itens-charg.
                wa_var_guar-charg = wl_itens-charg.
                " 21.03.2025 - RAMON --<

                APPEND wa_var_guar TO it_var_guar.

              ENDIF.

            ENDLOOP.

          ENDIF.

          READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

          " 29.10.2024 - 147331 - RAMON -->
          IF sy-subrc EQ 0.

            READ TABLE it_var_guar INTO wa_var_guar WITH KEY posnr = wl_itens-posnr.

            " 28.10.2024 - 147331 - RAMON -->
            IF sy-subrc EQ 0.

              IF wa_var_guar-guar IS INITIAL.

                wa_var_guar-guar    = 'X'.
                MODIFY it_var_guar FROM wa_var_guar TRANSPORTING guar WHERE posnr = wl_itens-posnr.
              ENDIF.

              " 20.09.2024 - 147331 - RAMON -->
              PERFORM f_guarda_saldo_fix USING wl_itens-posnr ls_good-value.
              " 20.09.2024 - 147331 - RAMON --<

            ENDIF.
            " 28.10.2024 - 147331 - RAMON --<

          ENDIF.

        ENDIF." 28.10.2024 - 147331 - RAMON --<


      ENDLOOP.

      " 07.04.2025 - RAMON -->
      PERFORM f_atualiza_lote USING er_data_changed.
      " 07.04.2025 - RAMON --<


    ENDIF.

    IF wg_acao EQ c_modif_qtd AND wg_redistribuir IS INITIAL.

      LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'ZMENG'.
*  Guarda os Zmeng para realizar o Calculo do Preço.
        IF NOT ( var_modred IS INITIAL ) AND ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z' ).
          IF it_var_guar IS INITIAL.
            LOOP AT tg_itens INTO wl_itens.
              IF wl_itens-fixacao IS NOT INITIAL.

                wa_var_guar-posnr   = wl_itens-posnr.
                wa_var_guar-fixacao = wl_itens-fixacao.
                wa_var_guar-zmeng   = wl_itens-zmeng.
                APPEND wa_var_guar TO it_var_guar.

              ENDIF.

            ENDLOOP.
          ENDIF.

          READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
          READ TABLE it_var_guar INTO wa_var_guar WITH KEY posnr = wl_itens-posnr.
          IF wa_var_guar-guar IS INITIAL.

            wa_var_guar-guar    = 'X'.
            MODIFY it_var_guar FROM wa_var_guar TRANSPORTING guar WHERE posnr = wl_itens-posnr.
          ENDIF.

        ENDIF.
      ENDLOOP.
    ENDIF.
*Fim Welgem

    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'ZMENG'.

      "Fazer o calculo para o FRAME, quando for modificação.
      IF NOT ( var_modred IS INITIAL ) AND ( wg_header-param_espec EQ 'M' OR   wg_header-param_espec EQ 'Z'  ).

        READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
        LOOP AT <fs_table> INTO <fs_line>.

          PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.

          IF ( wl_itens-fixacao NE  wg_valor ).
            CONTINUE.
          ENDIF.

          PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

          var_len = strlen( wg_valor ).
          IF ( var_len EQ 2 ) OR ( var_len EQ 3 ) AND ( wg_valor(1) EQ 'T' ).

            CLEAR: wg_valor.
            PERFORM get_set_valores USING 'QTDFIXADA' 'G' CHANGING wg_valor.

            REPLACE REGEX '[,]' IN wg_valor WITH ''.
            REPLACE REGEX '[.]' IN wg_valor WITH ''.

            CLEAR: wl_premio.
            wl_premio = wg_valor.

            IF ( wl_premio NE 0 ).
              var_tabix = sy-tabix.
            ENDIF.
          ELSEIF ( wg_valor EQ 'TAXA CAMBIO FRAME' ).

            CLEAR: var_taxa.
            PERFORM get_set_valores USING 'PRECO' 'G' CHANGING var_taxa.

          ENDIF.
          CLEAR: var_len, wg_valor.
        ENDLOOP.
      ENDIF.


      wl_atualiza_logistica = c_x.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wl_zmeng = lv_value.

      IF NOT ( var_tabix IS INITIAL ).


        "var_tabix = var_tabix + 1." 22.02.2024 - RAMON - #estava dando dump por causa dessa linha

*        VAR_DIFE =  WL_ZMENG - WL_ITENS-ZMENG.
        LOOP AT  it_var_guar INTO wa_var_guar WHERE posnr EQ wl_itens-posnr
                                              AND fixacao EQ wl_itens-fixacao.

          var_dife =  wl_zmeng - wa_var_guar-zmeng.

        ENDLOOP.

        CLEAR erro_59.

        " 19.02.2024 - RAMON -->
        IF wg_redistribuir = abap_true AND lv_redist = abap_true.

          DATA(lv_lines) = lines( <fs_table> ).

          IF var_tabix >= lv_lines.

            " 22.10.2024 - 154003 - RAMON -->
            UNASSIGN <fs_line>.
            " 22.10.2024 - 154003 - RAMON --<

            LOOP AT <fs_table> ASSIGNING <fs_line>.

              lv_lines = sy-tabix.

              PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

              " SE VIM ATE AQUI, PEGA O PRIMEIRO INDICE DO C1, APENAS PARA
              " ENTRAR NOS CODIGOS ABAIXO ( SEM ISSO NAO ENTRA QUANDO NAO ACHA INDICE )
              IF ( wg_valor EQ 'C1' ).
                EXIT.
              ELSE.
                CONTINUE.
              ENDIF.

            ENDLOOP.

            var_tabix = lv_lines.

          ENDIF.

        ENDIF.
        " 19.02.2024 - RAMON --

        obj_frete->check_estouro(
          EXPORTING
            fixacao  = wl_itens-fixacao
            fs_table = <fs_table>
            c_table  = c_bezei
            p_table  = p_bezei
            t_table  = r_bezei
            n_table  = n_bezei
            b_table  = b_bezei " 29.10.2024 - 147331 - RAMON -
          IMPORTING
            erro_59  = erro_59 ).

        IF erro_59 CS 'E_T'.
        ELSE.

          " 2024 - RAMON -->
          ADD 1 TO gv_ultimo_nvl.
          " 2024 - RAMON --<

          " 14.04.2025 - RAMON -- 166561 -->
          " nova forma de atribuir valores ao preço

          IF 1 = 1.

            " 25.04.2025 - RAMON -- 166561 -->

            " redistribuir passa valor inteiro
            IF wg_redistribuir = abap_true.
              WRITE wl_zmeng TO var_valor_c_p LEFT-JUSTIFIED.

              " Quando for modif qtde tem que passar a diferença
            ELSE.
              WRITE var_dife TO var_valor_c_p LEFT-JUSTIFIED.
            ENDIF.

            PERFORM update_bezei_2 TABLES tg_itens
                                      c_bezei
                                      p_bezei
                                      r_bezei
                                      n_bezei
                                      b_bezei " 29.10.2024 - 147331 - RAMON -
                               USING  ls_good-row_id
                                      var_valor_c_p
                                      " 21.03.2025 - RAMON - 166561 -->
                                      'T'
                                      lv_redist.

            " forma antiga, caso precise voltar codigo
          ELSE.

            " 22.10.2024 - 154003 - RAMON -->
            UNASSIGN <fs_line>.
            " 22.10.2024 - 154003 - RAMON --<

            LOOP AT <fs_table> ASSIGNING <fs_line>.

              IF ( sy-tabix EQ var_tabix ).

*              PERFORM GET_SET_VALORES USING 'BEZEI' 'G' CHANGING WG_VALOR.
*              IF WG_VALOR IN R_BEZEI.

                PERFORM get_set_valores USING 'QTDFIXADA'
                                              'G' CHANGING wg_valor.

                REPLACE REGEX '[.]' IN wg_valor WITH ''.

                FIND ',' IN wg_valor RESPECTING CASE MATCH OFFSET var_offset.

                CLEAR: var_valor_frame.

                FIND '-' IN wg_valor RESPECTING CASE MATCH OFFSET var_negativa.

                IF ( var_negativa EQ 0 ).
                  var_valor_frame = wg_valor(var_offset).
                ELSE.
                  var_valor_frame = wg_valor(var_offset).
                  var_valor_frame = var_valor_frame * -1.
                ENDIF.

                " 07.12.2023 - 128467 - RBL --> #duvida nao sei se esta certo comentar esse trecho
                "var_dife = var_valor_frame  + var_dife.
                " 07.12.2023 - 128467 - RBL --<
                var_valor_char = var_dife.

                CLEAR: var_offset.
                FIND '.' IN var_valor_char RESPECTING CASE MATCH OFFSET var_offset.

                var_valor_char = var_valor_char(var_offset).

                IF ( var_dife < 0 ).
                  CONDENSE var_valor_char NO-GAPS.
                  CONCATENATE var_valor_char '-' INTO var_valor_char.

                ELSE.
                  CONDENSE var_valor_char NO-GAPS.

                ENDIF.

                " 07.12.2023 - 128467 - RBL -->
                WRITE var_dife TO var_valor_char LEFT-JUSTIFIED.
                " 07.12.2023 - 128467 - RBL --<

                var_valor_c_p = var_valor_char.
                CONDENSE var_valor_c_p NO-GAPS.

                PERFORM get_set_valores USING   'QTDFIXADA'
                                                'S'
                                       CHANGING var_valor_char.

*              PERFORM GET_SET_VALORES USING   'QTDFIXADA'
*                                              'S'
*                                     CHANGING F_G.


                PERFORM get_set_valores USING   'PRECO'
                                                'S'
                                       CHANGING var_taxa.

                PERFORM get_set_valores USING   'POSNR1'
                                                'S'
                                       CHANGING wl_itens-posnr.

                CLEAR: var_valor_char.
                var_valor_char = '00000000'.
                CONDENSE var_valor_char NO-GAPS.

                PERFORM get_set_valores USING   'VALDT_HEDGE'
                                                'S'
                                       CHANGING var_valor_char.

                PERFORM get_set_valores USING   'VALDT'
                                                'S'
                                       CHANGING sy-datum.

*       Verifica se o Valor X é preenchido e armazena as informação na linha com o VALDT_HEDGE INITIAL
*       senão continua o processo normal.
*            BREAK ABAP.
*            IF ANT_VAR EQ 'X'.
*              MODIFY <FS_TABLE> FROM <FS_LINE> INDEX VAR_TABIX.
*            ELSE.
*            BREAK-POINT.

                " 11.12.2023 - 128467 - RBL -->

*              MODIFY <fs_table> FROM <fs_line> INDEX var_tabix.
*
*              PERFORM update_c_p TABLES tg_itens
*                                        c_bezei
*                                        p_bezei
*                                 USING  ls_good-row_id
*                                        var_valor_c_p.

                "MODIFY <fs_table> FROM <fs_line> INDEX var_tabix.

                PERFORM update_bezei TABLES tg_itens
                                          c_bezei
                                          p_bezei
                                          r_bezei
                                          n_bezei
                                          b_bezei " 29.10.2024 - 147331 - RAMON -
                                   USING  ls_good-row_id
                                          var_valor_c_p
                                          " 21.03.2025 - RAMON - 166561 -->
                                          'T'
                                          lv_redist.
                " 21.03.2025 - RAMON - 166561 --<

                " 11.12.2023 - 128467 - RBL --<

                CLEAR: var_valor_c_p.
*            ENDIF.
                CLEAR ant_var.
*              ELSE.
**              <ERRO_59> = 'X'.
*                MESSAGE S836(SD) DISPLAY LIKE 'E' WITH text-M31 .
*              ENDIF.

              ENDIF.

            ENDLOOP.

          ENDIF.
          " 14.04.2025 - RAMON -- 166561 --<

        ENDIF.
*        ENDIF.

      ENDIF.
      CLEAR: wl_itens,
             var_len        ,
             var_tabix      ,
             var_dife       ,
             var_valor_frame ,
             var_valor_char ,
             var_negativa   .


      IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'FIXACAO'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_posnr = lv_value_aux.

        PERFORM retorna_preco_item USING   '1'
                                         wl_posnr
                                CHANGING wl_dmbtr.
      ELSE.
        PERFORM retorna_preco_item USING   '1'
                                         space
                                CHANGING wl_dmbtr.
      ENDIF.

      IF wg_header-param_espec EQ c_p.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'VLRTOT'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_dmbtr = lv_value_aux.

        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'ZMENG'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_menge = lv_value_aux.

        TRY .
            DIVIDE wl_dmbtr BY wl_menge.
          CATCH cx_sy_zerodivide .

        ENDTRY.
        CLEAR: wl_menge.
        lv_value = wl_dmbtr.
        CONDENSE lv_value NO-GAPS.
      ELSE.
        lv_value = wl_dmbtr.
        CONDENSE lv_value NO-GAPS.
      ENDIF.
*      LV_VALUE = WL_DMBTR.

      " 01.12.2023 - 128467 - RBL -->
*      CALL METHOD er_data_changed->modify_cell
*        EXPORTING
*          i_row_id    = ls_good-row_id
*          i_fieldname = 'DMBTR'
*          i_value     = lv_value.
      " 01.12.2023 - 128467 - RBL --<

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'MATNR'
        IMPORTING
          e_value     = lv_value_aux.
      CONDENSE lv_value_aux NO-GAPS.
      wl_matnr = lv_value_aux.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'ZIEME'
        IMPORTING
          e_value     = lv_value_aux.
      CONDENSE lv_value_aux NO-GAPS.
      wl_zieme = lv_value_aux.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'PMEIN'
        IMPORTING
          e_value     = lv_value_aux.
      CONDENSE lv_value_aux NO-GAPS.
      wl_pmein = lv_value_aux.
      wl_menge = wl_zmeng.
      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = wl_matnr
          i_in_me              = wl_zieme
          i_out_me             = wl_pmein
          i_menge              = wl_menge
        IMPORTING
          e_menge              = wl_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      IF sy-subrc EQ 0.
        wl_zmeng = wl_menge.
      ENDIF.

      IF wg_header-param_espec NE c_p.
        TRY .
            wl_vlrtot = wl_dmbtr * wl_zmeng.
          CATCH cx_sy_arithmetic_overflow.
            wl_vlrtot = 0.
        ENDTRY.

        lv_value = wl_vlrtot.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLRTOT'
            i_value     = lv_value.

      ELSE.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'VLRTOT'
          IMPORTING
            e_value     = lv_value_aux.
        CONDENSE lv_value_aux NO-GAPS.
        wl_vlrtot = lv_value_aux.

        TRY.
            wl_dmbtr = wl_vlrtot / wl_zmeng.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        lv_value = wl_dmbtr.
        CONDENSE lv_value NO-GAPS.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'DMBTR'
            i_value     = lv_value.

      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'VALDT'
                                OR fieldname = 'WERKS'
                                OR fieldname = 'LGORT'
                                OR fieldname = 'CHARG'
                                OR fieldname = 'ZIEME'
                                OR fieldname = 'PMEIN'
                                OR fieldname = 'DMBTR'
                                OR fieldname = 'KUNNR'
                                OR fieldname = 'FIXACAO'
                                OR fieldname = 'VLRTOT'.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = ls_good-fieldname
        IMPORTING
          e_value     = lv_value.

      IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z
      AND ( ls_good-fieldname EQ 'DMBTR'
         OR ls_good-fieldname EQ 'CHARG' ).
        CONTINUE.
      ENDIF.

      IF pular_lgort IS INITIAL.
        lv_value = ls_good-value.
      ENDIF.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = ls_good-fieldname "'VALDT'
          i_value     = lv_value.

      IF ls_good-fieldname EQ 'ZIEME'
      OR ls_good-fieldname EQ 'PMEIN'.

        READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

        CASE ls_good-fieldname.
          WHEN 'PMEIN'.
            wl_itens-pmein = ls_good-value.
          WHEN 'ZIEME'.
            wl_itens-zieme = ls_good-value.
        ENDCASE.

        TRY.
            IF wg_header-param_espec EQ c_p.
              IF wl_itens-pmein = 'TO' AND wl_itens-zieme = 'KG'.
                wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) * 1000.       "Campo: Qtd.Prev.
              ELSEIF wl_itens-pmein = 'KG' AND    wl_itens-zieme = 'TO'.
                wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) / 1000.       "Campo: Qtd.Prev.

                " 02.09.2024 - RAMON --> bug 150168
                "QUANDO RISCO SACADO
*              ELSEIF wl_itens-pmein = 'L' AND wl_itens-zieme = 'M3'.
*                wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) * 1000.       "Campo: Qtd.Prev.
*              ELSEIF wl_itens-pmein = 'M3' AND    wl_itens-zieme = 'L'.
*                wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) / 1000.       "Campo: Qtd.Prev.

                " 02.09.2024 - RAMON --> bug 150168

              ELSE.
                wl_itens-zmeng  = wl_itens-vlt_porto / wl_itens-p_porto.                  "Campo: Qtd.Prev.
              ENDIF.
            ENDIF.
          CATCH cx_sy_zerodivide.
            MESSAGE TEXT-m07 TYPE 'S'."'Valor inválido'
        ENDTRY.
        MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING zmeng.

      ENDIF.

      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDLOOP.


*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = _FIELDNAME.
*      IF _FIELDNAME EQ 'DESC_ABSOLUTO' OR
*         _FIELDNAME EQ 'ZMENG' OR
*         _FIELDNAME EQ 'VLRTOT'.
*        OBG_EVENT->CALCULO_DESCONTO( ER_DATA  = ER_DATA_CHANGED FIELDNAME = LS_GOOD-FIELDNAME ).
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'DESC_ABSOLUTO'.
*      OBG_EVENT->CALCULO_DESCONTO( ER_DATA  = ER_DATA_CHANGED FIELDNAME = LS_GOOD-FIELDNAME ) .
*    ENDLOOP.
*
*    LOOP AT ER_DATA_CHANGED->MT_GOOD_CELLS
*                             INTO LS_GOOD
*                             WHERE FIELDNAME = 'ZMENG'.
*      OBG_EVENT->CALCULO_DESCONTO( ER_DATA  = ER_DATA_CHANGED FIELDNAME = LS_GOOD-FIELDNAME ) .
*    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'VALDT'.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = ls_good-fieldname
        IMPORTING
          e_value     = lv_value.

      IF ( lv_value < sy-datum ).
        CLEAR: lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = ls_good-fieldname "'VALDT'
            i_value     = lv_value.

        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m08."'A data não pode ser menor que a data atual'.
      ENDIF.


      IF ( wg_header-param_espec EQ 'M' OR  wg_header-param_espec EQ 'Z'  ) AND ( wg_redistribuir IS INITIAL ).

        READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

        LOOP AT <fs_table> INTO <fs_line>.

          var_tabix = sy-tabix.

          PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.

          IF ( wl_itens-fixacao NE  wg_valor ).
            CONTINUE.
          ENDIF.

          PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

          var_len = strlen( wg_valor ).
          IF ( var_len EQ 2 ) OR ( var_len EQ 3 ).

            IF (  wg_valor(1) EQ 'T' ).
              CLEAR: var_valor_char.
              var_valor_char = '00000000'.
              CONDENSE var_valor_char NO-GAPS.

              PERFORM get_set_valores USING   'VALDT_HEDGE' 'S' CHANGING var_valor_char.

              MODIFY <fs_table> FROM <fs_line> INDEX var_tabix.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good
                                          WHERE fieldname = 'VLT_PORTO'
                                            OR  fieldname = 'P_PORTO'.
*
      CONDENSE ls_good-value NO-GAPS.

      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.

      IF ls_good-fieldname = 'VLT_PORTO'.
        wl_itens-vlt_porto = ls_good-value.
      ELSEIF ls_good-fieldname = 'P_PORTO'.
        wl_itens-p_porto = ls_good-value.
      ENDIF.

      CREATE DATA t_new_line LIKE LINE OF <fs_table>.
      ASSIGN t_new_line->* TO <fs_line>.

      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'PRECO_ITEM'
                                      'G'
                              CHANGING wg_valor.
        CONDENSE wg_valor NO-GAPS.
        IF wg_valor = 5.
          PERFORM get_set_valores USING 'PRECO'
                                        'G'
                                CHANGING wg_valor.
          REPLACE ALL OCCURRENCES OF ',' IN wg_valor WITH '.'.
          MOVE wg_valor TO wl_premio.
        ENDIF.
      ENDLOOP.

      TRY.
          IF wl_itens-pmein = 'TO' AND wl_itens-zieme = 'KG'.
            wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) * 1000.       "Campo: Qtd.Prev.
          ELSEIF wl_itens-pmein = 'KG' AND wl_itens-zieme = 'TO'.
            wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) / 1000.       "Campo: Qtd.Prev.

            " 02.09.2024 - RAMON --> bug 150168
            "QUANDO RISCO SACADO SIM
*        ELSEIF wl_itens-pmein = 'L' AND wl_itens-zieme = 'M3'.
*          wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) * 1000.       "Campo: Qtd.Prev.
*        ELSEIF wl_itens-pmein = 'M3' AND    wl_itens-zieme = 'L'.
*          wl_itens-zmeng  = ( wl_itens-vlt_porto / wl_itens-p_porto ) / 1000.       "Campo: Qtd.Prev.

            " 02.09.2024 - RAMON --< bug 150168





          ELSE."IF WL_ITENS-PMEIN = ''.
            wl_itens-zmeng  = wl_itens-vlt_porto / wl_itens-p_porto.                  "Campo: Qtd.Prev.
          ENDIF.

          wl_itens-vlrtot = wl_itens-vlt_porto + ( ( wl_itens-vlt_porto * wl_premio ) / 100 ).  "Campo: Valor Total
          wl_itens-dmbtr  = wl_itens-vlrtot / wl_itens-zmeng.                         "Campo: Preço

          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id TRANSPORTING zmeng vlrtot dmbtr.
        CATCH cx_sy_zerodivide.
          MESSAGE TEXT-m07 TYPE 'S'."'Valor inválido'
      ENDTRY.
    ENDLOOP.

    "US 73163 - Ini
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'ID_NOMEACAO_TRAN'.

      READ TABLE tg_itens ASSIGNING <fs_item> INDEX ls_good-row_id.

      CHECK sy-subrc EQ 0.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = ls_good-fieldname
        IMPORTING
          e_value     = lv_value.


      IF ( lv_value IS INITIAL ) OR ( lv_value EQ '0000000000' ).
        CLEAR: <fs_item>-navio, <fs_item>-id_nomeacao_tran.
      ELSE.

        SELECT SINGLE *
          FROM znom_transporte INTO @DATA(lwa_znom_transporte)
         WHERE id_nomeacao_tran EQ @lv_value.

        IF sy-subrc EQ 0.
          <fs_item>-navio = lwa_znom_transporte-ds_nome_transpor.
        ELSE.
          MESSAGE |Nomeação não encontrada para o Id: { lv_value } | TYPE 'S'.
          CLEAR: <fs_item>-navio, <fs_item>-id_nomeacao_tran, lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'ID_NOMEACAO_TRAN'
              i_value     = lv_value.
        ENDIF.

      ENDIF.


    ENDLOOP.
    "US 73163 - Fim


    " 07.04.2025 - RAMON - 166561 -->
    PERFORM f_update_preco_lote USING er_data_changed.
    " 07.04.2025 - RAMON - 166561 --<

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_f4.

    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,     "Nome da tabela
             fieldname TYPE dd03l-fieldname,   "Nome de campo
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,     "Nome da tabela
             fieldname  TYPE dd03l-fieldname,   "Nome de campo
             char79(79) TYPE c,
           END OF ty_value,

           BEGIN OF tyl_instrucao,
             field(100),
           END OF tyl_instrucao,

           BEGIN OF tyl_ins_aux,
             instrucao TYPE zsdt0066-instrucao,
           END OF tyl_ins_aux.

    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    TYPES:
      BEGIN OF ty_0108,
        lifnr TYPE lifnr,
        lgort TYPE lgort_d,
      END OF ty_0108,

      BEGIN OF ty_f4,
        lifnr TYPE lifnr,
        ort01 TYPE ort01_gp,
        lgort TYPE lgort_d,
        lgobe TYPE lgobe,
      END OF ty_f4,

      BEGIN OF ty_f4_ins,
        instrucao TYPE zsdt0053-instrucao,
        terminal  TYPE zsdt0053-terminal,
        charg     TYPE zsdt0053-charg,
        matnr     TYPE zsdt0053-matnr,
        voleh     TYPE zsdt0053-voleh,
      END OF ty_f4_ins,

      BEGIN OF ty_f4_zieme,
        msehi TYPE t006a-msehi,
        msehl TYPE t006a-msehl,
      END OF ty_f4_zieme.

    DATA: tl_valuetab        LIKE TABLE OF wl_valuetab,
          tl_field           TYPE TABLE OF ty_field,
          wl_field           TYPE ty_field,
          tl_value           TYPE TABLE OF ty_value,
          wl_value           TYPE ty_value,
          wl_itens           LIKE LINE OF tg_itens,
          wl_logistica       LIKE LINE OF tg_logistica,
          l_zieme            TYPE zsdt0055-zieme,
          wl_index           TYPE sy-tabix,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30),
          tl_znom_transporte TYPE TABLE OF znom_transporte,
          wl_znom_transporte TYPE znom_transporte,
          tl_ins             TYPE TABLE OF tyl_instrucao,
          wl_ins             TYPE tyl_instrucao,
          wl_instrucao       LIKE LINE OF tg_instrucao,
          tl_ins_aux         TYPE TABLE OF tyl_ins_aux,
          wl_ins_aux         TYPE tyl_ins_aux,
          it_return          TYPE TABLE OF ddshretval,
          wa_return          LIKE LINE  OF it_return,
          it_zsdt0108        TYPE TABLE OF ty_0108,
          it_f4              TYPE TABLE OF ty_f4,
          tf4ins             TYPE TABLE OF ty_f4_ins,
          tf4zieme           TYPE TABLE OF ty_f4_zieme,
          it_lfa1            TYPE TABLE OF lfa1,
          it_t001l           TYPE TABLE OF t001l,
          it_fmap            TYPE STANDARD TABLE OF dselc,
          wa_fmap            TYPE dselc.

    FIELD-SYMBOLS: <wa_0108>      TYPE ty_0108,
                   <wa_f4>        TYPE ty_f4,
                   <wa_lfa1>      TYPE lfa1,
                   <wa_t001l>     TYPE t001l,
                   <wa_itens>     TYPE ty_itens,
                   <wa_logistica> TYPE ty_logistica.

    CLEAR: tl_valuetab[], tl_field[], wl_field, tl_value[],
           wl_value, wl_itens, wl_index, wl_char, wl_fieldname, wl_logistica,
           wl_tabname, tl_znom_transporte[], wl_znom_transporte, it_zsdt0108[],
           it_lfa1[], it_t001l[].

    READ TABLE tg_itens     INTO wl_itens     INDEX es_row_no-row_id.
    READ TABLE tg_logistica INTO wl_logistica INDEX es_row_no-row_id.

    IF it_zsdt0045[] IS INITIAL.  "#performance

      SELECT * FROM zsdt0045
        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
          WHERE bukrs       EQ wg_header-vkorg
            AND objecttable EQ 'ZSDT0051'.

    ENDIF.


    CASE e_fieldname.

*-IR065241 - 06.07.2021 - JT - inicio
      WHEN 'ZIEME'.
        SELECT *
          FROM t006a
          INTO TABLE @DATA(t_t006a)
         WHERE   spras = @sy-langu
           AND ( msehi = 'KG'
            OR   msehi = 'TO'

            " 02.09.2024 - RAMON --> bug 150168
            OR   msehi = 'M3'
            OR   msehi = 'L'
          " 02.09.2024 - RAMON --< bug 150168

            ).

        tf4zieme = VALUE #( FOR ls19 IN t_t006a
                           (
                             msehi     = ls19-msehi
                             msehl     = ls19-msehl
                           )
                       ).

        FREE: it_fmap.
        wa_fmap-fldname   = 'F0001'.
        wa_fmap-dyfldname = 'MSEHI'.
        APPEND wa_fmap   TO it_fmap.

        SORT tf4zieme BY msehi.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'MSEHI'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            dynpfld_mapping = it_fmap
            value_tab       = tf4zieme
            return_tab      = it_return
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc IS INITIAL.
          READ TABLE tg_logistica ASSIGNING <wa_logistica> INDEX es_row_no-row_id.

          IF sy-subrc = 0.
            CLEAR l_zieme.
            TRY .
                l_zieme = it_return[ 1 ]-fieldval.
              CATCH cx_sy_itab_line_not_found.
            ENDTRY.

            <wa_logistica>-zieme = l_zieme.
          ENDIF.
        ENDIF.
*-IR065241 - 06.07.2021 - JT - fim

      WHEN 'TERMINAL'.

        IF wl_itens-lgort IS INITIAL.
          SELECT terminal lgort
             FROM zsdt0108
            INTO TABLE it_zsdt0108.
        ELSE.
          SELECT terminal lgort
            FROM zsdt0108
            INTO TABLE it_zsdt0108
            WHERE lgort EQ wl_itens-lgort.
        ENDIF.

        CHECK it_zsdt0108 IS NOT INITIAL.

        SELECT * FROM lfa1
          INTO TABLE it_lfa1
          FOR ALL ENTRIES IN it_zsdt0108
          WHERE lifnr EQ it_zsdt0108-lifnr.

        SELECT * FROM t001l
          INTO TABLE it_t001l
          FOR ALL ENTRIES IN it_zsdt0108
          WHERE lgort EQ it_zsdt0108-lgort.

        SORT it_t001l BY lgort.
        DELETE ADJACENT DUPLICATES FROM it_t001l COMPARING lgort.

        LOOP AT it_zsdt0108 ASSIGNING <wa_0108>.
          APPEND INITIAL LINE TO it_f4 ASSIGNING <wa_f4>.

          MOVE <wa_0108>-lifnr TO <wa_f4>-lifnr.
          MOVE <wa_0108>-lgort TO <wa_f4>-lgort.

          READ TABLE it_lfa1 ASSIGNING <wa_lfa1> WITH KEY lifnr = <wa_0108>-lifnr.
          IF sy-subrc IS INITIAL.
            MOVE <wa_lfa1>-ort01 TO <wa_f4>-ort01.
          ENDIF.

          READ TABLE it_t001l ASSIGNING <wa_t001l> WITH KEY lgort = <wa_0108>-lgort.
          IF sy-subrc IS INITIAL.
            MOVE <wa_t001l>-lgobe TO <wa_f4>-lgobe.
          ENDIF.

        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield         = 'LIFNR'
            value_org        = 'S'
            callback_program = sy-cprog
            callback_form    = 'CALLBACK_F4'
          TABLES
            value_tab        = it_f4
            return_tab       = it_return
          EXCEPTIONS
            parameter_error  = 1
            no_values_found  = 2
            OTHERS           = 3.

        IF sy-subrc IS INITIAL.
          LOOP AT it_return INTO wa_return.
            READ TABLE tg_itens ASSIGNING <wa_itens> INDEX es_row_no-row_id.
            IF sy-subrc IS INITIAL.
              CASE wa_return-retfield.
                WHEN 'LIFNR'.

                  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                    EXPORTING
                      input  = wa_return-fieldval
                    IMPORTING
                      output = <wa_itens>-terminal.

                WHEN 'LGORT'.
                  IF wl_itens-lgort IS INITIAL.
                    MOVE wa_return-fieldval TO <wa_itens>-lgort.
                  ENDIF.
              ENDCASE.
            ENDIF.
          ENDLOOP.
        ENDIF.

        " 18.03.2025 -- RAMON -->
      WHEN 'PMEIN'.
        PERFORM f_f4_pmein USING es_row_no-row_id.
        " 18.03.2025 -- RAMON --<

      WHEN 'NAVIO'.
        "Comentando selecão por nome Navio - US 73163
*        SELECT DISTINCT ds_nome_transpor
*          FROM znom_transporte
*          INTO CORRESPONDING FIELDS OF TABLE tl_znom_transporte.
*
*        wl_fieldname  = 'DS_NOME_TRANSPOR'.
*        wl_tabname    = 'ZNOM_TRANSPORTE'.
*
*        LOOP AT tl_znom_transporte INTO wl_znom_transporte.
*          MOVE: wl_znom_transporte-ds_nome_transpor TO wl_valuetab-field.
*          APPEND wl_valuetab TO tl_valuetab.
*        ENDLOOP.
*
*        wl_field-tabname = wl_tabname.
*        wl_field-fieldname = wl_fieldname.
*        wl_field-s = 'X'.
*        APPEND wl_field TO tl_field.
        "Comentando selecão por nome Navio - US 73163 - Fim
      WHEN 'PORTO'.
        "Comentando selecão por nome Porto - US 73163
*        SELECT DISTINCT ds_porto
*          FROM znom_transporte
*          INTO CORRESPONDING FIELDS OF TABLE tl_znom_transporte.
*
*        wl_fieldname  = 'DS_PORTO'.
*        wl_tabname    = 'ZNOM_TRANSPORTE'.
*
*        LOOP AT tl_znom_transporte INTO wl_znom_transporte.
*          MOVE: wl_znom_transporte-ds_porto TO wl_valuetab-field.
*          APPEND wl_valuetab TO tl_valuetab.
*        ENDLOOP.
*
*        wl_field-tabname = wl_tabname.
*        wl_field-fieldname = wl_fieldname.
*        wl_field-s = 'X'.
*        APPEND wl_field TO tl_field.
        "Comentando selecão por nome Porto - US 73163

      WHEN 'INSTRUCAO_ANT'.
*        BREAK WBARBOSA.

*-CS2021000615 - 17.06.2021 - JT - inicio
        IF wg_header-param_espec = c_ax.
          tf4ins = VALUE #( FOR ls12 IN it_zsdt0045
                             (
                               instrucao = ls12-instrucao
                               terminal  = ls12-terminal
                               charg     = ls12-safra
                               matnr     = ls12-matnr
                               voleh     = ls12-voleh
                             )
                         ).
        ELSE.
          tf4ins = VALUE #( FOR ls IN tg_form_lote
                            FOR ls1 IN tg_instrucao WHERE ( instrucao = ls-instrucao )
                              (
                                instrucao = ls-instrucao
                                terminal  = ls1-terminal
                                charg     = ls1-safra
                                matnr     = ls1-matnr
                                voleh     = ls1-voleh
                              )
                          ).
        ENDIF.
*-CS2021000615 - 17.06.2021 - JT - fim

*-CS2021000615 - 17.06.2021 - JT - inicio
        FREE: it_fmap.
        wa_fmap-fldname   = 'F0001'.
        wa_fmap-dyfldname = 'INSTRUCAO'.
        APPEND wa_fmap   TO it_fmap.
        wa_fmap-fldname   = 'F0004'.
        wa_fmap-dyfldname = 'MATNR'.
        APPEND wa_fmap   TO it_fmap.
*-CS2021000615 - 17.06.2021 - JT - fim

        SORT tf4ins BY instrucao.
        DELETE ADJACENT DUPLICATES FROM tf4ins COMPARING instrucao.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'INSTRUCAO'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            value_tab       = tf4ins
            return_tab      = it_return
            dynpfld_mapping = it_fmap
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc IS INITIAL.

          "DATA INSTRUCAO TYPE ZSDED030.
          CLEAR instrucao.
          TRY .
              instrucao = it_return[ 1 ]-fieldval.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          CLEAR matnr.
          TRY .
              matnr = it_return[ 2 ]-fieldval.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

          CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
            EXPORTING
              input  = matnr
            IMPORTING
              output = matnr.

          READ TABLE tg_itens ASSIGNING <wa_itens> INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            <wa_itens>-instrucao_ant = instrucao.
            <wa_itens>-matnr         = matnr.

            TRY.
                DATA(wa_inst) = tg_instrucao[ instrucao = <wa_itens>-instrucao_ant
                                              matnr     = <wa_itens>-matnr  ].
              CATCH cx_sy_itab_line_not_found.
                CLEAR wa_inst.
            ENDTRY.

            <wa_itens>-terminal  = wa_inst-terminal.
            <wa_itens>-charg     = wa_inst-safra.
*           <wa_itens>-matnr     = wa_inst-matnr.

            SELECT SINGLE maktx
              FROM makt
              INTO <wa_itens>-maktx
              WHERE matnr EQ <wa_itens>-matnr
                AND spras EQ sy-langu.

            <wa_itens>-voleh = wa_inst-voleh.

          ENDIF.
        ENDIF.


*        WL_FIELDNAME  = 'INSTRUCAO'.
*        WL_TABNAME    = 'ZSDT0053'.



*        LOOP AT TG_FORM_LOTE INTO DATA(_FORM_LOTE).

*          APPEND VALUE #( FIELD = _FORM_LOTE-INSTRUCAO ) TO TL_VALUETAB.
*          APPEND VALUE #( INSTRUCAO = _FORM_LOTE-INSTRUCAO ) TO TL_INS_AUX.

*
*          WL_VALUETAB-FIELD = WL_INSTRUCAO-BUKRS.
*          APPEND WL_VALUETAB TO TL_VALUETAB.
*
*          WL_VALUETAB-FIELD = WL_INSTRUCAO-WERKS.
*          APPEND WL_VALUETAB TO TL_VALUETAB.
*
*          WL_VALUETAB-FIELD = WL_INSTRUCAO-INSTRUCAO.
*          APPEND WL_VALUETAB TO TL_VALUETAB.
*
*          WL_INS_AUX-INSTRUCAO = WL_INSTRUCAO-INSTRUCAO.
*          APPEND WL_INS_AUX TO TL_INS_AUX.
*
*          WL_VALUETAB-FIELD = WL_INSTRUCAO-PORTO_EMBARQUE.
*          APPEND WL_VALUETAB TO TL_VALUETAB.
*
*          CLEAR: WL_INS_AUX.
*        ENDLOOP.

*        SORT: TL_INS_AUX, TL_VALUETAB.
*        DELETE ADJACENT DUPLICATES FROM TL_INS_AUX COMPARING ALL FIELDS.
*        DELETE ADJACENT DUPLICATES FROM TL_VALUETAB COMPARING ALL FIELDS.
*
*        LOOP AT TL_INS_AUX INTO DATA(_INST).
*          DATA(VL_FL) = REDUCE ZSDED029( INIT X = 0
*                            FOR LT IN TG_FORM_LOTE
*                            WHERE ( INSTRUCAO EQ _INST-INSTRUCAO )
*                                NEXT X = X + LT-VOLUM ).
*
*          DATA(VL_EX) = REDUCE ZSDED029( INIT X = 0
*                            FOR EX IN TG_ITENS
*                            WHERE ( INSTRUCAO EQ _INST-INSTRUCAO )
*                                NEXT X = X + EX-VOLUM ).
*
*          IF ( VL_FL - VL_EX ) <= 0.
*            DELETE TL_INS_AUX WHERE INSTRUCAO EQ _INST-INSTRUCAO.
*            DELETE TL_VALUETAB WHERE FIELD EQ _INST-INSTRUCAO.
*          ENDIF.
*        ENDLOOP.
*
*        TL_FIELD =
*        VALUE #(
*                ( TABNAME = WL_TABNAME FIELDNAME = WL_FIELDNAME S = ABAP_TRUE )
*               ).
      WHEN 'INSTRUCAO'.

        READ TABLE tg_itens ASSIGNING <wa_itens> INDEX es_row_no-row_id.
        IF sy-subrc IS INITIAL.
          IF <wa_itens>-instrucao_ant IS NOT INITIAL.

            "select * from zsdt0045 into TL_0045.

            "TG_INSTRUCAO   _ant = ZEINSTRUCAO.

            tf4ins = VALUE #( FOR ls12 IN it_zsdt0045
                               (
                                 instrucao = ls12-instrucao
                                 terminal  = ls12-terminal
                                 charg     = ls12-safra
                                 matnr     = ls12-matnr
                                 voleh     = ls12-voleh
                               )
                           ).

*-CS2021000615 - 17.06.2021 - JT - inicio
            FREE: it_fmap.
            wa_fmap-fldname   = 'F0001'.
            wa_fmap-dyfldname = 'INSTRUCAO'.
            APPEND wa_fmap   TO it_fmap.
            wa_fmap-fldname   = 'F0004'.
            wa_fmap-dyfldname = 'MATNR'.
            APPEND wa_fmap   TO it_fmap.
*-CS2021000615 - 17.06.2021 - JT - fim

            SORT tf4ins BY instrucao.
            DELETE ADJACENT DUPLICATES FROM tf4ins COMPARING instrucao matnr.

            CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
              EXPORTING
                retfield        = 'INSTRUCAO'
                dynpprog        = sy-repid
                dynpnr          = sy-dynnr
                value_org       = 'S'
              TABLES
                dynpfld_mapping = it_fmap
                value_tab       = tf4ins
                return_tab      = it_return
              EXCEPTIONS
                parameter_error = 1
                no_values_found = 2
                OTHERS          = 3.

            IF sy-subrc IS INITIAL.

              "DATA INSTRUCAO TYPE ZSDED030.
              CLEAR instrucao.
              TRY .
                  instrucao = it_return[ 1 ]-fieldval.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              "READ TABLE TG_ITENS ASSIGNING <WA_ITENS> INDEX ES_ROW_NO-ROW_ID.
              "IF SY-SUBRC IS INITIAL.
              <wa_itens>-instrucao = instrucao.

*-CS2021000615 - 17.06.2021 - JT - inicio
              CLEAR matnr.
              TRY .
                  matnr = it_return[ 2 ]-fieldval.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input  = matnr
                IMPORTING
                  output = matnr.

              "READ TABLE TG_ITENS ASSIGNING <WA_ITENS> INDEX ES_ROW_NO-ROW_ID.
              "IF SY-SUBRC IS INITIAL.
              <wa_itens>-matnr  = matnr.
*-CS2021000615 - 17.06.2021 - JT - fim

*              TRY.
*                  DATA(WA_INSTR) = IT_ZSDT0045[ INSTRUCAO = <WA_ITENS>-INSTRUCAO ].
*                CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*                  CLEAR WA_INSTR.
*              ENDTRY.
*
*              <WA_ITENS>-TERMINAL  = WA_INSTR-TERMINAL.
*              <WA_ITENS>-CHARG     = WA_INSTR-SAFRA.
*              <WA_ITENS>-MATNR     = WA_INSTR-MATNR.
*
*              SELECT SINGLE MAKTX
*                FROM MAKT
*                INTO <WA_ITENS>-MAKTX
*                WHERE MATNR EQ <WA_ITENS>-MATNR
*                  AND SPRAS EQ SY-LANGU.
*
*              <WA_ITENS>-VOLEH = WA_INST-VOLEH.

              " ENDIF.
            ENDIF.
          ELSE.

*            BREAK WBARBOSA.

*-CS2021000615 - 17.06.2021 - JT - inicio
            IF wg_header-param_espec = c_ax.
              tf4ins = VALUE #( FOR ls12 IN it_zsdt0045
                                 (
                                   instrucao = ls12-instrucao
                                   terminal  = ls12-terminal
                                   charg     = ls12-safra
                                   matnr     = ls12-matnr
                                   voleh     = ls12-voleh
                                 )
                             ).
            ELSE.
              tf4ins = VALUE #( FOR ls IN tg_form_lote
                                FOR ls1 IN tg_instrucao WHERE ( instrucao = ls-instrucao )
                                  (
                                    instrucao = ls-instrucao
                                    terminal  = ls1-terminal
                                    charg     = ls1-safra
                                    matnr     = ls1-matnr
                                    voleh     = ls1-voleh
                                  )
                              ).
            ENDIF.
*-CS2021000615 - 17.06.2021 - JT - fim

*-CS2021000615 - 17.06.2021 - JT - inicio
            FREE: it_fmap.
            wa_fmap-fldname   = 'F0001'.
            wa_fmap-dyfldname = 'INSTRUCAO'.
            APPEND wa_fmap   TO it_fmap.
            wa_fmap-fldname   = 'F0004'.
            wa_fmap-dyfldname = 'MATNR'.
            APPEND wa_fmap   TO it_fmap.
*-CS2021000615 - 17.06.2021 - JT - fim

            SORT tf4ins BY instrucao.
            DELETE ADJACENT DUPLICATES FROM tf4ins COMPARING instrucao matnr.

            CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
              EXPORTING
                retfield        = 'INSTRUCAO'
                dynpprog        = sy-repid
                dynpnr          = sy-dynnr
                value_org       = 'S'
              TABLES
                dynpfld_mapping = it_fmap
                value_tab       = tf4ins
                return_tab      = it_return
              EXCEPTIONS
                parameter_error = 1
                no_values_found = 2
                OTHERS          = 3.

            IF sy-subrc IS INITIAL.

              "DATA INSTRUCAO TYPE ZSDED030.
              CLEAR instrucao.
              TRY .
                  instrucao = it_return[ 1 ]-fieldval.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              "READ TABLE TG_ITENS ASSIGNING <WA_ITENS> INDEX ES_ROW_NO-ROW_ID.
              "IF SY-SUBRC IS INITIAL.
              <wa_itens>-instrucao = instrucao.

*-CS2021000615 - 17.06.2021 - JT - inicio
              CLEAR matnr.
              TRY .
                  matnr = it_return[ 2 ]-fieldval.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.

              CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
                EXPORTING
                  input  = matnr
                IMPORTING
                  output = matnr.

              "READ TABLE TG_ITENS ASSIGNING <WA_ITENS> INDEX ES_ROW_NO-ROW_ID.
              "IF SY-SUBRC IS INITIAL.
              <wa_itens>-matnr  = matnr.
*-CS2021000615 - 17.06.2021 - JT - fim

              TRY.
                  DATA(wa_ins) = tg_instrucao[ instrucao = <wa_itens>-instrucao
                                               matnr     = <wa_itens>-matnr     ].
                CATCH cx_sy_itab_line_not_found.
                  CLEAR wa_ins.
              ENDTRY.

              <wa_itens>-terminal  = wa_ins-terminal.
              <wa_itens>-charg     = wa_ins-safra.
*             <wa_itens>-matnr     = wa_ins-matnr.

              SELECT SINGLE maktx
                FROM makt
                INTO <wa_itens>-maktx
                WHERE matnr EQ <wa_itens>-matnr
                  AND spras EQ sy-langu.

              <wa_itens>-voleh = wa_inst-voleh.

              " ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

    ENDCASE.

    CASE e_fieldname.
      WHEN 'TERMINAL' OR 'INSTRUCAO_ANT' OR 'INSTRUCAO'.

*        READ TABLE TG_ITENS ASSIGNING <WA_ITENS> INDEX ES_ROW_NO-ROW_ID.
*        IF SY-SUBRC IS INITIAL.
*          IF E_FIELDNAME = 'INSTRUCAO' AND <WA_ITENS>-INSTRUCAO_ANT IS NOT INITIAL.
*            EXIT.
*          ENDIF.
*        ENDIF.
      WHEN OTHERS.
        IF wl_fieldname  IS NOT INITIAL
        AND wl_tabname    IS NOT INITIAL
        AND tl_field[]    IS NOT INITIAL
        AND tl_valuetab[] IS NOT INITIAL.

          CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
            EXPORTING
*             cucol                     = '3'
              fieldname                 = wl_fieldname
              tabname                   = wl_tabname
            IMPORTING
              index                     = wl_index
              select_value              = wl_char
            TABLES
              fields                    = tl_field
              select_values             = tl_value
              valuetab                  = tl_valuetab
            EXCEPTIONS
              field_not_in_ddic         = 001
              more_then_one_selectfield = 002
              no_selectfield            = 003.

          IF sy-subrc IS INITIAL
            AND wl_index <> 0.
            CASE e_fieldname.
              WHEN 'NAVIO'.
                "Comentando selecão por nome Navio - US 73163 - Ini
*                READ TABLE tl_znom_transporte INTO wl_znom_transporte INDEX wl_index.
*                IF es_row_no-row_id GT 0.
*                  READ TABLE tg_itens INTO wl_itens INDEX es_row_no-row_id.
*                  IF sy-subrc IS INITIAL.
*                    MOVE: wl_znom_transporte-ds_nome_transpor TO wl_itens-navio.
*                    MODIFY tg_itens FROM wl_itens INDEX es_row_no-row_id TRANSPORTING navio.
*                  ENDIF.
*                ENDIF.
                "Comentando selecão por nome Navio - US 73163 - Fim
              WHEN 'PORTO'.
                "Comentando selecão por nome Porto - US 73163
*                READ TABLE tl_znom_transporte INTO wl_znom_transporte INDEX wl_index.
*                IF es_row_no-row_id GT 0.
*                  READ TABLE tg_itens INTO wl_itens INDEX es_row_no-row_id.
*                  IF sy-subrc IS INITIAL.
*                    MOVE: wl_znom_transporte-ds_porto TO wl_itens-porto.
*                    MODIFY tg_itens FROM wl_itens INDEX es_row_no-row_id TRANSPORTING porto.
*                  ENDIF.
*                ENDIF.
                "Comentando selecão por nome Porto - US 73163
              WHEN 'INSTRUCAO'.

*                READ TABLE TL_INS_AUX INTO WL_INS_AUX INDEX WL_INDEX.
*                READ TABLE TG_ITENS INTO WL_ITENS INDEX ES_ROW_NO-ROW_ID.
*
*                TRY .
*                    WA_INST = TG_INSTRUCAO[ INSTRUCAO = WL_INS_AUX-INSTRUCAO ].
*                  CATCH CX_SY_ITAB_LINE_NOT_FOUND.
*                    CLEAR WA_INST.
*                ENDTRY.
*
*                WL_ITENS-INSTRUCAO = WL_INS_AUX-INSTRUCAO.
*                WL_ITENS-TERMINAL  = WA_INST-TERMINAL.
*                WL_ITENS-CHARG = WA_INST-SAFRA.
*                WL_ITENS-MATNR = WA_INST-MATNR.
*
*                SELECT SINGLE MAKTX
*                  FROM MAKT
*                  INTO WL_ITENS-MAKTX
*                  WHERE MATNR EQ WL_ITENS-MATNR
*                    AND SPRAS EQ SY-LANGU.
*
*                WL_ITENS-VOLEH = WA_INST-VOLEH.
*
*                MODIFY TG_ITENS FROM WL_ITENS INDEX ES_ROW_NO-ROW_ID TRANSPORTING INSTRUCAO TERMINAL CHARG MATNR MAKTX VOLEH.
            ENDCASE.
          ENDIF.
        ENDIF.
    ENDCASE.

*** Método de atualização de dados na grid
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

*-IR065241 - 06.07.2021 - JT - inicio
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
*-IR065241 - 06.07.2021 - JT - fim

  ENDMETHOD.                    "on_f4

  METHOD on_data_changed_pgto.
    DATA: ls_good      TYPE lvc_s_modi,
          ls_good_aux  TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          lv_value_aux TYPE lvc_value,
          vl_tabix     TYPE sy-tabix,
          vl_value     TYPE lvc_value,
          wl_vlr_real  TYPE zsdt0054-vlr_real,
          wl_dmbtr     TYPE zsdt0054-dmbtr,
          wl_kursf     TYPE zsdt0054-kursf.

    CLEAR: wl_vlr_real, wl_dmbtr, wl_kursf.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'DMBTR'
                                OR fieldname = 'KURSF'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF ls_good-fieldname EQ 'DMBTR'.
        wl_dmbtr = lv_value.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'KURSF'
          IMPORTING
            e_value     = lv_value.

        CONDENSE lv_value NO-GAPS.
        wl_kursf = lv_value.

      ELSE.
        wl_kursf = lv_value.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good_aux-row_id
            i_tabix     = ls_good_aux-tabix
            i_fieldname = 'DMBTR'
          IMPORTING
            e_value     = lv_value.

        CONDENSE lv_value NO-GAPS.
        wl_dmbtr = lv_value.
      ENDIF.
      IF wg_header-param_espec EQ c_p
      AND ( wg_cond_pgt-pgto_ant EQ c_x
       OR wg_cond_pgt-pgto_ant EQ c_n ).
        wl_vlr_real = wl_dmbtr * wl_kursf.
        lv_value = wl_vlr_real.
        CONDENSE lv_value NO-GAPS.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_REAL'
            i_value     = lv_value.
      ELSE.
        CLEAR: lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_REAL'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'KURSF'
            i_value     = lv_value.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VLR_REAL'
            i_value     = lv_value.
      ENDIF.
    ENDLOOP.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DATA_CHANGED_PGTO
  METHOD on_data_changed_adto_ext.
    DATA: ls_good      TYPE lvc_s_modi,
          ls_good_aux  TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          lv_value_aux TYPE lvc_value,
          vl_tabix     TYPE sy-tabix,
          vl_value     TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'LIFNR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE name1
         FROM lfa1
         INTO lv_value
          WHERE lifnr EQ lv_value.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: lv_value.
      ENDIF.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'NAME1'
          i_value     = lv_value.
    ENDLOOP.


    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DATA_CHANGED_ADTO_EXT

  METHOD on_data_changed_finished.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: obg_event TYPE REF TO lcl_event_handler.
    CREATE OBJECT obg_event.

    DATA: wl_itens    LIKE LINE OF tg_itens,
          tl_makt     TYPE TABLE OF makt,
          tl_mara     TYPE TABLE OF mara,
          wl_preco    LIKE LINE OF tg_preco,
          wl_makt     TYPE makt,
          wl_mara     TYPE mara,
          wl_dmbtr    TYPE bsid-dmbtr,
          ls_good     TYPE lvc_s_modi,
          wl_matnr    TYPE mara-matnr,
          wl_zieme    TYPE mara-meins,
          wl_pmein    TYPE mara-meins,
          wl_menge    TYPE ekpo-menge,
          wl_terminal TYPE zsdt0053-terminal,
          wl_tabix    TYPE sy-tabix.

*-CS2023000189-#126959-07.11.2023-JT-inicio
    CHECK NOT e_modified IS INITIAL.
    CHECK NOT et_good_cells IS INITIAL.
*-CS2023000189-#126959-07.11.2023-JT-fim

    UNASSIGN <fs_line>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.
    CLEAR: wl_makt, wl_itens, wl_mara, wl_preco, wl_dmbtr.
    REFRESH: tl_makt, tl_mara.

    TRY .
        DATA(_fieldname) = et_good_cells[ 1 ]-fieldname.
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
*.
    IF tg_itens[] IS NOT INITIAL.

      tl_mara[] = zcl_material=>get_instance( )->get_material_by_table( tg_itens[] ). "# performance

*      SELECT *
*        FROM mara
*        INTO TABLE tl_mara
*         FOR ALL ENTRIES IN tg_itens
*         WHERE matnr EQ tg_itens-matnr.

      IF sy-subrc IS INITIAL.

        tl_makt[] = zcl_material=>get_instance( )->get_material_text_by_table( tl_mara[] ). "# performance

*        SELECT *
*          FROM makt
*          INTO TABLE tl_makt
*           FOR ALL ENTRIES IN tl_mara
*           WHERE matnr EQ tl_mara-matnr
*          AND spras EQ sy-langu.

      ENDIF.
    ENDIF.
    IF wg_header-param_espec NE c_m.
      PERFORM retorna_preco_item USING   '1'
                                       space
                              CHANGING wl_dmbtr.

    ENDIF.
    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      READ TABLE tl_mara INTO wl_mara
          WITH KEY matnr = wl_itens-matnr.

      READ TABLE tl_makt INTO wl_makt
          WITH KEY matnr = wl_itens-matnr.
      IF sy-subrc IS INITIAL.
        IF wg_header-param_espec EQ c_m.

          PERFORM retorna_preco_item USING   '1'
                                           wl_itens-fixacao
                                  CHANGING wl_dmbtr.

        ENDIF.
        MOVE: wl_makt-maktx TO wl_itens-maktx.
        IF  ls_good-fieldname EQ 'MATNR'.
*        and ls_good-value ne wl_itens-matnr )
*        or wl_itens-zieme is initial.
          MOVE:  wl_mara-meins TO wl_itens-zieme.
        ENDIF.
        IF wg_header-param_espec EQ c_z.
          wl_itens-item_edit = 'X'.
          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id  TRANSPORTING item_edit.

          " wl_dmbtr = wl_itens-dmbtr.
          "  CLEAR wl_itens-vlrtot.

        ENDIF.
        IF wg_header-param_espec NE c_p.
          TRY .
              wl_itens-vlrtot = wl_itens-zmeng * wl_dmbtr.
            CATCH cx_sy_arithmetic_overflow.
              wl_itens-vlrtot = 0.
          ENDTRY.
        ELSE.
          TRY.
              wl_itens-dmbtr = wl_itens-vlrtot / wl_itens-zmeng.
            CATCH cx_sy_zerodivide.
          ENDTRY.
        ENDIF.
        MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
        CLEAR: wl_makt.
      ENDIF.
*      IF LS_GOOD-FIELDNAME EQ 'ZMENG'.
*        PERFORM REFRESH_LOGISTICA.
*      ENDIF.
    ENDLOOP.
    LOOP AT tg_itens INTO wl_itens
       WHERE item_edit EQ c_x.

    ENDLOOP.
    IF sy-subrc IS INITIAL
    AND wg_acao NE c_atual.
      IF wg_header-param_espec NE c_p
     AND wg_header-param_espec NE c_m
         AND  wg_header-param_espec NE c_z.
        IF wg_redistribuir IS INITIAL.
          IF ( wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
               wg_header-param_espec EQ c_ax ) "OR
              " wg_header-param_espec EQ c_z ) " CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
          AND wg_acao EQ c_modif_c_ov.
            MODIFY tg_itens FROM wl_itens
            TRANSPORTING matnr maktx werks lgort charg zieme pmein
             WHERE item_edit IS INITIAL.
          ELSE.
            MODIFY tg_itens FROM wl_itens
            TRANSPORTING matnr maktx werks lgort charg zieme dmbtr pmein
             WHERE item_edit IS INITIAL.
          ENDIF.
        ELSEIF wg_acao EQ c_modif_qtd
            AND wg_redistribuir IS NOT INITIAL.
          MODIFY tg_itens FROM wl_itens
        TRANSPORTING matnr maktx werks lgort zieme dmbtr pmein
         WHERE item_edit IS INITIAL.
        ENDIF.
      ELSEIF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z .
        MODIFY tg_itens FROM wl_itens                    "########incluido o terminal para replicar para as outras celulas
        TRANSPORTING matnr maktx werks lgort zieme pmein terminal
         WHERE item_edit IS INITIAL.
      ELSEIF wg_header-param_espec EQ c_p.
        MODIFY tg_itens FROM wl_itens
        TRANSPORTING matnr maktx werks lgort charg zieme pmein
         WHERE item_edit IS INITIAL.
      ENDIF.
*         AND MATNR EQ WL_ITENS-MATNR.
      LOOP AT tg_itens INTO wl_itens.

        wl_tabix = sy-tabix.
        wl_matnr = wl_itens-matnr.
        wl_zieme = wl_itens-zieme.
        wl_pmein = wl_itens-pmein.
        wl_menge = wl_itens-zmeng.
*        WL_TERMINAL = TG_ITENS-TERMINAL.

        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = wl_matnr
            i_in_me              = wl_zieme
            i_out_me             = wl_pmein
            i_menge              = wl_menge
          IMPORTING
            e_menge              = wl_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.


        IF sy-subrc EQ 0.
          IF wg_header-param_espec NE c_p.
            TRY .
                wl_itens-vlrtot = wl_menge * wl_itens-dmbtr.
              CATCH cx_sy_arithmetic_overflow.
                wl_itens-vlrtot = 0.
            ENDTRY.

          ELSE.
            TRY.
                wl_itens-dmbtr = wl_itens-vlrtot / wl_menge.
              CATCH cx_sy_zerodivide.
            ENDTRY.
          ENDIF.
        ELSE.
          IF wg_header-param_espec NE c_p.
            TRY .
                wl_itens-vlrtot = wl_itens-zmeng * wl_itens-dmbtr.
              CATCH cx_sy_arithmetic_overflow.
                wl_itens-vlrtot = 0.
            ENDTRY.

          ELSE.
            TRY.
                wl_itens-dmbtr =  wl_itens-vlrtot / wl_itens-zmeng.
              CATCH cx_sy_zerodivide.
            ENDTRY.
          ENDIF.
        ENDIF.
        IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
           wg_header-param_espec EQ c_ax OR
           wg_header-param_espec EQ c_z. " CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
          ADD wl_itens-desc_absoluto TO wl_itens-vlrtot.
        ENDIF.

        IF wg_header-param_espec NE c_p.
          MODIFY tg_itens FROM wl_itens INDEX wl_tabix TRANSPORTING vlrtot.
        ELSE.
          MODIFY tg_itens FROM wl_itens INDEX wl_tabix TRANSPORTING dmbtr.
        ENDIF.


      ENDLOOP.
*      PERFORM REFRESH_LOGISTICA.
    ENDIF.


    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
      READ TABLE et_good_cells TRANSPORTING NO FIELDS
        WITH KEY fieldname = 'FIXACAO'.
      IF sy-subrc IS INITIAL.
*        DELETE TG_ITENS INDEX WG_SELECTEDCELL-ROW_ID-INDEX.
        PERFORM remove_preco_frame.
*              SUBTRACT 1 FROM WG_HEADER-NUM_FIXACAO.

      ENDIF.
    ENDIF.

    " 29.11.2023 - 128467 - RBL -->
    IF e_modified EQ 'I'.
      RETURN.
    ENDIF.
    " 29.11.2023 - 128467 - RBL --<

    IF e_modified IS NOT INITIAL.
      IF wg_redistribuir IS NOT INITIAL
      OR wg_acao EQ c_modif_c_ov.
        IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
          LOOP AT tg_itens  INTO wl_itens.
            IF wl_itens-fixacao GT wg_header-num_fixacao.
              PERFORM add_item_preco_frame USING wl_itens-fixacao  .
              ADD 1 TO wg_header-num_fixacao.
            ENDIF.
          ENDLOOP.
          enter.
        ENDIF.
      ENDIF.

      obg_toolbar->handle_user_command_itens( e_ucomm = 'ATUAL_ITENS' ).

    ENDIF.

    IF _fieldname IS NOT INITIAL.
      LOOP AT et_good_cells INTO ls_good
        WHERE fieldname = _fieldname.
        obg_event->calculo_desconto( linha = ls_good-row_id ).
      ENDLOOP.
    ENDIF.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD on_data_changed_finished_pgto.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: wl_pgto_ant LIKE LINE OF tg_pgt_ant,
          ls_good     TYPE lvc_s_modi.
    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_pgt_ant INTO wl_pgto_ant INDEX ls_good-row_id.

      IF wg_header-param_espec EQ c_p
        AND ( wg_cond_pgt-pgto_ant EQ c_x
         OR   wg_cond_pgt-pgto_ant EQ c_n ).
        wl_pgto_ant-vlr_real = wl_pgto_ant-dmbtr * wl_pgto_ant-kursf.
      ELSE.
        CLEAR: wl_pgto_ant-vlr_real.
      ENDIF.
      MODIFY TABLE tg_pgt_ant FROM wl_pgto_ant TRANSPORTING vlr_real.
    ENDLOOP.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "on_data_changed_finisheD_pgto
  METHOD on_data_changed_finished_adto.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: wl_adto_ext LIKE LINE OF tg_adto_ext,
          tl_lfa1     TYPE TABLE OF lfa1,
          ls_good     TYPE lvc_s_modi,
          wl_lfa1     TYPE lfa1.

    CLEAR: wl_lfa1, wl_adto_ext, ls_good.
    REFRESH: tl_lfa1.
*.
    IF tg_adto_ext[] IS NOT INITIAL.
      SELECT *
        FROM lfa1
        INTO TABLE tl_lfa1
         FOR ALL ENTRIES IN tg_adto_ext
         WHERE lifnr EQ tg_adto_ext-lifnr.

    ENDIF.
    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_adto_ext INTO wl_adto_ext INDEX ls_good-row_id.

      READ TABLE tl_lfa1 INTO wl_lfa1
        WITH KEY lifnr = wl_adto_ext-lifnr.

      IF sy-subrc IS INITIAL.
        wl_adto_ext-name1 = wl_lfa1-name1.

        MODIFY TABLE tg_adto_ext FROM wl_adto_ext. "TRANSPORTING name1 posnr.
      ENDIF.
      CLEAR: wl_lfa1, wl_adto_ext.
    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_ADTO
  METHOD on_data_changed_log.
    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          wl_logistica LIKE LINE OF tg_logistica.

    DATA: var_len        TYPE sy-tabix,
          var_valor_char TYPE c LENGTH 128,
          var_tabix      TYPE sy-tabix.

    CLEAR: ls_good, lv_value.
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'ZMENG'.
      lv_value = ls_good-value.
    ENDLOOP.


    LOOP AT er_data_changed->mt_good_cells INTO ls_good WHERE fieldname = 'VALDT_HEDGE' OR fieldname = 'DATA_PROGR'.

      IF ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z' ).

        READ TABLE tg_logistica INTO wl_logistica INDEX ls_good-row_id.

        " 13.06.2024 - RAMON - Esse codigo foi desenvolvido pq após o
        " preenchimento da condição o fieldsymbol estava mantendo o valor da ultima
        " linha do grid na referencia, quando é feito um INTO ele sobregravava esse valor, então, limpamos
        " a referencia e trocamos os INTO para ASSIGNING para nao ter mais esse problema -->>>
        UNASSIGN <fs_line>.
        "<-----

        LOOP AT <fs_table> ASSIGNING <fs_line>.

          var_tabix = sy-tabix.

          PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.

          IF ( wl_logistica-fixacao NE  wg_valor ).
            CONTINUE.
          ENDIF.

          PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

          var_len = strlen( wg_valor ).
          IF ( var_len EQ 2 ) OR ( var_len EQ 3 ).

            IF (  wg_valor(1) EQ 'T' ).
              CLEAR: var_valor_char.
              var_valor_char = '00000000'.
              CONDENSE var_valor_char NO-GAPS.

              PERFORM get_set_valores USING   'VALDT_HEDGE' 'S' CHANGING var_valor_char.

              MODIFY <fs_table> FROM <fs_line> INDEX var_tabix.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDLOOP.



    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                  "ON_DATA_CHANGED_log
  METHOD on_data_changed_finished_log.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          wl_logistica LIKE LINE OF tg_logistica.

    LOOP AT et_good_cells INTO ls_good.
      READ TABLE tg_logistica INTO wl_logistica INDEX ls_good-row_id.
      IF wl_logistica-zmeng IS NOT INITIAL.
        DELETE tg_logistica WHERE data_progr     NE wl_logistica-data_progr
                                 OR zmeng        NE wl_logistica-zmeng
                                 OR zieme        NE wl_logistica-zieme
                                 OR cadencia_qte NE wl_logistica-cadencia_qte.
      ENDIF.
    ENDLOOP.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                  "on_data_changed_finisheD_log
  METHOD on_data_changed_preco.

* Impede exclusão de linhas via tecla Delete - 26-06-2025 SMC - 180502 - IR245078>>>>>
    IF er_data_changed IS BOUND AND er_data_changed->mt_deleted_rows IS NOT INITIAL.
      MESSAGE 'Exclusão de linhas não permitida.' TYPE 'E'.
      CLEAR er_data_changed->mt_deleted_rows. " Limpa as deleções para não processar
      RETURN.
    ENDIF.
* Impede exclusão de linhas via tecla Delete - 26-06-2025 SMC 180502 - IR245078<<<<<<<

    DATA: ls_good      TYPE lvc_s_modi,
          lv_value     TYPE lvc_value,
          lv_value_aux TYPE lvc_value.

    DATA: p_2(13) TYPE p DECIMALS 2,
          p_5(13) TYPE p DECIMALS 5,
          p_4(13) TYPE p DECIMALS 4.

    LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good.
*                                 WHERE FIELDNAME = 'FORMULA2'.

      IF ls_good-fieldname EQ 'CBOT'.
        IF zcl_solicitacao_ov=>check_cbot( CONV #( ls_good-value ) ) IS NOT INITIAL.

          lv_value = ls_good-value.
          CONDENSE lv_value NO-GAPS.

          CLEAR lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = ls_good-fieldname
              i_value     = lv_value.

          MESSAGE |CBOT "{ ls_good-value }" invalido!| TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

      READ TABLE tg_fields TRANSPORTING NO FIELDS
        WITH KEY field = ls_good-fieldname.
      IF sy-subrc IS INITIAL.
        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = 'COD_FP'
          IMPORTING
            e_value     = lv_value.
        CONDENSE lv_value NO-GAPS.

        READ TABLE tg_preco_n INTO tg_preco_n
          WITH KEY field  = ls_good-fieldname
                   cod_fp = lv_value.

        TRANSLATE ls_good-value USING '. '.
        TRANSLATE ls_good-value USING ',.'.

        CONDENSE ls_good-value NO-GAPS.

        IF tg_preco_n-c_decimais EQ '2'.

          TRY.
              p_2 = ls_good-value.
              WRITE p_2 TO lv_value.
            CATCH cx_sy_conversion_no_number INTO obj_exc.
              CLEAR: lv_value.
              exc_text = obj_exc->get_text( ).
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m09."'Formato númerico não aceito'.
          ENDTRY.

        ELSEIF tg_preco_n-c_decimais EQ '4'.

          TRY.
              p_4 = ls_good-value.
              WRITE p_4 TO lv_value.
            CATCH cx_sy_conversion_no_number INTO obj_exc.
              CLEAR: lv_value.
              exc_text = obj_exc->get_text( ).
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m09."'Formato númerico não aceito'.
          ENDTRY.

        ELSEIF tg_preco_n-c_decimais EQ '5'.

          TRY.
              p_5 = ls_good-value.
              WRITE p_5 TO lv_value.
            CATCH cx_sy_conversion_no_number INTO obj_exc.
              CLEAR: lv_value.
              exc_text = obj_exc->get_text( ).
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m09."'Formato númerico não aceito'.
          ENDTRY.
        ENDIF.

*      LV_VALUE = LS_GOOD-VALUE.
        CONDENSE lv_value NO-GAPS.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = ls_good-fieldname
            i_value     = lv_value.


        CALL METHOD er_data_changed->get_cell_value
          EXPORTING
            i_row_id    = ls_good-row_id
            i_tabix     = ls_good-tabix
            i_fieldname = ls_good-fieldname
          IMPORTING
            e_value     = lv_value_aux.

      ENDIF.

      CLEAR: lv_value.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALDT_HEDGE'
          i_value     = lv_value.

      CLEAR:  lv_value_aux.
      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'VALDT_HEDGE'
        IMPORTING
          e_value     = lv_value_aux.

    ENDLOOP.

  ENDMETHOD.                    "on_data_changed_PRECO
  METHOD on_data_changed_finished_preco.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: wl_preco         LIKE LINE OF tg_preco,
          wl_preco_n       LIKE LINE OF tg_preco_n,
          wl_preco_in      TYPE zsds006,
          tl_preco_in      TYPE TABLE OF zsds006,
          tl_preco_out     TYPE TABLE OF zsds006,
          tl_0058          TYPE TABLE OF zsdt0058,
          wl_0058          TYPE zsdt0058,
          ls_good          TYPE lvc_s_modi,
          wl_dmbtr         TYPE bsid-dmbtr,
          wl_posnr         TYPE zsdt0059-posnr,
          p_2(13)          TYPE p DECIMALS 2,
          p_5(13)          TYPE p DECIMALS 5,
          p_4(13)          TYPE p DECIMALS 4,
          wl_tabix         TYPE sy-tabix,
          wl_valor(60),
          wl_valor_aux(60),
          wl_node_key      TYPE lvc_nkey,
          tl_matkl         TYPE TABLE OF mara,
          wl_matkl         TYPE mara,
          tl_0101          TYPE TABLE OF zsdt0101,
          wl_0101          TYPE zsdt0101.

    DATA: l_matnr  TYPE matnr,
          lr_matnr TYPE RANGE OF matnr.

    REFRESH: tl_preco_in, tl_preco_out.
    CLEAR: wl_posnr, wl_0101, wl_matkl.

* BUSCA O MATKL DO MATERIAL E PEGA O CONVERSOR DA ZSDT0101 E INCLUE NA ZSDT0059
*######################################################################
    IF wg_header-matnr IS NOT INITIAL.

      IF strlen( wg_header-matnr ) EQ 40.
        l_matnr = wg_header-matnr+22(18).
      ELSE.
        l_matnr = wg_header-matnr.
      ENDIF.

      APPEND INITIAL LINE TO lr_matnr ASSIGNING FIELD-SYMBOL(<mat>).
      <mat>-sign = 'I'.
      <mat>-option = 'EQ'.
      <mat>-low = l_matnr.

      APPEND INITIAL LINE TO lr_matnr ASSIGNING <mat>.
      <mat>-sign = 'I'.
      <mat>-option = 'EQ'.
      <mat>-low = wg_header-matnr.

      wl_matkl = zcl_material=>get_instance( )->get_material_by_matnr( l_matnr ). "#performance

*        SELECT SINGLE *
*          FROM mara
*          INTO wl_matkl
*          WHERE matnr IN lr_matnr.

      IF wl_matkl IS NOT INITIAL.
        IF wl_matkl-matnr = l_matnr.
          wg_header-matnr = l_matnr.
        ENDIF.
      ENDIF.

      IF gs_0101-matkl <> wl_matkl-matkl.

        SELECT SINGLE * FROM zsdt0101
          INTO gs_0101
            WHERE matkl EQ wl_matkl-matkl.

      ENDIF.

      wl_0101 = gs_0101.

    ENDIF.

*######################################################################

    LOOP AT et_good_cells INTO ls_good.

      UNASSIGN <fs_line>.
      CREATE DATA t_new_line LIKE LINE OF <fs_table>.

*     Cria uma field-symbol como work area
      ASSIGN t_new_line->* TO <fs_line>.

      IF wg_prec EQ c_chg_dtv.

        PERFORM atualiza_cbot USING ls_good-row_id.

        acao c_search.
        FREE: wg_prec, wg_acao.

      ENDIF.

      READ TABLE <fs_table> ASSIGNING <fs_line> INDEX ls_good-row_id.
      PERFORM get_set_valores USING    'COD_FP'
                                       'G'
                              CHANGING wl_preco_n-cod_fp.

      LOOP AT tg_fields INTO tg_fields.
        READ TABLE tg_preco_n INTO wl_preco_n
          WITH KEY cod_fp = wl_preco_n-cod_fp
                   field  = tg_fields-field.

        CLEAR wl_valor.
        PERFORM get_set_valores USING   tg_fields-field
                                        'G'
                                CHANGING wl_valor.


        TRANSLATE wl_valor USING '. '.
        TRANSLATE wl_valor USING ',.'.

        CONDENSE wl_valor NO-GAPS.

        IF wl_preco_n-c_decimais EQ '2'.
          TRY.
              p_2 = wl_valor.
              WRITE p_2 TO wl_valor.
            CATCH cx_sy_conversion_no_number INTO obj_exc.
              exc_text = obj_exc->get_text( ).
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m09."'Formato númerico não aceito'.
          ENDTRY.

        ELSEIF wl_preco_n-c_decimais EQ '4'.
          TRY.
              p_4 = wl_valor.
              WRITE p_4 TO wl_valor.
            CATCH cx_sy_conversion_no_number INTO obj_exc.
              exc_text = obj_exc->get_text( ).
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m09."'Formato númerico não aceito'.
          ENDTRY.

        ELSEIF wl_preco_n-c_decimais EQ '5'.
          TRY.
              p_5 = wl_valor.
              WRITE p_5 TO wl_valor.
            CATCH cx_sy_conversion_no_number INTO obj_exc.
              exc_text = obj_exc->get_text( ).
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m09."'Formato númerico não aceito'.
          ENDTRY.

        ENDIF.
        CONDENSE wl_valor NO-GAPS.
        PERFORM get_set_valores USING   tg_fields-field
                                       'S'
                               CHANGING wl_valor.

      ENDLOOP.
      MODIFY <fs_table> FROM <fs_line> INDEX ls_good-row_id.

      PERFORM get_set_valores USING   'POSNR'
                                      'G'
                              CHANGING wl_posnr.
    ENDLOOP.

    IF sy-subrc IS INITIAL.
      LOOP AT <fs_table> ASSIGNING <fs_line>.
        PERFORM get_set_valores USING 'POSNR'
                                      'G'
                              CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.
        IF wl_posnr EQ wl_valor.
          PERFORM get_set_valores USING 'COD_FP'
                                        'G'
                              CHANGING  wl_valor.

*          PERFORM GET_SET_VALORES USING 'CONVERSOR'
*                                        'S'
*                               CHANGING WL_0101-CONVERSOR.

          CONDENSE wl_valor NO-GAPS.
          LOOP AT tg_preco_n INTO wl_preco_n
            WHERE cod_fp EQ wl_valor.
            MOVE-CORRESPONDING: <fs_line> TO wl_preco_in.
            MOVE: wl_preco_n-field TO wl_preco_in-field.
            MOVE: wl_preco_n-nivel TO wl_preco_in-nivel.
            MOVE: wl_preco_n-c_decimais TO wl_preco_in-c_decimais.

            IF wl_preco_n-tipo_calc EQ c_c
            OR wl_preco_n-tipo_calc EQ c_r.
              MOVE: wl_preco_n-formula TO wl_preco_in-formula.
            ELSE.
              IF wl_preco_in-bezei NE 'CONVERSOR'.
                PERFORM get_set_valores USING wl_preco_n-field
                                              'G'
                                     CHANGING  wl_valor.
                TRANSLATE wl_valor USING '. '.
                TRANSLATE wl_valor USING ',.'.
              ELSE.
                MOVE wl_0101-conversor TO wl_valor.
              ENDIF.

              CONDENSE wl_valor NO-GAPS.

              MOVE: wl_valor TO wl_preco_in-formula.
              CONDENSE wl_preco_in-formula NO-GAPS.
            ENDIF.

            APPEND wl_preco_in TO tl_preco_in.
            CLEAR: wl_preco_in.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
* Removendo o recalculo por que influencia no preço inicial do produto.
      IF wg_acao NE 'MODIF_QTD'.

        CALL FUNCTION 'ZSDMF003_REALIZA_CALCULO'
          EXPORTING
            i_fixacao   = wl_posnr
            i_repid     = sy-repid
          TABLES
            ti_esq_calc = tl_preco_in
            te_esq_calc = tl_preco_out.

        LOOP AT <fs_table> ASSIGNING <fs_line>.
          PERFORM get_set_valores USING 'POSNR'
                                       'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          IF wl_posnr EQ wl_valor.

            LOOP AT tg_fields INTO tg_fields.
              CLEAR: wl_valor.
              PERFORM get_set_valores USING 'NIVEL'
                                                   'G'
                                           CHANGING wl_valor.
              CONDENSE wl_valor NO-GAPS.

              CLEAR: wl_valor_aux.
              PERFORM get_set_valores USING 'COD_FP'
                                             'G'
                                    CHANGING wl_valor_aux.
              CONDENSE wl_valor_aux NO-GAPS.

              wl_tabix = sy-tabix.
              READ TABLE tl_preco_out INTO wl_preco_in
                WITH KEY field  = tg_fields-field
                         cod_fp = wl_valor_aux.
              IF sy-subrc IS INITIAL.

                MOVE-CORRESPONDING: wl_preco_in TO <fs_line>.

                READ TABLE tg_preco_n INTO tg_preco_n
                WITH KEY cod_fp = wl_valor_aux
                         field  = tg_fields-field.

                CONDENSE wl_preco_in-formula NO-GAPS.
                IF tg_preco_n-c_decimais EQ '2'.
                  p_2 = wl_preco_in-formula.
                  WRITE p_2 TO wl_preco_in-formula.
                ELSEIF tg_preco_n-c_decimais EQ '4'.
                  IF wl_preco_in-bezei NE 'CONVERSOR'.
                    p_4 = wl_preco_in-formula.
                    WRITE p_4 TO wl_preco_in-formula.
                  ELSE.
                    p_4 = wl_0101-conversor.
                    WRITE p_4 TO wl_preco_in-formula.
                  ENDIF.
*              ELSEIF TG_PRECO_N-C_DECIMAIS EQ '5'
                  .
*                IF WL_0101 IS NOT INITIAL.
*                  P_5 = WL_0101-CONVERSOR.
*                ELSE.
*                  P_5 = WL_PRECO_IN-FORMULA.
*                ENDIF.
*                WRITE P_5 TO WL_PRECO_IN-FORMULA.
                ENDIF.
                CONDENSE wl_preco_in-formula NO-GAPS.
                PERFORM get_set_valores USING tg_fields-field
                                                    'S'
                                     CHANGING wl_preco_in-formula.
              ELSE.
                CLEAR: wl_preco_in.
                CONDENSE wl_preco_in-formula NO-GAPS.
                PERFORM get_set_valores USING tg_fields-field
                                                    'S'
                                     CHANGING wl_preco_in-formula.
              ENDIF.
            ENDLOOP.
            CLEAR:wl_valor_aux.
            PERFORM get_set_valores USING 'TIPO_CALC'
                                     'G'
                             CHANGING wl_valor_aux.
            CONDENSE wl_valor_aux NO-GAPS.

            IF wl_valor_aux EQ c_c.
              CLEAR:wl_valor_aux.
              PERFORM get_set_valores USING 'NIVEL'
                                       'G'
                               CHANGING wl_valor_aux.
              CONDENSE wl_valor_aux NO-GAPS.

              CLEAR:wl_valor.
              PERFORM get_set_valores USING 'COD_FP'
                                       'G'
                               CHANGING wl_valor.
              CONDENSE wl_valor_aux NO-GAPS.
              READ TABLE tl_preco_in INTO wl_preco_in
                WITH KEY nivel  = wl_valor_aux
                         cod_fp = wl_valor. "INDEX wl_tabix."sy-tabix.

              PERFORM get_set_valores USING 'FORMULA'
                                            'S'
                                   CHANGING wl_preco_in-formula.

            ENDIF.
          ENDIF.
          CLEAR: wl_preco_in, wl_preco.
        ENDLOOP.
      ENDIF.
    ENDIF.
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    IF grid8 IS NOT INITIAL.
      UNASSIGN <fs_line>.
      CREATE DATA t_new_line LIKE LINE OF <fs_table>.

      ASSIGN t_new_line->* TO <fs_line>.
      CALL METHOD grid8->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      IF et_good_cells[] IS NOT INITIAL.
        LOOP AT <fs_table> INTO <fs_line>.
          PERFORM get_set_valores USING  'ITEM_KEY'
                                              'G'
                               CHANGING wl_node_key.
          LOOP AT tg_fields INTO tg_fields.
            CLEAR: wg_valor.
            PERFORM get_set_valores USING tg_fields-field
                                                'G'
                                 CHANGING wg_valor.
            CONDENSE wg_valor NO-GAPS.
            CALL METHOD tree1->change_item
              EXPORTING
                i_node_key     = wl_node_key "WL_PRECO-ITEM_KEY
                i_fieldname    = tg_fields-field "'FORMULA2'
                i_data         = wg_valor       "no change of text
                i_u_data       = 'X'
*               IS_ITEM_LAYOUT = L_S_LACI
              EXCEPTIONS
                node_not_found = 1
                OTHERS         = 2.
            CALL METHOD cl_gui_cfw=>flush.
          ENDLOOP.
          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'WAERS'
                                        'G'
                                 CHANGING wg_valor.
          CONDENSE wg_valor NO-GAPS.
          CALL METHOD tree1->change_item
            EXPORTING
              i_node_key     = wl_node_key " WL_PRECO-ITEM_KEY
              i_fieldname    = 'WAERS'
              i_data         = wg_valor "WL_PRECO-WAERS       "no change of text
              i_u_data       = 'X'
*             IS_ITEM_LAYOUT = L_S_LACI
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2.
          CALL METHOD cl_gui_cfw=>flush.

          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'CBOT'
                                        'G'
                                 CHANGING wg_valor.
          CONDENSE wg_valor NO-GAPS.
          CALL METHOD tree1->change_item
            EXPORTING
              i_node_key     = wl_node_key
              i_fieldname    = 'CBOT'
              i_data         = wg_valor "WL_PRECO-CBOT       "no change of text
              i_u_data       = 'X'
*             IS_ITEM_LAYOUT = L_S_LACI
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2.
          CALL METHOD cl_gui_cfw=>flush.


*          CALL METHOD TREE1->CHANGE_ITEM
*           EXPORTING
*             I_NODE_KEY     = WL_PRECO-ITEM_KEY
*             I_FIELDNAME    = 'ZMENG'
*             I_DATA         = WL_PRECO-ZMENG       "no change of text
*             I_U_DATA       = 'X'
**          IS_ITEM_LAYOUT = L_S_LACI
*           EXCEPTIONS
*             NODE_NOT_FOUND = 1
*             OTHERS         = 2.
*          CALL METHOD CL_GUI_CFW=>FLUSH.
          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'VALDT'
                                        'G'
                                 CHANGING wg_valor.
          CONDENSE wg_valor NO-GAPS.
          CALL METHOD tree1->change_item
            EXPORTING
              i_node_key     = wl_node_key " WL_PRECO-ITEM_KEY
              i_fieldname    = 'VALDT'
              i_data         = wg_valor "WL_PRECO-VALDT       "no change of text
              i_u_data       = 'X'
*             IS_ITEM_LAYOUT = L_S_LACI
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2.
          CALL METHOD cl_gui_cfw=>flush.

          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'MONAT'
                                        'G'
                                 CHANGING wg_valor.
          CONDENSE wg_valor NO-GAPS.
          CALL METHOD tree1->change_item
            EXPORTING
              i_node_key     = wl_node_key "WL_PRECO-ITEM_KEY
              i_fieldname    = 'MONAT'
              i_data         = wg_valor "WL_PRECO-MONAT       "no change of text
              i_u_data       = 'X'
*             IS_ITEM_LAYOUT = L_S_LACI
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2.
          CALL METHOD cl_gui_cfw=>flush.


          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'POSNR1'
                                        'G'
                                 CHANGING wg_valor.
          CONDENSE wg_valor NO-GAPS.
          CALL METHOD tree1->change_item
            EXPORTING
              i_node_key     = wl_node_key
              i_fieldname    = 'POSNR1'
              i_data         = wg_valor
              i_u_data       = 'X'
*             IS_ITEM_LAYOUT = L_S_LACI
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2.
          CALL METHOD cl_gui_cfw=>flush.


          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'VALDT_HEDGE'
                                        'G'
                                 CHANGING wg_valor.
          CONDENSE wg_valor NO-GAPS.
          CALL METHOD tree1->change_item
            EXPORTING
              i_node_key     = wl_node_key
              i_fieldname    = 'VALDT_HEDGE'
              i_data         = wg_valor
              i_u_data       = 'X'
*             IS_ITEM_LAYOUT = L_S_LACI
            EXCEPTIONS
              node_not_found = 1
              OTHERS         = 2.
          CALL METHOD cl_gui_cfw=>flush.

        ENDLOOP.

* calculate totals
        CALL METHOD tree1->update_calculations.

** this method must be called to send the data to the frontend
        CALL METHOD tree1->frontend_update.
      ENDIF.
    ENDIF.

    IF e_modified IS NOT INITIAL.
      obg_toolbar->handle_user_command_itens( e_ucomm = 'ATUAL_ITENS' ).
    ENDIF.
    UNASSIGN <fs_line>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

*     Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.

  ENDMETHOD.                    "on_data_changed_finisheD

*  INSTRUÇÃO
  METHOD on_data_changed_ins.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          material TYPE makt,
          v_index  TYPE sy-tabix.

    FIELD-SYMBOLS <ins> TYPE zeinstrucao.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      IF ls_good-value IS INITIAL.
        CONTINUE.
      ENDIF.

      CONDENSE ls_good-value NO-GAPS.

      CASE ls_good-fieldname.
        WHEN 'MATNR'.

          SELECT SINGLE *
           FROM makt
             INTO material
               WHERE matnr EQ ls_good-value
                  AND spras EQ sy-langu.

          CALL METHOD er_data_changed->modify_cell(
              i_row_id    = ls_good-row_id
              i_fieldname = 'DESC_MAT'
              i_value     = material-maktx ).

        WHEN OTHERS.
*
*          DATA(W0045) = TG_INSTRUCAO[ LS_GOOD-ROW_ID ].
*          IF NOT W0045-WERKS IS INITIAL AND
*             NOT W0045-MATNR IS INITIAL AND
*             NOT W0045-CHARG IS INITIAL AND
*             NOT W0045-QUANTIDADE IS INITIAL AND
*                 W0045-ICON  IS INITIAL AND
*                 W0045-ZSEQ_INST   IS INITIAL AND
*                 W0045-OBJEK       IS INITIAL AND
*                 W0045-OBJECTTABLE IS INITIAL.
*
*            SELECT * FROM MCHB
*              INTO CORRESPONDING FIELDS OF TABLE TMCHB
*              WHERE MATNR EQ W0045-MATNR
*                AND WERKS EQ W0045-WERKS
*                AND LGORT EQ W0045-CHARG(4).
*
*            LOOP AT TMCHB INTO DATA(WMCHB).
*              COLLECT WMCHB INTO SALDO.
*            ENDLOOP.
*
*            DATA(WSALDO) = SALDO[ 1 ].
*            ADD WSALDO-CLABS TO WSALDO-SALDO.
*            ADD WSALDO-CUMLM TO WSALDO-SALDO.
*            ADD WSALDO-CINSM TO WSALDO-SALDO.
*            ADD WSALDO-CEINM TO WSALDO-SALDO.
*            ADD WSALDO-CSPEM TO WSALDO-SALDO.
*            ADD WSALDO-CRETM TO WSALDO-SALDO.
*            ADD WSALDO-CVMLA TO WSALDO-SALDO.
*            ADD WSALDO-CVMUM TO WSALDO-SALDO.
*            ADD WSALDO-CVMIN TO WSALDO-SALDO.
*            ADD WSALDO-CVMEI TO WSALDO-SALDO.
*            ADD WSALDO-CVMSP TO WSALDO-SALDO.
*            ADD WSALDO-CVMRE TO WSALDO-SALDO.
*
*            IF W0045-QUANTIDADE > WSALDO-SALDO.
*              MESSAGE |Total da linha { LS_GOOD-ROW_ID } maior que o disponível { WSALDO-SALDO }!| TYPE 'i'.
*            ENDIF.
*
*          ENDIF.
      ENDCASE.


    ENDLOOP.

    CALL METHOD grid6->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                      "on_data_changed_ins

* INSTRUÇÃO
  METHOD on_data_changed_finished_ins.

    DATA: contrato   TYPE zsdt0045-contrato,
          soma       TYPE zsdt0045-quantidade,
          nro_sol_ov TYPE zsdt0045-objek.

    CHECK NOT e_modified IS INITIAL.
    CHECK NOT et_good_cells IS INITIAL.

    IF et_good_cells[ 1 ]-fieldname EQ 'BTGEW'.

      PERFORM check_ins.

*      CLEAR ERRO_INS.
*
*      CONTRATO = WG_HEADER-BSTKD.
*      NRO_SOL_OV = WG_HEADER-NRO_SOL_OV.
*
*      SELECT SINGLE *
*          FROM ZSDT0143
*          INTO @DATA(WA_0143)
*          WHERE CONTRATO EQ @WG_HEADER-BSTKD.
*
*      IF NOT SY-SUBRC IS INITIAL.
*        CLEAR WA_0143.
*      ENDIF.
*
*      SELECT *
*        FROM ZSDT0045
*        INTO TABLE @DATA(IT_SOMA)
*        WHERE CONTRATO EQ @CONTRATO
*        AND OBJEK NE @NRO_SOL_OV.
*
*      LOOP AT IT_SOMA INTO DATA(WA_SOMA).
*        ADD WA_SOMA-QUANTIDADE TO SOMA.
*      ENDLOOP.
*
*      LOOP AT TG_INSTRUCAO INTO DATA(WA_INST).
*        ADD WA_INST-QUANTIDADE TO SOMA.
*      ENDLOOP.
*
*      IF SOMA GE WA_0143-QUATIDADE.
*        ERRO_INS = ABAP_TRUE.
*      ENDIF.
    ENDIF.

    "PERFORM verifica_erros.  "Comentando 25.05.2021

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_INS

  METHOD on_data_changed_pedido.

    DATA: lv_value TYPE lvc_value,
          vl_matnr TYPE makt-matnr.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(wl_good).

      CLEAR lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = wl_good-row_id
          i_tabix     = wl_good-tabix
          i_fieldname = 'MATNR'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      vl_matnr = lv_value.

      SELECT SINGLE maktx
        FROM makt
        INTO @DATA(wl_maktx)
        WHERE matnr EQ @vl_matnr
        AND spras EQ @sy-langu.

      lv_value = wl_maktx.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = wl_good-row_id
          i_fieldname = 'DESC_MATERIAL'
          i_value     = lv_value.

      CLEAR lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = wl_good-row_id
          i_tabix     = wl_good-tabix
          i_fieldname = 'WERKS'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      DATA(vl_werks) = lv_value.

      SELECT SINGLE name1
        FROM t001w
        INTO @DATA(wl_t001w)
        WHERE werks = @vl_werks.

      lv_value = wl_t001w.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = wl_good-row_id
          i_fieldname = 'DESC_CENTRO'
          i_value     = lv_value.

      CLEAR lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = wl_good-row_id
          i_tabix     = wl_good-tabix
          i_fieldname = 'PONTO_C'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      DATA(vl_pc) = lv_value.

      SELECT SINGLE name1
        FROM lfa1
        INTO @DATA(wl_pc)
        WHERE lifnr = @vl_pc.

      lv_value = wl_pc.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = wl_good-row_id
          i_fieldname = 'DESC_PC'
          i_value     = lv_value.

      CLEAR lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = wl_good-row_id
          i_tabix     = wl_good-tabix
          i_fieldname = 'FORNECEDOR'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.   DATA(vl_forn) = lv_value.

      SELECT SINGLE name1 FROM lfa1 INTO @DATA(wl_forn) WHERE lifnr = @vl_forn.

      lv_value = wl_forn.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = wl_good-row_id
          i_fieldname = 'DESC_FORN'
          i_value     = lv_value.

      CLEAR lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = wl_good-row_id
          i_tabix     = wl_good-tabix
          i_fieldname = 'LENTREGA'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.   DATA(vl_le) = lv_value.

      SELECT SINGLE name1 FROM kna1 INTO @DATA(wl_le) WHERE kunnr = @vl_le.

      lv_value = wl_le.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = wl_good-row_id
          i_fieldname = 'DESC_LE'
          i_value     = lv_value.


    ENDLOOP.
  ENDMETHOD.

  "on_data_changed_finished_ins
  METHOD on_data_changed_form.

    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          wl_dmbtr    TYPE zsdt0066-dmbtr,
          wl_usd_to   TYPE zsdt0066-usd_to,
          wl_zmeng    TYPE ekpo-menge,
          wl_zieme    TYPE mara-meins,
          wl_tabix    TYPE sy-tabix,
          wl_pmein    TYPE mara-meins,
          wl_menge    TYPE ekpo-menge,
          wl_menge_2  TYPE ekpo-menge,
          wl_maktx    TYPE makt-maktx,
          wl_vlrtot   TYPE zsdt0066-vlrtot,
          wl_matnr    TYPE makt-matnr,
          wl_libra_to TYPE zsdt0066-libra_to.

    LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'MATNR'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      wl_matnr = lv_value.

*        CONDENSE ls_good-value NO-GAPS.
*        wl_matnr = ls_good-value.
      SELECT SINGLE maktx
        FROM makt
        INTO wl_maktx
         WHERE matnr EQ wl_matnr
          AND spras EQ sy-langu.

      lv_value = wl_maktx.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MAKTX'
          i_value     = lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'DMBTR'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      wl_dmbtr = lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'ZMENG'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      wl_zmeng  = lv_value.
      wl_menge_2 = lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'ZIEME'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      wl_zieme = lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'PMEIN'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      wl_pmein = lv_value.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = wl_matnr
          i_in_me              = wl_zieme
          i_out_me             = wl_pmein
          i_menge              = wl_zmeng
        IMPORTING
          e_menge              = wl_zmeng
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.

      TRY.
          wl_vlrtot = wl_zmeng * wl_dmbtr.
        CATCH  cx_sy_arithmetic_overflow.

      ENDTRY.

      lv_value = wl_vlrtot.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLRTOT'
          i_value     = lv_value.

      CALL METHOD er_data_changed->get_cell_value
        EXPORTING
          i_row_id    = ls_good-row_id
          i_tabix     = ls_good-tabix
          i_fieldname = 'LIBRA_TO'
        IMPORTING
          e_value     = lv_value.

      CONDENSE lv_value NO-GAPS.
      wl_libra_to = lv_value.
      MULTIPLY wl_libra_to BY c_usd.

      lv_value = wl_libra_to.
      CONDENSE lv_value NO-GAPS.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'USD_TO'
          i_value     = lv_value.

      IF wl_zieme = 'TO'.
        MULTIPLY wl_libra_to BY wl_menge_2.
      ELSE.
        wl_libra_to = ( wl_libra_to * wl_menge_2 ) / 1000.
      ENDIF.

      lv_value = wl_libra_to.
      CONDENSE lv_value NO-GAPS.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VLR_TOT_FRM_USD'
          i_value     = lv_value.

    ENDLOOP.

    CALL METHOD grid7->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                      "on_data_changed_ins
  METHOD on_data_changed_finished_form.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

*    DATA: WL_FORM_LOTE LIKE LINE OF TG_FORM_LOTE,
*          TL_MAKT TYPE TABLE OF MAKT,
*          WL_MAKT TYPE MAKT,
*          WL_ZMENG TYPE EKPO-MENGE,
*          WL_ZIEME TYPE MARA-MEINS,
*          WL_PMEIN TYPE MARA-MEINS,
*          LS_GOOD  TYPE LVC_S_MODI.
*
*    IF TG_FORM_LOTE[] IS NOT INITIAL.
*      SELECT *
*        FROM MAKT
*        INTO TABLE TL_MAKT
*         FOR ALL ENTRIES IN TG_FORM_LOTE
*         WHERE MATNR EQ TG_FORM_LOTE-MATNR.
*
*    ENDIF.
*
*    LOOP AT ET_GOOD_CELLS INTO LS_GOOD.
*      READ TABLE TG_FORM_LOTE INTO WL_FORM_LOTE INDEX LS_GOOD-ROW_ID.
*
*      READ TABLE TL_MAKT INTO WL_MAKT WITH KEY MATNR = WL_FORM_LOTE-MATNR.
*
*      WL_FORM_LOTE-MAKTX = WL_MAKT-MAKTX.
*      WL_ZIEME = WL_FORM_LOTE-ZIEME.
*      WL_PMEIN = WL_FORM_LOTE-PMEIN.
*      WL_ZMENG = WL_FORM_LOTE-ZMENG.
*
*      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
*        EXPORTING
*          I_MATNR              = WL_FORM_LOTE-MATNR
*          I_IN_ME              = WL_ZIEME
*          I_OUT_ME             = WL_PMEIN
*          I_MENGE              = WL_ZMENG
*        IMPORTING
*          E_MENGE              = WL_ZMENG
*        EXCEPTIONS
*          ERROR_IN_APPLICATION = 1
*          ERROR                = 2
*          OTHERS               = 3.
*
*      WL_FORM_LOTE-VLRTOT = WL_FORM_LOTE-DMBTR * WL_ZMENG.
*      WL_FORM_LOTE-USD_TO = WL_FORM_LOTE-LIBRA_TO * C_USD.
*
*      IF SY-SUBRC IS INITIAL.
*        IF WL_FORM_LOTE-PMEIN = 'TO'
*        AND WL_FORM_LOTE-ZIEME = 'KG'.
*          WL_FORM_LOTE-VLR_TOT_FRM_USD = ( WL_FORM_LOTE-USD_TO * WL_ZMENG ) * 1000.       "Campo: Qtd.Prev.
*        ELSEIF WL_FORM_LOTE-PMEIN = 'KG'
*        AND WL_FORM_LOTE-ZIEME = 'TO'.
*          WL_FORM_LOTE-VLR_TOT_FRM_USD = ( WL_FORM_LOTE-USD_TO * WL_ZMENG ) / 1000.       "Campo: Qtd.Prev.
*        ELSE.
*          WL_FORM_LOTE-VLR_TOT_FRM_USD = WL_FORM_LOTE-USD_TO * WL_ZMENG.                               "Campo: Qtd.Prev.
*        ENDIF.
*      ELSE.
*        IF WL_FORM_LOTE-ZIEME = 'KG'.
*          WL_FORM_LOTE-VLR_TOT_FRM_USD = ( WL_FORM_LOTE-USD_TO * WL_FORM_LOTE-ZMENG ) / 1000.
*        ELSE.
*          WL_FORM_LOTE-VLR_TOT_FRM_USD = WL_FORM_LOTE-USD_TO * WL_FORM_LOTE-ZMENG.
*        ENDIF.
*      ENDIF.
*
*      MODIFY TABLE TG_FORM_LOTE FROM WL_FORM_LOTE TRANSPORTING VLRTOT MAKTX VLR_TOT_FRM_USD USD_TO.
*    ENDLOOP.

    CALL METHOD grid7->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "on_data_changed_finished_ins



  METHOD on_data_changed_finished_ped.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: ls_good   TYPE lvc_s_modi.

    LOOP AT tg_pedido[] ASSIGNING FIELD-SYMBOL(<wl_pedido>).
      IF ( <wl_pedido>-posnr IS INITIAL ).
        DATA(tl_pedido) = tg_pedido[].
        SORT tl_pedido[] BY posnr DESCENDING.
        READ TABLE tl_pedido[] INTO DATA(wl_pedido) INDEX 1.
        IF ( sy-subrc EQ 0 ).
          <wl_pedido>-posnr = wl_pedido-posnr + 000010.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CALL METHOD grid10->refresh_table_display.

  ENDMETHOD.


  METHOD changed_cond_esp.
    DATA: ls_good  TYPE lvc_s_modi.
    LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = ls_good-fieldname
          i_value     = ls_good-value.

    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED_COND_ESP
  METHOD changed_finished_cond_esp.

*   "// PBI-59125 Melhoria de perfomace Inicio
*    CHECK NOT e_modified IS INITIAL.
*    CHECK NOT et_good_cells IS INITIAL.
*   "// PBI-59125 Melhoria de perfomace Fim

    DATA: wl_cond_esp LIKE LINE OF tg_cond_esp,
          ls_good     TYPE lvc_s_modi,
          tl_t052     TYPE TABLE OF t052,
          wl_t052     TYPE t052,
          wl_itens    LIKE LINE OF tg_itens.

** Controle de campos de tela
    IF tg_cond_esp[] IS NOT INITIAL.
      CLEAR: wl_t052.
      SELECT *
        FROM t052
         INTO TABLE tl_t052
         FOR ALL ENTRIES IN tg_cond_esp
         WHERE zterm EQ tg_cond_esp-zterm.
    ENDIF.
    CLEAR: wl_cond_esp, wl_t052.
    LOOP AT et_good_cells INTO ls_good.
      REFRESH: style, tg_cond_esp-style.
      CLEAR: wa_style.

      READ TABLE tg_cond_esp INTO wl_cond_esp INDEX ls_good-row_id.
      READ TABLE tl_t052 INTO wl_t052
        WITH KEY zterm = wl_cond_esp-zterm.
      REFRESH: wl_cond_esp-style.
      IF wl_t052-zlsch EQ 'V'.
        wa_style-fieldname = 'QTE_VENC'.
        wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
        INSERT  wa_style INTO TABLE style .

      ELSE.
        CLEAR: wl_cond_esp-qte_venc.
        wa_style-fieldname = 'QTE_VENC'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        wl_cond_esp-qte_venc = 1.
      ENDIF.

      PERFORM: limpar_data_hedge_lib USING 'F' wl_cond_esp-fixacao.


      INSERT LINES OF style INTO TABLE wl_cond_esp-style.
      MODIFY tg_cond_esp FROM wl_cond_esp INDEX ls_good-row_id.
    ENDLOOP.

    IF obg_dialogbox IS NOT INITIAL.
      CALL METHOD grid9->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_COND_ESP
  METHOD on_onf4.

    TYPES: BEGIN OF tyl_instrucao,
             field(100),
           END OF tyl_instrucao,

           BEGIN OF tyl_ins_aux,
             instrucao TYPE zsdt0066-instrucao,
           END OF tyl_ins_aux,

           BEGIN OF tyl_field,
             tabname   TYPE dd03l-tabname,    "Nome da tabela
             fieldname TYPE dd03l-fieldname,    "Nome de campo
             s(1)      TYPE c,
           END OF tyl_field,

           BEGIN OF tyl_value,
             tabname    TYPE dd03l-tabname,    "Nome da tabela
             fieldname  TYPE dd03l-fieldname,    "Nome de campo
             char79(79) TYPE c,
           END OF tyl_value.

    DATA: tl_ins       TYPE TABLE OF tyl_instrucao,
          tl_ins_aux   TYPE TABLE OF tyl_ins_aux,
          wl_ins       TYPE tyl_instrucao,
          wl_ins_aux   TYPE tyl_ins_aux,
          wl_instrucao LIKE LINE OF tg_instrucao,
          wl_form_lote LIKE LINE OF tg_form_lote,
          tl_field     TYPE TABLE OF tyl_field,
          wl_field     TYPE tyl_field,
          tl_value     TYPE TABLE OF tyl_value,
          wl_value     TYPE tyl_value,
          wl_char(20),
          wl_index     TYPE sy-tabix.


    REFRESH: tl_ins.
    LOOP AT tg_instrucao INTO wl_instrucao.
      wl_ins-field = wl_instrucao-bukrs.
      APPEND wl_ins TO tl_ins.

      wl_ins-field = wl_instrucao-werks.
      APPEND wl_ins TO tl_ins.

      wl_ins-field = wl_instrucao-instrucao.
      APPEND wl_ins TO tl_ins.
      wl_ins_aux-instrucao = wl_instrucao-instrucao.
      APPEND wl_ins_aux TO tl_ins_aux.

      wl_ins-field = wl_instrucao-porto_embarque.
      APPEND wl_ins TO tl_ins.

      CLEAR: wl_ins_aux.
    ENDLOOP.
    wl_field-tabname = 'ZSDT0045'.
    wl_field-fieldname = 'BUKRS'.
    wl_field-s = 'X'.
    APPEND wl_field TO tl_field.

    wl_field-tabname = 'ZSDT0045'.
    wl_field-fieldname = 'WERKS'.
    wl_field-s = 'X'.
    APPEND wl_field TO tl_field.

    wl_field-tabname = 'ZSDT0066'.
    wl_field-fieldname = 'INSTRUCAO'.
    wl_field-s = 'X'.
    APPEND wl_field TO tl_field.

    wl_field-tabname = 'ZSDT0045'.
    wl_field-fieldname = 'PORTO_EMBARQUE'.
    wl_field-s = 'X'.
    APPEND wl_field TO tl_field.

    CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
      EXPORTING
*       cucol                     = '3'
        fieldname                 = 'INSTRUCAO'
        tabname                   = 'TG_FORM_LOTE'
      IMPORTING
        index                     = wl_index
        select_value              = wl_char
      TABLES
        fields                    = tl_field
        select_values             = tl_value
        valuetab                  = tl_ins
      EXCEPTIONS
        field_not_in_ddic         = 001
        more_then_one_selectfield = 002
        no_selectfield            = 003.

    IF sy-subrc IS INITIAL.
      CLEAR: wl_ins, wl_form_lote.
      READ TABLE tl_ins_aux INTO wl_ins_aux INDEX wl_index.
      READ TABLE tg_form_lote INTO wl_form_lote INDEX es_row_no-row_id.

      wl_form_lote-instrucao = wl_ins_aux-instrucao.
      MODIFY tg_form_lote FROM wl_form_lote INDEX es_row_no-row_id.
    ENDIF.
    CALL METHOD grid7->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "on_onf4

  METHOD calculo_desconto.

    DATA: vl_zmeng  TYPE zsded054,
          vl_dmbtr  TYPE dmbtr,
          vl_vlrtot TYPE dmbtr,
          diferenca TYPE dmbtr,
          vl_zieme  TYPE dzieme,
          vl_pmein  TYPE pmein,
          vl_desc   TYPE kbetr,
          lv_value  TYPE lvc_value,
          ls_good   TYPE lvc_s_modi.

    LOOP AT tg_itens ASSIGNING FIELD-SYMBOL(<itens>).
      IF sy-tabix NE linha.
        CONTINUE.
      ENDIF.

      vl_zmeng = <itens>-zmeng.  " // Quantidade
      vl_dmbtr = <itens>-dmbtr.  " // Preço
      vl_zieme = <itens>-zieme.  " // Unidade de Medida Quantidade
      vl_pmein = <itens>-pmein.  " // Unidade de Medida Preço

      IF vl_zmeng IS NOT INITIAL
     AND vl_dmbtr IS NOT INITIAL.
        IF vl_zieme NE vl_pmein.
          CASE 'KG'.
            WHEN vl_zieme.
              vl_zmeng = vl_zmeng / 1000.
            WHEN vl_pmein.
              vl_dmbtr = vl_dmbtr / 1000.
          ENDCASE.
        ENDIF.
      ENDIF.

      CASE wg_header-param_espec.
        WHEN c_p.

          vl_vlrtot = <itens>-vlrtot.

          diferenca = vl_vlrtot - ( vl_dmbtr * vl_zmeng ).
          <itens>-desc_absoluto = diferenca.

*-CS2021000615 - 17.06.2021 - JT - inicio
        WHEN c_a OR c_ax OR c_z  ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim

          diferenca = ( vl_dmbtr * vl_zmeng ).
          vl_desc = <itens>-desc_absoluto. "// Desconto Absoluto

          ADD vl_desc TO diferenca.
          <itens>-vlrtot = diferenca.

      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_cell.

    CALL METHOD er_data->get_cell_value
      EXPORTING
        i_row_id    = i_good-row_id
        i_tabix     = i_good-tabix
        i_fieldname = i_field
      IMPORTING
        e_value     = e_value.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

DATA: tree_event_receiver   TYPE REF TO lcl_tree_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.
* handle double_click
  METHOD handle_double_click.

    IF node_key GT 1.
      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'ITEM_KEY'
                                      'G'
                               CHANGING wg_valor.
*        CONDENSE WG_VALOR NO-GAPS.

        IF wg_valor EQ node_key.
          CLEAR node_key.
        ENDIF.
      ENDLOOP.
      IF node_key IS NOT INITIAL.
        PERFORM preco_frame_alv USING node_key.
      ENDIF.
    ENDIF.


  ENDMETHOD.                    "handle_double_click
* handle double_click
  METHOD handle_expand_no_children.
*    .
  ENDMETHOD.                    "handle_double_click
* handle double_click
  METHOD handle_item_double_click.
    IF node_key GT 1.
      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'ITEM_KEY'
                                      'G'
                               CHANGING wg_valor.

        IF wg_valor EQ node_key.
          CLEAR node_key.
        ENDIF.
      ENDLOOP.
      IF node_key IS NOT INITIAL.
        PERFORM preco_frame_alv USING node_key.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE
*&SPWIZARD: SETS ACTIVE TAB
MODULE tab_strip_active_tab_set OUTPUT.

*-CS2022000332-#78223-02.06.2022-JT-inicio
  IF wg_header-param_espec = 'A'.
    tab_strip_tab6 = icon_financing && 'Adiantamento Fornecedor'.
  ELSE.
    tab_strip_tab6 = icon_financing && 'Adiantamento Externo'.
  ENDIF.
*-CS2022000332-#78223-02.06.2022-JT-inicio

  "PERFORM verifica_erros. "Comentando 25.05.2021
  tab_strip-activetab = g_tab_strip-pressed_tab.

  CASE g_tab_strip-pressed_tab.
    WHEN c_tab_strip-tab1.
      g_tab_strip-subscreen = '0101'.
    WHEN c_tab_strip-tab2.
      g_tab_strip-subscreen = '0102'.
    WHEN c_tab_strip-tab4.
      g_tab_strip-subscreen = '0104'.
    WHEN c_tab_strip-tab5.
      g_tab_strip-subscreen = '0105'.
    WHEN c_tab_strip-tab6.
      g_tab_strip-subscreen = '0106'.
    WHEN c_tab_strip-tab7.
      g_tab_strip-subscreen = '0107'.
    WHEN c_tab_strip-tab8.
      g_tab_strip-subscreen = '0108'.
    WHEN c_tab_strip-tab9.
      g_tab_strip-subscreen = '0109'.
    WHEN c_tab_strip-tab3.
      IF wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z'.
        g_tab_strip-subscreen = '0113'.
      ELSE.
        g_tab_strip-subscreen = '0103'.
      ENDIF.
  ENDCASE.

ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_SET OUTPUT

*&SPWIZARD: INPUT MODULE FOR TS 'TAB_STRIP_NF'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
MODULE tab_strip_active_tab_get INPUT.
  ok_code = sy-ucomm.

  CASE ok_code.
    WHEN c_tab_strip-tab1.
      g_tab_strip-pressed_tab = c_tab_strip-tab1.
    WHEN c_tab_strip-tab2.
      g_tab_strip-pressed_tab = c_tab_strip-tab2.
    WHEN c_tab_strip-tab3.
      g_tab_strip-pressed_tab = c_tab_strip-tab3.
    WHEN c_tab_strip-tab4.
      g_tab_strip-pressed_tab = c_tab_strip-tab4.
    WHEN c_tab_strip-tab5.
      g_tab_strip-pressed_tab = c_tab_strip-tab5.
    WHEN c_tab_strip-tab6.
      g_tab_strip-pressed_tab = c_tab_strip-tab6.
    WHEN c_tab_strip-tab7.
      g_tab_strip-pressed_tab = c_tab_strip-tab7.
    WHEN c_tab_strip-tab8.
      g_tab_strip-pressed_tab = c_tab_strip-tab8.
    WHEN c_tab_strip-tab9.
      g_tab_strip-pressed_tab = c_tab_strip-tab9.
  ENDCASE.

  IF ( ok_code EQ 'MODIF').
    var_edit = 'X'.
  ENDIF.

  IF ( ok_code EQ 'MODIF_QTD' OR ok_code EQ 'REDIST').
    var_modred = 'MODRED'.
    MOVE var_modred TO modred_hedge.
  ENDIF.

ENDMODULE.                    "TAB_STRIP_NF_ACTIVE_TAB_GET INPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .

  TYPES: BEGIN OF tyl_vlr_adiant,
           posnr TYPE zsdt0053-posnr,
           dmbtr TYPE zsdt0053-dmbtr,
         END OF tyl_vlr_adiant.

  CREATE OBJECT obj_frete.


  DATA: wl_tvko               TYPE tvko,
        wl_tvtw               TYPE tvtw,
        wl_tvbur              TYPE tvbur,
        wl_tcurc              TYPE tcurc,
        wl_tvak               TYPE tvak,
        wl_tspa               TYPE tspa,
        wl_tvkgr              TYPE tvkgr,
        wl_kna1               TYPE kna1,
        wl_tinc               TYPE tinc,
        wl_tvlv               TYPE tvlv,
        wl_t042z              TYPE t042z,
        wl_t052               TYPE t052,
        wl_t012               TYPE t012,
        wl_mara               TYPE mara,
        wl_0057               TYPE zsdt0057,
        wl_0060               TYPE zsdt0060,
        wl_0075               TYPE zsdt0075,
        wl_linha(6),
        wl_duplicado,
        wl_matnr              TYPE mara-matnr,
        wl_zieme              TYPE mara-meins,
        wl_pmein              TYPE mara-meins,
        wl_menge              TYPE ekpo-menge,
        wl_zmeng_aux          TYPE zsdt0053-zmeng,
        wl_zmeng_aux2         TYPE zsdt0053-zmeng,
        wl_qtd_total          TYPE zsdt0053-zmeng,
        wl_qtd_total_item     TYPE zsdt0053-zmeng,
        wl_qtd_total_item_f   TYPE zsdt0053-zmeng,
        wl_qtd_total_item_aux TYPE dmbtr,
        wl_dmbtr              TYPE zsdt0053-vlrtot,
        wl_vlrtot             TYPE zsdt0053-vlrtot,
        tl_preco              LIKE TABLE OF tg_preco WITH HEADER LINE,
        tl_t001w              TYPE TABLE OF t001w WITH HEADER LINE,
        tl_t001l              TYPE TABLE OF t001l WITH HEADER LINE,
        tl_marc               TYPE TABLE OF marc WITH HEADER LINE,
        tl_mch1               TYPE TABLE OF mch1 WITH HEADER LINE,
        tl_t006               TYPE TABLE OF t006 WITH HEADER LINE,
        tl_mara               TYPE TABLE OF mara WITH HEADER LINE,
        tl_0061               TYPE TABLE OF zsdt0061 WITH HEADER LINE,
        tl_0053               TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_0060               TYPE TABLE OF zsdt0060 WITH HEADER LINE,
        tl_0064               TYPE TABLE OF zsdt0064 WITH HEADER LINE,
        tl_t052               TYPE TABLE OF t052 WITH HEADER LINE,
        tl_vlr_adiant         TYPE TABLE OF tyl_vlr_adiant WITH HEADER LINE,
        tl_itens              LIKE TABLE OF tg_itens WITH HEADER LINE,
        tl_vbfa               TYPE TABLE OF vbfa WITH HEADER LINE,
        tl_kna1               TYPE TABLE OF kna1 WITH HEADER LINE,
        tl_0086               TYPE TABLE OF zsdt0086 WITH HEADER LINE,
        wl_0032               TYPE TABLE OF zsdt0032 WITH HEADER LINE, " #192298 - 02-10-2025 - SMC
        wl_tabix              TYPE sy-tabix,
        wl_flag,
        vl_vlt_porto          TYPE zsdt0053-vlt_porto,

        wa_mvke_53            TYPE mvke,
        wa_mvke_66            TYPE mvke,
        wa_marc_53            TYPE marc,
        wa_marc_66            TYPE marc,
        wa_mard_53            TYPE mard,
        wa_mard_66            TYPE mard,
        wa_knvv_66            TYPE knvv,
        wa_knvv_53            TYPE knvv,
        wa_0108_aux           TYPE zsdt0108,
        wl_qtd_fixar          TYPE zsdt0053-zmeng,
        wl_qprod_total        TYPE zsdt0053-zmeng.

  DATA:
    wl_sqtd_valor TYPE string,
    " wl_mqtd_valor type FDT_S_QUANTITY.
    wl_mqtd_valor TYPE zsdt0053-zmeng.



  DATA: vg_last_day    TYPE sy-datum,
        vg_first_day   TYPE sy-datum,
        wl_sab_dom_fer TYPE TABLE OF iscal_day WITH HEADER LINE,
        s_subrc        TYPE sy-subrc.

  DATA: obj_util TYPE REF TO zcl_util.
  DATA: var_data TYPE c LENGTH 10.
  DATA: validacao_posicao TYPE c LENGTH 10.

  REFRESH: tg_msg_ret, tl_t001w, tl_t001l, tl_marc, tl_mch1, tl_mara, tl_t006,
           tl_0061, tl_0053, tl_vlr_adiant, tl_itens, tl_vbfa, tl_kna1, tl_0060,
           tl_t052.

  CLEAR: tg_msg_ret,    tl_t001w, tl_t001l, tl_marc,  tl_mch1,  tl_mara,  tl_vbfa,
         wl_tvko,       wl_tvtw,  wl_tvbur, wl_tcurc, wl_tvak,  wl_tspa,  wl_tvkgr,
         wl_kna1,       wl_tinc,  wl_tvlv,  wl_t042z, wl_t052,  wl_t012,  wl_linha,
         wl_qtd_total,  wl_qtd_total_item,  wl_0057,  wl_duplicado,       wl_mara,
         wl_0060,       wl_vlrtot,          wl_dmbtr, tl_0060,  wl_flag,  wl_0075,
         wa_mvke_53,    wa_mvke_66,         wa_marc_53,         wa_marc_66,
         wa_mard_53,    wa_mard_66,         wa_knvv_66,         wa_knvv_53,wl_qtd_fixar.

  IF <fs_table> IS ASSIGNED.
    UNASSIGN <fs_line>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.
  ENDIF.

  SELECT SINGLE *
   FROM mara
   INTO wl_mara
    WHERE matnr = wg_header-matnr.

  SELECT SINGLE *
   FROM zsdt0057
   INTO wl_0057
    WHERE tp_venda = wg_header-tp_venda.

* #192298 - 02-10-2025 - SMC
  SELECT SINGLE *
   FROM zsdt0032
   INTO wl_0032
    WHERE cliente = wg_header-kunnr
    AND inativo <> 'X'.
* #192298 - 02-10-2025 - SMC

  SELECT *
   FROM zsdt0086
   INTO TABLE tl_0086
    WHERE tp_venda = wg_header-tp_venda.

  SELECT SINGLE *
   FROM tvbur
   INTO wl_tvbur
    WHERE vkbur = wg_header-vkbur.

  SELECT SINGLE *
    FROM tvko
    INTO wl_tvko
     WHERE vkorg = wg_header-vkorg.

  SELECT SINGLE *
    FROM tvtw
    INTO wl_tvtw
     WHERE vtweg = wg_header-vtweg.

  SELECT SINGLE *
    FROM tcurc
    INTO wl_tcurc
     WHERE waers EQ wg_header-waerk.

  SELECT SINGLE *
    FROM tvak
    INTO wl_tvak
     WHERE auart EQ wg_header-auart.

  SELECT SINGLE *
    FROM tspa
    INTO wl_tspa
     WHERE spart EQ wg_header-spart.

  SELECT SINGLE *
   FROM tvkgr
   INTO wl_tvkgr
    WHERE vkgrp EQ wg_header-vkgrp.

  SELECT SINGLE *
   FROM kna1
   INTO wl_kna1
    WHERE kunnr EQ wg_header-kunnr.

  SELECT SINGLE *
   FROM tinc
   INTO wl_tinc
    WHERE inco1 EQ wg_header-inco1.

  SELECT SINGLE *
   FROM tcurc
   INTO wl_tcurc
    WHERE waers EQ wg_header-waerk.

  SELECT SINGLE *
   FROM tvlv
   INTO wl_tvlv
    WHERE abrvw EQ wg_header-vkaus.

  SELECT SINGLE *
   FROM t042z
   INTO wl_t042z
    WHERE zlsch EQ wg_cond_pgt-zlsch.

  SELECT SINGLE *
    FROM t052
    INTO wl_t052
     WHERE zterm EQ wg_cond_pgt-zterm.

  SELECT SINGLE *
    FROM t012
    INTO wl_t012
     WHERE hbkid EQ wg_cond_pgt-hbkid.

  " 25.04.2025 - RAMON - tem um bug nessa seleção da tolerancia, teria que passar o escritorio de vendas,
  " se nao, vai ser atualizado a tolerancia e ele sempre pega o primeiro
  SELECT SINGLE *
    FROM zsdt0060
     INTO wl_0060
     WHERE usnam EQ sy-uname
      AND  programa EQ sy-cprog.

  SELECT *
    FROM zsdt0060
     INTO TABLE tl_0060
     WHERE usnam EQ sy-uname
      AND  programa EQ sy-cprog.

  SELECT *
    FROM zsdt0064
    INTO TABLE tl_0064
  WHERE uname EQ sy-uname
    AND werks EQ wg_header-vkbur.

  SELECT *
    FROM zsdt0061
    INTO TABLE tl_0061
     WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

  SELECT *
    FROM zsdt0053
    INTO TABLE tl_0053
     WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

  SELECT SINGLE *
     FROM knvv
     INTO wa_knvv_53
     WHERE kunnr EQ wg_header-kunnr
       AND vkorg EQ wg_header-vkorg
       AND vtweg EQ wg_header-vtweg
       AND spart EQ wg_header-spart.

  IF wa_knvv_53 IS INITIAL AND wg_header-vkorg NE '0101'.
    CONCATENATE TEXT-m35 wg_header-vkorg '/'
                TEXT-m36 wg_header-vtweg '/'
                TEXT-m37 wg_header-spart INTO  tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  SELECT SINGLE *
     FROM knb1
     INTO @DATA(wa_knb1)
     WHERE kunnr EQ @wg_header-kunnr
       AND bukrs EQ @wg_header-vkorg.

  IF wa_knb1 IS INITIAL AND wg_header-vkorg EQ '0101'.
    APPEND VALUE #( msg = |{ TEXT-m35 } { wg_header-vkorg }| ) TO tg_msg_ret.
  ENDIF.

* INICIO PARA ZSDT0066
*##############    VERIFICA SE EXISTE EXPANSÃO PARA O CENTRO   #################

  IF tg_form_lote[] IS NOT INITIAL.
    IF tg_form_lote-matnr IS NOT INITIAL.
      IF tg_form_lote-werks IS NOT INITIAL.
        IF tg_form_lote-lgort IS NOT INITIAL.

          SELECT SINGLE *
             FROM mard
             INTO wa_mard_66
             WHERE matnr EQ tg_form_lote-matnr
               AND werks EQ tg_form_lote-werks
               AND lgort EQ tg_form_lote-lgort.

        ENDIF.

        SELECT SINGLE *
           FROM marc
           INTO wa_marc_66
           WHERE matnr EQ tg_form_lote-matnr
             AND werks EQ tg_form_lote-werks.

      ENDIF.

      IF wg_header-vkorg IS NOT INITIAL.

        SELECT SINGLE *
            FROM mvke
            INTO wa_mvke_66
            WHERE matnr EQ tg_form_lote-matnr
              AND vkorg EQ wg_header-vkorg
              AND vtweg EQ '10'.

      ENDIF.
    ENDIF.

** INICIO VERIFICA EMISSOR
    SELECT SINGLE *
      FROM knvv
      INTO wa_knvv_66
      WHERE kunnr EQ tg_form_lote-kunnr
        AND vkorg EQ wg_header-vkorg
        AND vtweg EQ '10'
        AND spart EQ wg_header-spart.
** FIM VERIFICA EMISSOR

  ENDIF.

*##############    VERIFICA SE EXISTE EXPANSÃO PARA O CENTRO   #################
* FIM PARA ZSDT0066


  SORT tl_0061 BY posnr item ASCENDING.

  IF tg_itens[] IS NOT INITIAL.
    SELECT *
       FROM t001w
       INTO TABLE tl_t001w
        FOR ALL ENTRIES IN tg_itens
        WHERE werks EQ tg_itens-werks.

    SELECT *
      FROM marc
      INTO TABLE tl_marc
       FOR ALL ENTRIES IN tg_itens
       WHERE werks EQ tg_itens-werks
         AND matnr EQ tg_itens-matnr.

    SELECT *
      FROM mch1
      INTO TABLE tl_mch1
       FOR ALL ENTRIES IN tg_itens
       WHERE charg EQ tg_itens-charg
         AND matnr EQ tg_itens-matnr.

    SELECT *
      FROM mara
      INTO TABLE tl_mara
       FOR ALL ENTRIES IN tg_itens
       WHERE matnr EQ tg_itens-matnr.

    SELECT *
      FROM t006
      INTO TABLE tl_t006
       FOR ALL ENTRIES IN tg_itens
       WHERE msehi EQ tg_itens-pmein.

    SELECT *
       FROM t001l
       INTO TABLE tl_t001l
        FOR ALL ENTRIES IN tg_itens
        WHERE werks EQ tg_itens-werks
          AND lgort EQ tg_itens-lgort.


* INICIO PARA ZSDT0053
*##############    VERIFICA SE EXISTE EXPANSÃO PARA O MATERIAL   ##############
    SELECT SINGLE *
       FROM mvke
       INTO wa_mvke_53
       WHERE vkorg EQ wg_header-vkorg
         AND vtweg EQ wg_header-vtweg
         AND matnr EQ tg_itens-matnr.

    SELECT SINGLE *
       FROM marc
       INTO wa_marc_53
       WHERE matnr EQ tg_itens-matnr
         AND werks EQ tg_itens-werks.

    SELECT SINGLE *
       FROM mard
       INTO wa_mard_53
       WHERE matnr EQ tg_itens-matnr
         AND werks EQ tg_itens-werks
         AND lgort EQ tg_itens-lgort.

*##############    VERIFICA SE EXISTE EXPANSÃO PARA O CENTRO   #################
* FIM PARA ZSDT0053

*** Redistribuição de Quantidade
    IF wg_redistribuir EQ c_x.

      tl_itens[] = tg_itens[].
      DELETE tl_itens WHERE vbeln IS INITIAL.

      IF tl_itens[] IS NOT INITIAL.

        SELECT *
          FROM vbfa
          INTO TABLE tl_vbfa
           FOR ALL ENTRIES IN tl_itens
           WHERE vbelv EQ tl_itens-vbeln
             AND vbtyp_n EQ 'J'
             AND vbtyp_v EQ 'C'
             AND plmin   EQ '+'.

        SORT tl_vbfa BY vbelv.
        REFRESH: tg_saldo.

        LOOP AT tl_itens.
          CLEAR: tg_saldo.

          LOOP AT tl_vbfa WHERE vbelv EQ tl_itens-vbeln.

            tg_saldo-vbeln = tl_vbfa-vbelv.
            tg_saldo-werks = tl_itens-werks.
            tg_saldo-total = tl_vbfa-rfmng.

            COLLECT tg_saldo.
            wl_tabix = sy-tabix.
*        CLEAR TG_SALDO.
          ENDLOOP.

          IF sy-subrc IS INITIAL.
            READ TABLE tg_saldo INDEX wl_tabix.
          ENDIF.

          wl_matnr = tl_itens-matnr.
          wl_zieme = tl_itens-zieme.
          wl_pmein = tl_vbfa-meins.
          wl_menge = tl_itens-zmeng.

          IF tl_vbfa IS NOT INITIAL.
            CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
              EXPORTING
                i_matnr              = wl_matnr
                i_in_me              = wl_zieme
                i_out_me             = wl_pmein
                i_menge              = wl_menge
              IMPORTING
                e_menge              = wl_menge
              EXCEPTIONS
                error_in_application = 1
                error                = 2
                OTHERS               = 3.
          ENDIF.

          tg_saldo-zmeng = wl_menge.
          tg_saldo-saldo = tg_saldo-zmeng - tg_saldo-total.

          IF wl_tabix IS NOT INITIAL.
            MODIFY tg_saldo INDEX wl_tabix.
          ELSE.

            tg_saldo-vbeln = tl_itens-vbeln.
            tg_saldo-werks = tl_itens-werks.
            APPEND tg_saldo.
          ENDIF.

          CLEAR: wl_tabix.
*          ENDIF.
        ENDLOOP.
      ENDIF.

      tl_itens[] = tg_itens[].
      DELETE tl_itens WHERE kunnr IS  INITIAL.
      IF tl_itens[] IS NOT INITIAL.

        SELECT *
          FROM kna1
          INTO TABLE tl_kna1
           FOR ALL ENTRIES IN tl_itens
            WHERE kunnr EQ tl_itens-kunnr.

      ENDIF.
    ENDIF.
  ENDIF.

  IF tg_cond_esp[] IS NOT INITIAL.

    SELECT *
      FROM t052
      INTO TABLE tl_t052
      FOR ALL ENTRIES IN tg_cond_esp
        WHERE zterm EQ tg_cond_esp-zterm.

  ENDIF.

  "" inicio Dados Gerais

*-CS2022000332-#78223-02.06.2022-JT-inicio
  IF wg_header-param_espec = c_a OR
     wg_header-param_espec = c_x OR
    wg_header-param_espec = c_z ." CS2020000373 23/08/2022 -LP
    IF wg_header-id_contrato IS INITIAL.
      MOVE: 'WG_HEADER-ID_CONTRATO'      TO tg_msg_ret-field.
      CONCATENATE TEXT-e01 TEXT-ee9'.' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    SELECT SINGLE tp_venda
      INTO @DATA(l_tpvenda)
      FROM zsdt0143
     WHERE id_contrato = @wg_header-id_contrato
      AND  tp_venda    <> '00000'.

    IF sy-subrc = 0 AND wg_header-tp_venda <> l_tpvenda.
      MOVE: 'WG_HEADER-TP_VENDA'          TO tg_msg_ret-field.
      CONCATENATE TEXT-w31 TEXT-w32 '.' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
*-CS2022000332-#78223-02.06.2022-JT-inicio

** Tipo de Venda (TP_VENDA)
  IF wg_header-tp_venda IS INITIAL.

    MOVE: 'WG_HEADER-TP_VENDA'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e32'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ELSEIF wl_0057-tp_venda IS INITIAL.

    MOVE: 'WG_HEADER-TP_VENDA'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e32 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ELSEIF wl_0057-status IS INITIAL.

    IF wg_acao EQ 'ADD'.
      MOVE: 'WG_HEADER-TP_VENDA'       TO tg_msg_ret-field.
      tg_msg_ret-msg = TEXT-e33.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ELSEIF tp_venda_error EQ c_i.
      MOVE: 'WG_HEADER-TP_VENDA'       TO tg_msg_ret-field.
      tg_msg_ret-msg = TEXT-e33.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

** Material (MATNR)
  IF wg_header-matnr IS INITIAL.
    MOVE: 'WG_HEADER-MATNR'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e34 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF wg_header-auart NE 'ZEXI' AND
     wg_header-auart NE 'ZEXP' AND
     wg_header-auart NE 'ZPER' .


    IF wl_0032 IS INITIAL. " #192298 - 02-10-2025 - SMC ><

      IF wg_header-tx_multa IS INITIAL.
        MOVE: 'WG_HEADER-TX_MULTA'       TO tg_msg_ret-field.
        CONCATENATE TEXT-e01 TEXT-m36 INTO tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF wg_header-tx_juros IS INITIAL.
        MOVE: 'WG_HEADER-TX_JUROS'       TO tg_msg_ret-field.
        CONCATENATE TEXT-e01 TEXT-m37 INTO tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
    ENDIF.
    CLEAR: wl_0032." #192298 - 02-10-2025 - SMC ><
    FREE: wl_0032." #192298 - 02-10-2025 - SMC ><
  ENDIF.

** Data de Venda(DATA_VENDA)
  IF wg_header-data_venda IS INITIAL.
    MOVE: 'WG_HEADER-DATA_VENDA'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e35 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.


** Organização de Vendas (VKORG)
  IF wg_header-vkorg IS INITIAL.
    MOVE: 'WG_HEADER-VKORG'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e36'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvko-vkorg IS INITIAL.

    MOVE: 'WG_HEADER-VKORG'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e36 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.


** Canal de Distribuição (VTWEG)
  IF wg_header-vtweg IS INITIAL.
    MOVE: 'WG_HEADER-VTWEG'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e37'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvtw-vtweg IS INITIAL.
    MOVE: 'WG_HEADER-VTWEG'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e37 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  " 08.02.2024 - RAMON - campo porto -->

  IF wg_header-param_espec = c_z.

    " campo porto
    IF wg_header-porto IS INITIAL.
      MOVE: 'WG_HEADER-PORTO'       TO tg_msg_ret-field.
      CONCATENATE TEXT-e01 'Porto' INTO tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

  " 08.02.2024 - RAMON - campo porto --<

** Setor de Atividades (SPART)
  IF wg_header-spart IS INITIAL.
    MOVE: 'WG_HEADER-SPART'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e38'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tspa-spart IS INITIAL.
    MOVE: 'WG_HEADER-SPART'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e38 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.


** Escritório de vendas (VKBUR)
  IF wg_header-vkbur IS INITIAL.
    MOVE: 'WG_HEADER-VKBUR'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e39'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvbur-vkbur IS INITIAL.
    MOVE: 'WG_HEADER-VKBUR'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e39 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  CASE ok_code.
    WHEN: 'LIBERAR'.
      READ TABLE tl_0064 WITH KEY uname = sy-uname
                                  werks = wg_header-vkbur.

      IF sy-subrc IS NOT INITIAL.
        MOVE: 'WG_HEADER-VKBUR'       TO tg_msg_ret-field,
              TEXT-e22                TO tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.


      ENDIF.
    WHEN: 'ADD' OR 'ATUAL' OR 'SEARCH' OR 'SHOW_MSGRE' OR 'SAVE'.

      READ TABLE tl_0060 WITH KEY vkbur = wg_header-vkbur.
      IF sy-subrc IS NOT INITIAL.
        MOVE: 'WG_HEADER-VKBUR'       TO tg_msg_ret-field,
              TEXT-e22                TO tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
  ENDCASE.

** Vendedor (VKGRP)
  IF wg_header-vkgrp IS INITIAL.
    MOVE: 'WG_HEADER-VKGRP'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e40'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvkgr-vkgrp IS INITIAL.
    MOVE: 'WG_HEADER-VKGRP'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e40 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.


** Incoterms 1 (INCO1)
  IF wg_header-inco1 IS INITIAL.
    MOVE: 'WG_HEADER-INCO1'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e41'1.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tinc-inco1 IS INITIAL.
    MOVE: 'WG_HEADER-INCO1'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e41 '1' TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_header-inco1 EQ 'CIF'.

** Programação Logistica (DTDE_LOGIST)
    IF wg_header-dtde_logist IS INITIAL OR wg_header-dtate_logist IS INITIAL.
      APPEND VALUE #( field = 'WG_HEADER-DTATE_LOGIST' msg = | { TEXT-e01 } { TEXT-e42 }.| )  TO tg_msg_ret.
    ELSEIF wg_header-dtde_logist EQ wg_header-dtate_logist.
      APPEND VALUE #( field = 'WG_HEADER-DTATE_LOGIST' msg = | { TEXT-e42 } { TEXT-e13 }| )  TO tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_header-dtde_logist > wg_header-dtate_logist.
    APPEND VALUE #( field = 'WG_HEADER-DTATE_LOGIST' msg = | { TEXT-e42 } { TEXT-ee4 }| )  TO tg_msg_ret.
  ENDIF.

  CASE wg_header-risco_sacado.
    WHEN: 'S'.

      IF tg_logistica[] IS INITIAL.

        APPEND VALUE #(
                aba   = c_tab_strip-tab5
                obj   = 'GRID3'
                tabix = wl_linha
                msg   = |É Obrigatório o preenchimento da ABA Logística!|
        ) TO tg_msg_ret.

      ENDIF.

      LOOP AT tg_logistica.
        IF ( tg_logistica-valdt_hedge IS INITIAL ).
          MOVE: 'VALDT_HEDGE'             TO tg_msg_ret-field,
                c_tab_strip-tab5          TO tg_msg_ret-aba,
                'GRID3'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e01 TEXT-e43 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDLOOP.
  ENDCASE.

  "Verificação da data de vencimento.
  IF ( ok_code EQ c_liberar ).
    IF NOT tg_itens[]  IS INITIAL.
      LOOP AT tg_itens.

        IF NOT ( tg_itens-valdt IS INITIAL ) AND ( tg_itens-valdt < sy-datum ).
          IF ( ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z' ) OR ( wg_header-param_espec IS INITIAL ) ).

            MOVE: 'TG_ITENS-VALDT'  TO tg_msg_ret-field.

            CREATE OBJECT obj_util.
            var_data = obj_util->conv_data_us_br( i_data  = tg_itens-valdt
                                                  i_opcao = '/' ).
            FREE obj_util.

            CONCATENATE TEXT-e45 var_data TEXT-e46 INTO  tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF NOT ( wg_cond_pgt-valdt IS INITIAL ) AND ( wg_cond_pgt-valdt < sy-datum ) AND ( wg_header-param_espec NE 'P' ).
      MOVE: 'WG_COND_PGT-VALDT'  TO tg_msg_ret-field.
      CREATE OBJECT obj_util.
      var_data = obj_util->conv_data_us_br( i_data  = tg_itens-valdt
                                            i_opcao = '/' ).
      FREE obj_util.
      CONCATENATE TEXT-e45 var_data TEXT-e47 INTO  tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF NOT ( tg_pgt_ant[]  IS INITIAL ).
      LOOP AT tg_pgt_ant.

        IF ( tg_pgt_ant-valdt < sy-datum ) AND ( ( wg_header-param_espec IS INITIAL ) OR ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z' ) ).

          MOVE: 'TG_ITENS-VALDT'  TO tg_msg_ret-field.

          CREATE OBJECT obj_util.
          var_data = obj_util->conv_data_us_br( i_data  = tg_pgt_ant-valdt
                                                i_opcao = '/' ).
          FREE obj_util.

          CONCATENATE TEXT-e45 var_data TEXT-e48 INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.

  FREE wl_linha.
  IF NOT ( tg_pgt_ant[]  IS INITIAL ).
    LOOP AT tg_pgt_ant.
      wl_linha = sy-tabix.

      obj_frete->dia_util( EXPORTING p_vencimento = tg_pgt_ant-valdt
                           IMPORTING e_subrc      = s_subrc ).

      IF s_subrc = 0.
        CONCATENATE TEXT-d22 TEXT-e44 wl_linha  INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDLOOP.
  ENDIF.

** Código de utilização (VKAUS)
  IF wg_header-vkaus IS INITIAL.
    MOVE: 'WG_HEADER-VKAUS'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e49 INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tvlv-abrvw IS INITIAL.
    MOVE: 'WG_HEADER-VKAUS'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e49 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

** Moeda do documento (WAERK)
  IF wg_header-waerk IS INITIAL.
    MOVE: 'WG_HEADER-WAERK'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e50'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_tcurc-waers IS INITIAL.
    MOVE: 'WG_HEADER-WAERK'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e50 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

** Emissor da Ordem (KUNNR)
  IF wg_header-kunnr IS INITIAL.
    MOVE: 'WG_HEADER-KUNNR'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e51'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_kna1-kunnr IS INITIAL.
    MOVE: 'WG_HEADER-KUNNR'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e51 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.


** Observação (PB_OBS)
  IF wg_header-observacao IS INITIAL.
    MOVE: 'WG_HEADER-OBSERVACAO'       TO tg_msg_ret-field.
    CONCATENATE TEXT-e01 TEXT-e52'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

** Quantidade Fixações (NUM_FIXACAO)
  IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
    IF wg_header-num_fixacao IS INITIAL.
      MOVE: 'WG_HEADER-NUM_FIXACAO'       TO tg_msg_ret-field.
*          C_TAB_STRIP-TAB1          TO TG_MSG_RET-ABA.
      CONCATENATE TEXT-e01 TEXT-e53'.' INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      DESCRIBE TABLE tg_itens LINES wl_linha.
      LOOP AT tg_cond_esp.
        IF tg_cond_esp-qte_venc GT 0.
          SUBTRACT tg_cond_esp-qte_venc FROM wl_linha.
        ELSE.
          SUBTRACT 1 FROM wl_linha.
        ENDIF.
      ENDLOOP.
*      IF WG_HEADER-NUM_FIXACAO NE WL_LINHA.
      IF wl_linha NE 0 AND wg_header-param_espec NE c_z .
        MOVE: 'WG_HEADER-NUM_FIXACAO'       TO tg_msg_ret-field,
              TEXT-e25                      TO tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
  ENDIF.


*** Forma de pagamento (ZLSCH)
  IF wg_cond_pgt-zlsch IS INITIAL.
    MOVE: 'WG_COND_PGT-ZLSCH'       TO tg_msg_ret-field,
          c_tab_strip-tab1          TO tg_msg_ret-aba.
    CONCATENATE TEXT-e01 TEXT-e54'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_t042z-zlsch IS INITIAL.
    MOVE: 'WG_COND_PGT-ZLSCH'       TO tg_msg_ret-field,
          c_tab_strip-tab1          TO tg_msg_ret-aba.
    CONCATENATE TEXT-e54 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ELSE.
*    IF WG_HEADER-PARAM_ESPEC NE C_P.
    IF wl_0057-boleto IS NOT INITIAL.
      SELECT SINGLE *
        FROM zsdt0075
        INTO wl_0075
         WHERE kunnr EQ wg_header-kunnr
           AND vkorg EQ wg_header-vkorg
           AND bdatu GE sy-datum.
      IF sy-subrc IS NOT  INITIAL.

        SELECT SINGLE *
          FROM zsdt0075
          INTO wl_0075
           WHERE kunnr EQ wg_header-kunnr
             AND vkorg EQ space
             AND bdatu GE sy-datum..
      ENDIF.


      IF  wg_cond_pgt-zlsch NE c_e AND wg_cond_pgt-pgto_ant EQ 'N'.

        MOVE: 'WG_COND_PGT-ZLSCH'       TO tg_msg_ret-field,
              c_tab_strip-tab1          TO tg_msg_ret-aba.
        CONCATENATE TEXT-e54 TEXT-e26 INTO tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

      "      IF SY-SUBRC IS  INITIAL.
*      AND WG_COND_PGT-ZLSCH NE C_E
*      AND WG_COND_PGT-PGTO_ANT IS NOT INITIAL.
*        MOVE: 'WG_COND_PGT-ZLSCH'       TO TG_MSG_RET-FIELD,
*              C_TAB_STRIP-TAB1          TO TG_MSG_RET-ABA.
*        CONCATENATE 'Forma de pagamento' text-E26 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
*      ELSEIF SY-SUBRC IS NOT INITIAL.
*        IF WG_COND_PGT-PGTO_ANT EQ C_N.
*          MOVE: 'WG_COND_PGT-PGTO_ANT'       TO TG_MSG_RET-FIELD,
*                C_TAB_STRIP-TAB1             TO TG_MSG_RET-ABA.
*          CONCATENATE 'Pagamento Antecipado' text-E26 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*          APPEND TG_MSG_RET.
*          CLEAR: TG_MSG_RET.

      IF wg_cond_pgt-pgto_ant EQ c_x.
        "AND WG_COND_PGT-ZLSCH NE C_E.
        IF ( wg_cond_pgt-zlsch NE c_e AND wg_cond_pgt-zlsch NE c_d ).

          MOVE: 'WG_COND_PGT-ZLSCH'       TO tg_msg_ret-field,
              c_tab_strip-tab1          TO tg_msg_ret-aba.
          CONCATENATE TEXT-e54 TEXT-e26 INTO tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

*        ELSEIF WG_COND_PGT-PGTO_ANT IS INITIAL
*           AND WG_COND_PGT-ZLSCH NE C_D.
*          MOVE: 'WG_COND_PGT-ZLSCH'       TO TG_MSG_RET-FIELD,
*                      C_TAB_STRIP-TAB1          TO TG_MSG_RET-ABA.
*          CONCATENATE 'Forma de pagamento' text-E26 INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*          APPEND TG_MSG_RET.
*          CLEAR: TG_MSG_RET.
*        ENDIF.
      "      ENDIF.

    ENDIF.
  ENDIF.
  IF wg_header-param_espec NE c_m.
*** Cond.Pagamento (ZTERM)

    IF wg_cond_pgt-zterm IS INITIAL.
      MOVE: 'WG_COND_PGT-ZTERM'       TO tg_msg_ret-field,
            c_tab_strip-tab1          TO tg_msg_ret-aba.
      CONCATENATE TEXT-e01 TEXT-e55'.' INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF wl_t052-zterm IS INITIAL.
      MOVE: 'WG_COND_PGT-ZTERM'       TO tg_msg_ret-field,
            c_tab_strip-tab1          TO tg_msg_ret-aba.
      CONCATENATE TEXT-e55 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

*** Quantidade de Vencimento (QTE_VENC)
  IF wg_cond_pgt-qte_venc IS INITIAL
     AND wl_t052-zlsch EQ 'V'.
    MOVE: 'WG_COND_PGT-QTE_VENC'    TO tg_msg_ret-field,
          c_tab_strip-tab1          TO tg_msg_ret-aba.
    CONCATENATE TEXT-e01 TEXT-e56'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

*** Banco Empresa (HBKID)
  IF wg_cond_pgt-hbkid IS INITIAL.
    MOVE: 'WG_COND_PGT-HBKID'       TO tg_msg_ret-field,
          c_tab_strip-tab1          TO tg_msg_ret-aba.
    CONCATENATE TEXT-e01 TEXT-e57'.' INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wl_t012-hbkid IS INITIAL.
    MOVE: 'WG_COND_PGT-HBKID'       TO tg_msg_ret-field,
          c_tab_strip-tab1          TO tg_msg_ret-aba.
    CONCATENATE TEXT-e57 TEXT-e02 INTO tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

*  Aba - Produto\Quantidade >
*** Tabela TG_ITENS ()
  IF wg_header-param_espec NE c_p
  AND wg_header-param_espec NE c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
  AND wg_header-param_espec NE c_ax
*-CS2021000615 - 17.06.2021 - JT - fim
  AND wg_header-param_espec NE c_e
  AND wg_header-param_espec NE c_f
   AND wg_header-param_espec NE c_z.
    IF tg_itens[] IS INITIAL.
      MOVE: c_tab_strip-tab2          TO tg_msg_ret-aba.
      CONCATENATE TEXT-e06 TEXT-e58 INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.
  ENDIF.

  "" Fim Dados Gerais

  LOOP AT tg_itens.

    wl_linha = sy-tabix.
*** Material (MATNR)
    IF tg_itens-matnr IS INITIAL.
      MOVE: 'MATNR'                   TO tg_msg_ret-field,
            c_tab_strip-tab2          TO tg_msg_ret-aba,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e34 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tg_itens-matnr IS NOT INITIAL.
      READ TABLE tl_mara
        WITH KEY matnr = tg_itens-matnr.

      IF sy-subrc IS NOT INITIAL.
        MOVE: 'MATNR'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e34 TEXT-e02 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

**    Verifica se o grupo corresponde a soja e milho
**    em caso de positivo obriga o preenchimento do terminal
      CASE tl_mara-matkl.

        WHEN '700110' OR '700170'.
          IF tg_itens-terminal IS INITIAL.

            MOVE: 'TERMINAL'          TO tg_msg_ret-field,
            c_tab_strip-tab2          TO tg_msg_ret-aba,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e01 TEXT-e59 TEXT-e44 wl_linha INTO tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.

          ENDIF.

          IF NOT tg_itens-lgort IS INITIAL AND
             NOT tg_itens-terminal IS INITIAL.

            CLEAR: wa_0108_aux.

            SELECT SINGLE *
              FROM zsdt0108
                INTO wa_0108_aux
                  WHERE terminal EQ tg_itens-terminal
                       AND lgort EQ tg_itens-lgort.

            IF NOT sy-subrc IS INITIAL.

              MOVE: 'LGORT'                   TO tg_msg_ret-field,
                    c_tab_strip-tab2          TO tg_msg_ret-aba,
                    'GRID1'                   TO tg_msg_ret-obj,
                    wl_linha                  TO tg_msg_ret-tabix.

              CONCATENATE TEXT-d20 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.


            ENDIF.
          ENDIF.
      ENDCASE.

      " 19.03.2025 ---- RAMON -->
      IF tg_itens-pmein IS NOT INITIAL.

        DATA(lt_unidade) = zcl_material=>get_instance( )->get_zsdt0371_table_by_matnr( tg_itens-matnr ).

        READ TABLE lt_unidade TRANSPORTING NO FIELDS
          WITH KEY matkl = tl_mara-matkl
                   mseh3 = tg_itens-pmein.

        IF sy-subrc NE 0.

          MOVE: 'PMEIN'          TO tg_msg_ret-field,

          c_tab_strip-tab2          TO tg_msg_ret-aba,

          'GRID1'                   TO tg_msg_ret-obj,

          wl_linha                  TO tg_msg_ret-tabix.

          CONCATENATE 'Un.Medida' tg_itens-pmein 'não cadastrada' wl_linha INTO tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ENDIF.


      ENDIF.
      " 19.03.2025 ---- RAMON --<

    ENDIF.

    IF tg_itens-kvgr5 IS NOT INITIAL.
      CASE tg_itens-kvgr5.
        WHEN '001' OR '002'.
        WHEN OTHERS.
          APPEND VALUE #(
                          field = 'KVGR5'
                          aba = c_tab_strip-tab2
                          obj = 'GRID1'
                          tabix = wl_linha
                          msg = |Valor Frete Entrada invalido! "001 Não" - "002 Sim". Linha: { wl_linha }|
          ) TO tg_msg_ret.
      ENDCASE.

    ENDIF.

    IF tg_itens-auart IS NOT INITIAL.

      SELECT COUNT(*)
        FROM tvak
       WHERE auart EQ tg_itens-auart.

      IF sy-subrc IS NOT INITIAL.
        APPEND VALUE #(
                        field = 'AUART'
                        aba = c_tab_strip-tab2
                        obj = 'GRID1'
                        tabix = wl_linha
                        msg = |Tipo de Venda { tg_itens-auart } não Existe { wl_linha }|
        ) TO tg_msg_ret.
      ENDIF.

    ENDIF.

*** Centro (WERKS)

    IF tg_itens-werks IS INITIAL.
      MOVE: 'WERKS'                   TO tg_msg_ret-field,
            c_tab_strip-tab2          TO tg_msg_ret-aba,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e34 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tg_itens-werks IS NOT INITIAL
       AND tg_itens-matnr IS NOT INITIAL.
      READ TABLE tl_marc
        WITH KEY matnr = tg_itens-matnr
                 werks = tg_itens-werks.

      IF sy-subrc IS NOT INITIAL.
        MOVE: 'WERKS'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e03 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.


*** Depósito (LGORT)
    IF wg_header-auart NE 'ZSER'.

      IF tg_itens-lgort IS INITIAL.
        MOVE: 'LGORT'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e60 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSEIF tg_itens-lgort IS NOT INITIAL
         AND tg_itens-werks IS NOT INITIAL.
        READ TABLE tl_t001l
          WITH KEY werks = tg_itens-werks
                   lgort = tg_itens-lgort.

        IF sy-subrc IS NOT INITIAL.
          MOVE: 'LGORT'                   TO tg_msg_ret-field,
                c_tab_strip-tab2          TO tg_msg_ret-aba,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e04 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.

*    " 12.01.2024 RAMON ------>
*    " FIXACAO
*    IF tg_itens-fixacao IS INITIAL.
*      MOVE: 'FIXACAO'                   TO tg_msg_ret-field,
*            c_tab_strip-tab2          TO tg_msg_ret-aba,
*            'GRID3'                   TO tg_msg_ret-obj,
*            wl_linha                  TO tg_msg_ret-tabix.
*      CONCATENATE TEXT-e01 'Fixação' TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*
*      APPEND tg_msg_ret.
*      CLEAR: tg_msg_ret.
*    ENDIF.
*    " 12.01.2024 RAMON ------<

*** Lote (CHARG)
    IF wg_header-auart NE 'ZSER'.
      IF tg_itens-charg IS INITIAL.
        MOVE: 'CHARG'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.

        " 28.11.2024 - 159383 - RAMON -->
        IF wg_header-auart = 'ZUB' AND wg_header-tp_venda = '00392'.

          CONCATENATE TEXT-e01 'Lote/Safra' TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        ELSE.

          CONCATENATE TEXT-e01 TEXT-e61 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        ENDIF.
        " 28.11.2024 - 159383 - RAMON --<

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ELSEIF tg_itens-charg IS NOT INITIAL
         AND tg_itens-matnr IS NOT INITIAL.
        READ TABLE tl_mch1
          WITH KEY charg = tg_itens-charg
                   matnr = tg_itens-matnr.

        IF sy-subrc IS NOT INITIAL.
          MOVE: 'CHARG'                   TO tg_msg_ret-field,
                c_tab_strip-tab2          TO tg_msg_ret-aba,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e05 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.

*** Quantidade Prevista (ZMENG)
    IF wg_header-param_espec NE c_p
    AND wg_header-param_espec NE c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
    AND wg_header-param_espec NE c_ax.
*-CS2021000615 - 17.06.2021 - JT - fim
      IF tg_itens-status NE 'E'. " Ordem Encerradas não é obrigatorio o preenchimento do Zmeng
        IF tg_itens-zmeng IS INITIAL.
          MOVE: 'ZMENG'                   TO tg_msg_ret-field,
                c_tab_strip-tab2          TO tg_msg_ret-aba,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e01 TEXT-e62 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ENDIF.
      ENDIF.
*** Modificar quantidade de ordem de venda
      IF wg_acao EQ c_modif_qtd AND wg_redistribuir IS INITIAL.

        CLEAR: wl_zmeng_aux, wl_zmeng_aux2, tl_0061.
        READ TABLE tl_0061 WITH KEY posnr = tg_itens-posnr.

        IF sy-subrc IS NOT INITIAL.
          READ TABLE tl_0053 WITH KEY posnr = tg_itens-posnr.
          IF sy-subrc IS INITIAL.
            tl_0061-zmeng_old = tl_0053-zmeng.
          ENDIF.
        ENDIF.

        IF tg_itens-vbeln IS NOT INITIAL.
          READ TABLE tl_0053 WITH KEY posnr = tg_itens-posnr.
          IF tg_itens-zmeng LT tl_0053-zmeng.
            MOVE: 'ZMENG'                   TO tg_msg_ret-field,
                  c_tab_strip-tab2          TO tg_msg_ret-aba,
                  'GRID1'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e30 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.


        IF  tg_itens-status NE c_y.
          IF tg_itens-status NE c_w.
            IF tg_itens-status NE c_c.

              wl_zmeng_aux = ( tl_0061-zmeng_old * ( wl_0060-tolerancia / 100 ) ).
              wl_zmeng_aux2 = tl_0061-zmeng_old - tg_itens-zmeng.

              IF wl_zmeng_aux2 LT 0.
                MULTIPLY wl_zmeng_aux2 BY -1.
              ENDIF.

              IF wl_zmeng_aux2 GT wl_zmeng_aux.
                MOVE: 'ZMENG'                   TO tg_msg_ret-field,
                      c_tab_strip-tab2          TO tg_msg_ret-aba,
                      'GRID1'                   TO tg_msg_ret-obj,
                      wl_linha                  TO tg_msg_ret-tabix.
                CONCATENATE TEXT-e14 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


*** Redistribuição de Quantidade
      ELSEIF  wg_acao EQ c_modif_qtd AND wg_redistribuir IS NOT INITIAL.

        READ TABLE tg_saldo WITH KEY vbeln = tg_itens-vbeln.

        IF sy-subrc IS INITIAL.
          IF tg_itens-status NE c_y.
            IF tg_itens-status NE c_w.
              IF tg_itens-status NE c_c.
                IF tg_itens-zmeng LT tg_saldo-total.
                  MOVE: 'ZMENG'                   TO tg_msg_ret-field,
                        c_tab_strip-tab2          TO tg_msg_ret-aba,
                        'GRID1'                   TO tg_msg_ret-obj,
                        wl_linha                  TO tg_msg_ret-tabix.
                  CONCATENATE TEXT-e17 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.


      ENDIF.
    ENDIF.

    IF wg_header-param_espec IS INITIAL.
      IF ( wg_header-auart EQ 'ZMIT' ) OR ( wg_header-auart EQ 'ZFEX' ).

        SELECT *
          FROM t052
          INTO TABLE tl_t052
        WHERE zterm EQ wg_cond_pgt-zterm.

        IF ( sy-subrc EQ 0 ).

          READ TABLE tl_t052 WITH KEY zterm = wg_cond_pgt-zterm.

          CASE wg_cond_pgt-pgto_ant.
            WHEN: c_n OR c_x.

              IF NOT tl_t052-ztag1 IS INITIAL.
                MOVE: 'VALDT'                   TO tg_msg_ret-field,
                      c_tab_strip-tab2          TO tg_msg_ret-aba,
                      'GRID1'                   TO tg_msg_ret-obj,
                      wl_linha                  TO tg_msg_ret-tabix.
                CONCATENATE TEXT-e63 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ELSE.

                IF tg_itens-valdt IS INITIAL.
                  MOVE: 'VALDT'                   TO tg_msg_ret-field,
                        c_tab_strip-tab2          TO tg_msg_ret-aba,
                        'GRID1'                   TO tg_msg_ret-obj,
                        wl_linha                  TO tg_msg_ret-tabix.
                  CONCATENATE TEXT-e01 TEXT-e64 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.

              ENDIF.
            WHEN OTHERS.
              IF tl_t052-ztag1 IS INITIAL.
                IF tg_itens-valdt IS INITIAL.
                  MOVE: 'VALDT'                   TO tg_msg_ret-field,
                        c_tab_strip-tab2          TO tg_msg_ret-aba,
                        'GRID1'                   TO tg_msg_ret-obj,
                        wl_linha                  TO tg_msg_ret-tabix.
                  CONCATENATE TEXT-e01 TEXT-e64 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.
              ENDIF.
          ENDCASE.


        ENDIF.
      ENDIF.
    ENDIF.


    IF ( wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
         OR wg_header-param_espec EQ c_ax )
      " 18.12.2023 - 128467 - RBL Fixação obrigatorio -->
         "OR wg_header-param_espec EQ c_z ) " CS2020000373 23/08/2022 -LP
      " 18.12.2023 - 128467 - RBL --<
*-CS2021000615 - 17.06.2021 - JT - fim
    AND ( wg_header-auart EQ 'ZEXI'
          OR wg_header-auart EQ 'ZEXP' ).
*** Peso bruto (BRGEW)
      IF tg_itens-brgew IS INITIAL.
        MOVE: 'BRGEW'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e65 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*** Volume (VOLUM)
      IF tg_itens-volum IS INITIAL.
        MOVE: 'VOLUM'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e66 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*** Terminal (TERMINAL)
      IF tg_itens-terminal IS INITIAL.
        MOVE: 'TERMINAL'                TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e59 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*** UVl (VOLEH)

      IF tg_itens-voleh IS INITIAL.
        MOVE: 'VOLEH'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 'UV1' TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

*** Taxa câmbio (KURSF)
      IF tg_itens-kursf IS INITIAL.
        MOVE: 'KURSF'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e68 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSEIF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
*** Fixação (FIXACAO)

      IF tg_itens-fixacao IS INITIAL.
        MOVE: 'FIXACAO'                   TO tg_msg_ret-field,
              c_tab_strip-tab2          TO tg_msg_ret-aba,
              'GRID1'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e69 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        IF tg_itens-fixacao GT wg_header-num_fixacao.
          MOVE: 'FIXACAO'                   TO tg_msg_ret-field,
                c_tab_strip-tab2          TO tg_msg_ret-aba,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e69 TEXT-e02 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

*** Dt.Vencimento (VALDT)
      READ TABLE tg_cond_esp
        WITH KEY fixacao = tg_itens-fixacao.

      IF sy-subrc IS INITIAL.
        READ TABLE tl_t052
          WITH KEY zterm = tg_cond_esp-zterm.

        IF tl_t052-ztag1 IS INITIAL.
          IF tg_itens-valdt IS INITIAL.
            MOVE: 'VALDT'                   TO tg_msg_ret-field,
                  c_tab_strip-tab2          TO tg_msg_ret-aba,
                  'GRID1'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e01 TEXT-e64 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*** UM (ZIEME)
    IF tg_itens-zieme IS INITIAL.
      MOVE: 'ZIEME'                   TO tg_msg_ret-field,
            c_tab_strip-tab2          TO tg_msg_ret-aba,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e70 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


*** UM Preço (PMEIN)
    IF tg_itens-pmein IS INITIAL.
      MOVE: 'PMEIN'                   TO tg_msg_ret-field,
            c_tab_strip-tab2          TO tg_msg_ret-aba,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e70 TEXT-e71 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

*** Data Efetiva Fixa (VALDT)
    IF tg_itens-valdt IS INITIAL
    AND wg_cond_pgt-qte_venc GT 1.
      MOVE: 'VALDT'                   TO tg_msg_ret-field,
            c_tab_strip-tab2          TO tg_msg_ret-aba,
            'GRID1'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e72 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    " 08.10.2024 - RAMON -- 154003 (comentado) -->
**** " Emissor da Ordem (KUNNR)
****    IF tg_itens-kunnr IS INITIAL AND wg_redistribuir IS NOT INITIAL.
****
****      MOVE: 'KUNNR'                   TO tg_msg_ret-field,
****            c_tab_strip-tab2          TO tg_msg_ret-aba,
****            'GRID1'                   TO tg_msg_ret-obj,
****            wl_linha                  TO tg_msg_ret-tabix.
****
****      CONCATENATE TEXT-e01 TEXT-e73 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
****
****      APPEND tg_msg_ret.
****
****      CLEAR: tg_msg_ret.
****
****    ELSEIF tg_itens-kunnr IS NOT INITIAL AND wg_redistribuir IS NOT INITIAL.
****
****      READ TABLE tl_kna1 WITH KEY kunnr = tg_itens-kunnr.
****
****      IF sy-subrc IS NOT INITIAL.
****
****        MOVE: 'KUNNR'                   TO tg_msg_ret-field,
****              c_tab_strip-tab2          TO tg_msg_ret-aba,
****              'GRID1'                   TO tg_msg_ret-obj,
****              wl_linha                  TO tg_msg_ret-tabix.
****        CONCATENATE TEXT-e73 TEXT-e02 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
****
****        APPEND tg_msg_ret.
****
****        CLEAR: tg_msg_ret.
****
****      ENDIF.
****
****    ENDIF.
    " 08.10.2024 - RAMON -- 154003 (comentado) --<

**  envia msg quando o material não estiver expandido para o Empresa.
    IF tg_itens-matnr IS NOT INITIAL. " Entra se o material estiver preenchido

      IF wg_header-vkorg IS NOT INITIAL
      AND wg_header-vtweg IS NOT INITIAL.

        IF wa_mvke_53 IS INITIAL.
          MOVE: 'MVKE53'                  TO tg_msg_ret-field,
                c_tab_strip-tab2          TO tg_msg_ret-aba,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e34 tg_itens-matnr TEXT-e74 wg_header-vkorg TEXT-e75
          wg_header-vtweg TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF tg_itens-werks IS NOT INITIAL.
        IF wa_marc_53 IS INITIAL.
          MOVE: 'MARC53'                  TO tg_msg_ret-field,
                c_tab_strip-tab2          TO tg_msg_ret-aba,
                'GRID1'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e34 tg_itens-matnr TEXT-e76 tg_itens-werks TEXT-e44
          wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        IF tg_itens-lgort IS NOT INITIAL.
          IF wa_mard_53 IS INITIAL.
            DATA: mat TYPE matnr.
            mat = tg_itens-matnr.
            SHIFT mat LEFT DELETING LEADING '0'.
            MOVE: 'MARD53'                  TO tg_msg_ret-field,
                  c_tab_strip-tab2          TO tg_msg_ret-aba,
                  'GRID1'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e34 mat TEXT-e76 tg_itens-werks TEXT-e77 tg_itens-lgort
            TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret, mat.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*** Verifica se a Quantidade da 53 e 55 são iguais
*** Inicio
    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
      LOOP AT tg_logistica WHERE fixacao EQ tg_itens-fixacao.
        ADD tg_logistica-cadencia_qte TO wl_qtd_total.
      ENDLOOP.
      LOOP AT tg_itens WHERE fixacao EQ tg_itens-fixacao.
        ADD tg_itens-zmeng TO wl_qtd_total_item_f.
      ENDLOOP.

      IF  wl_qtd_total_item_f NE wl_qtd_total
      AND wl_qtd_total IS NOT INITIAL
      AND wg_header-param_espec NE c_f.

        MOVE: 'CADENCIA_QTE'            TO tg_msg_ret-field,
              c_tab_strip-tab5          TO tg_msg_ret-aba,
              'GRID3'                   TO tg_msg_ret-obj,
                1                       TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e31 TEXT-e78 tg_itens-fixacao INTO tg_msg_ret-msg  SEPARATED BY space.
        READ TABLE tg_msg_ret TRANSPORTING NO FIELDS WITH KEY msg+52(38) = tg_msg_ret-msg+52(38).
        IF  sy-subrc IS NOT INITIAL.
          APPEND tg_msg_ret.
        ENDIF.

        CLEAR: tg_msg_ret.
      ENDIF.
      CLEAR: wl_qtd_total, wl_qtd_total_item_f.
    ENDIF.
*** Fim


    IF wg_header-param_espec EQ c_a OR "Algodoeira
       wg_header-param_espec EQ c_p OR "Performance
       wg_header-param_espec EQ c_ax "Algodão Vda Fins Exp *-CS2021000615 - 17.06.2021 - JT - inicio
       OR wg_header-param_espec EQ c_z ." Algodão frame" CS2020000373 23/08/2022 -LP

      IF NOT ( line_exists( it_0235[ tabela = 'ZSDT0053' campo = 'NUMERO_RUC' ] ) ).
        IF ( tg_itens-vbeln IS INITIAL ) AND ( tg_itens-numero_ruc IS INITIAL ).
          MOVE: 'NUMERO_RUC'               TO tg_msg_ret-field,
                 c_tab_strip-tab2          TO tg_msg_ret-aba,
                 'GRID1'                   TO tg_msg_ret-obj,
                 wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-ee5 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.


      IF wg_header-param_espec EQ c_p. "Performance
        IF tg_itens-numero_ruc IS NOT INITIAL AND
           tg_itens-vbeln IS INITIAL. "IR124154 Quando ordem já criada não é necessário validar


          IF tg_itens-kunnr IS NOT INITIAL.
            DATA(_kunnr_exp) = tg_itens-kunnr.
          ELSEIF wg_header-kunnr IS NOT INITIAL.
            _kunnr_exp = wg_header-kunnr.
          ELSE.
            CLEAR: _kunnr_exp.
          ENDIF.

          SELECT SINGLE *
            FROM zsdt0289 INTO @DATA(lwa_zsdt0289)
           WHERE numero_ruc       EQ @tg_itens-numero_ruc
             AND codigo_ra        EQ @tg_itens-codigo_ra
             AND id_nomeacao_tran EQ @tg_itens-id_nomeacao_tran
             AND matnr            EQ @tg_itens-matnr
             AND kunnr_exp        EQ @_kunnr_exp.


          IF sy-subrc NE 0.
            tg_msg_ret-msg = |RUC { tg_itens-numero_ruc } não pode ser usada para Nomeação/Recinto A./Material/Cliente do item { tg_itens-posnr } ! |.
            APPEND tg_msg_ret.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.


    IF    wg_header-kunnr IS NOT INITIAL
      AND wg_header-vkorg IS NOT INITIAL
      AND wg_header-vtweg IS NOT INITIAL
      AND wg_header-spart IS NOT INITIAL.

    ENDIF.

    ADD tg_itens-vlt_porto TO vl_vlt_porto.

    ADD tg_itens-zmeng TO wl_qtd_total_item.
    ADD tg_itens-vlrtot TO wl_vlrtot.
    "--------------------------------- COMENTADO VALIDAÇÃO POR ERRO EM PRD #184230 ------------------------------------------------- INICIO
    "#180502 CS2025000566 - AJUSTES NAS VENDAS DE FRAMES DA ZSDT0062 - RGA - ini
    "não valida para o processo de redistribuição - o sistema não permite erro de inserção de fixação incorreta
*    IF WG_REDISTRIBUIR IS INITIAL.
*
*      IF <FS_TABLE> IS ASSIGNED.
*
*        UNASSIGN <FS_LINE>.
*
*        CREATE DATA T_NEW_LINE LIKE LINE OF <FS_TABLE>.
*        ASSIGN T_NEW_LINE->* TO <FS_LINE>.
*
*        CLEAR: WG_VALOR.
*
*        LOOP AT <FS_TABLE> INTO <FS_LINE>.
*          CLEAR WG_VALOR.
*          PERFORM GET_SET_VALORES USING 'REDIST'
*                                        'G'
*                               CHANGING WG_VALOR. "XXX - BG -- INICIO
*          IF WG_VALOR IS INITIAL.
*
*            PERFORM GET_SET_VALORES USING 'POSNR'
*                                          'G'
*                                 CHANGING WG_VALOR.
*
*
*
*            IF WG_VALOR IS NOT INITIAL AND WG_VALOR = TG_ITENS-FIXACAO .
*
*
*              CLEAR WG_VALOR.
*              PERFORM GET_SET_VALORES USING 'POSNR1'
*                                        'G'
*                               CHANGING WG_VALOR.
*
*              IF WG_VALOR NE '000000'.
*                IF WG_VALOR <> TG_ITENS-POSNR.
*                  TG_MSG_RET-MSG = |O item { WG_VALOR } informado na aba Preço é diferente do item { TG_ITENS-POSNR } da aba Produto/Quantidade para fixação { TG_ITENS-FIXACAO } ! |.
*                  APPEND TG_MSG_RET.
*                  CLEAR TG_MSG_RET.
*                ENDIF.
*              ENDIF.
*            ENDIF.
*          ENDIF."XXX - BG -- FIM
*
*        ENDLOOP.
*
*      ENDIF.
*
*      CLEAR WG_VALOR.
*
*    ENDIF.
    "#180502 CS2025000566 - AJUSTES NAS VENDAS DE FRAMES DA ZSDT0062 - RGA - fim
    "--------------------------------- COMENTADO VALIDAÇÃO POR ERRO EM PRD #184230 ------------------------------------------------- FIM
  ENDLOOP.




*  LOOP AT TG_ITENS.
*    ADD TG_ITENS-VLRTOT TO WL_QTD_TOTAL_ITEM_AUX.
*    IF ( WL_QTD_TOTAL_ITEM_AUX > WG_POSICAO-VLR_SALDO ).
*      TG_MSG_RET-MSG = 'Total da venda maior que o saldo do limite de crédito.'.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
*  ENDLOOP.

  IF wg_header-param_espec EQ c_p.
    IF <fs_table> IS ASSIGNED.
      CREATE DATA t_new_line LIKE LINE OF <fs_table>.
      ASSIGN t_new_line->* TO <fs_line>.

      CLEAR: wg_valor.
      LOOP AT <fs_table> INTO <fs_line>.
        PERFORM get_set_valores USING 'PRECO_ITEM'
                                      'G'
                             CHANGING wg_valor.
        IF wg_valor = '1'.
          PERFORM get_set_valores USING 'FORMULA'
                                         'G'
                               CHANGING wg_valor.

          TRANSLATE wg_valor USING '. '.
          TRANSLATE wg_valor USING ',.'.
          CONDENSE wg_valor NO-GAPS.

          IF vl_vlt_porto > wg_valor.
            MOVE: 'VLT_PORTO'               TO tg_msg_ret-field,
                  c_tab_strip-tab2          TO tg_msg_ret-aba,
                  'GRID1'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e28 TEXT-e79 INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ELSE.
          CLEAR wg_valor.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF wg_header-param_espec NE c_p
  AND wg_header-param_espec NE c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
  AND wg_header-param_espec NE c_ax
*-CS2021000615 - 17.06.2021 - JT - fim

 " 05.02.2024 - RAMON - 133197 -->
    AND wg_header-param_espec NE c_z.
    " 05.02.2024 - RAMON - 133197 --<

*    DESCRIBE TABLE TG_ITENS LINES WL_LINHA.
    wl_linha = REDUCE i( INIT x = 0 FOR ls IN tg_itens
                          WHERE ( status NE 'Y' AND status NE 'W' )
                       NEXT x = x + 1 ) .
    IF wg_cond_pgt-qte_venc GT 0.
      IF wl_linha NE wg_cond_pgt-qte_venc
        AND var_modred IS INITIAL.
*       AND WG_REDISTRIBUIR IS INITIAL.
        MOVE: c_tab_strip-tab2          TO tg_msg_ret-aba,
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
               TEXT-e10                 TO  tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

**** Redistribuição de Quantidade
    IF wg_redistribuir EQ c_x.
      IF wg_redistribuir_saldo GT 0.
        MOVE: c_tab_strip-tab2          TO tg_msg_ret-aba,
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
                 TEXT-e19                 TO  tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      IF wg_redistribuir_saldo LT 0 .
        MOVE: c_tab_strip-tab2          TO tg_msg_ret-aba,
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
                 TEXT-e18                 TO  tg_msg_ret-msg.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
  ELSEIF wg_header-param_espec EQ c_p.
    PERFORM retorna_preco_item USING '3'
                                     space
                               CHANGING wl_dmbtr.
*    LOOP AT TG_PRECO_N
*      WHERE PRECO EQ 3.
*      LOOP AT <FS_TABLE> INTO <FS_LINE>.
*        CLEAR: WG_VALOR.
*        PERFORM GET_SET_VALORES USING 'COD_FP'
*                                      'G'
*                             CHANGING WG_VALOR.
*
*        IF WG_VALOR EQ TG_PRECO_N-COD_FP.
*          CLEAR: WG_VALOR.
*          PERFORM GET_SET_VALORES USING WG_PRECO_N-FIELD
*                                        'G'
*                                 CHANGING WG_VALOR.
*
*          TRANSLATE WG_VALOR USING '. '.
*          TRANSLATE WG_VALOR USING ',.'.
*          CONDENSE WG_VALOR NO-GAPS.
*
*          WL_DMBTR = WG_VALOR.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.

    IF wl_vlrtot GT wl_dmbtr.
      MOVE: c_tab_strip-tab2          TO tg_msg_ret-aba.
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-e20 tg_preco-bezei TEXT-e80 INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.
  ENDIF.

*  Aba - Pagamento Antecipado >
  IF wg_cond_pgt-pgto_ant IS NOT INITIAL.
    DELETE tg_pgt_ant WHERE valdt IS INITIAL
                        AND dmbtr IS INITIAL.

*** Tabela TG_PGT_ANT ()
    IF tg_pgt_ant[] IS INITIAL AND  wg_header-param_espec NE c_z. " Condição Especial Z não obrigar pagamento antecipado
      MOVE: c_tab_strip-tab4          TO tg_msg_ret-aba.
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-e06 TEXT-e81 INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.

    CLEAR: wl_vlrtot.

    LOOP AT tg_pgt_ant.

      wl_linha = sy-tabix.
*** Dta.Vencimento (VALDT)

      IF tg_pgt_ant-valdt IS INITIAL.
        MOVE: 'VALDT'                   TO tg_msg_ret-field,
              c_tab_strip-tab4          TO tg_msg_ret-aba,
              'GRID2'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e64 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
*        CLEAR WL_DUPLICADO.
*        LOOP AT TG_PGT_ANT TRANSPORTING NO FIELDS
*          WHERE VALDT EQ TG_PGT_ANT-VALDT.
*          IF SY-TABIX NE WL_LINHA.
*            WL_DUPLICADO = C_X.
*          ENDIF.
*        ENDLOOP.
*        IF WL_DUPLICADO IS NOT INITIAL.
*          MOVE: 'VALDT'                   TO TG_MSG_RET-FIELD,
*                C_TAB_STRIP-TAB4          TO TG_MSG_RET-ABA,
*               'GRID2'                    TO TG_MSG_RET-OBJ,
*                WL_LINHA                  TO TG_MSG_RET-TABIX.
*          CONCATENATE text-E12 text-E44 WL_LINHA INTO  TG_MSG_RET-MSG SEPARATED BY SPACE.
*
*          APPEND TG_MSG_RET.
*          CLEAR: TG_MSG_RET.
*        ENDIF.
      ENDIF.
      IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
         wg_header-param_espec EQ c_ax." OR
        "wg_header-param_espec EQ c_z . " CS2020000373 23/08/2022 -LP - retirado dia 07.02.2024 - RAMON
*-CS2021000615 - 17.06.2021 - JT - fim
        IF tg_pgt_ant-posnr  IS INITIAL.
          IF tg_pgt_ant-adiant IS INITIAL.

            APPEND VALUE #(
                            field = 'POSNR'
                            aba   = c_tab_strip-tab4
                            obj   = 'GRID2'
                            tabix = wl_linha
                            msg   = |{ TEXT-e01 } 'Item na Aba Pg Antecipado' { TEXT-e44 } { wl_linha ALPHA = OUT  }|
                          ) TO tg_msg_ret.
          ENDIF.
        ELSE.

          CLEAR wl_duplicado.

          wl_duplicado =
                 REDUCE #( INIT x = 0
                          FOR wa IN tg_pgt_ant
                          WHERE ( posnr EQ tg_pgt_ant-posnr )
                          NEXT x = x + 1 ).

          CASE wl_duplicado.
            WHEN 0 OR 1.
            WHEN OTHERS.

              APPEND VALUE #(
                  field = 'POSNR'
                  aba   = c_tab_strip-tab4
                  obj   = 'GRID2'
                  tabix = wl_linha
                  msg   = |{ TEXT-e12 } "{ TEXT-w30 }" iguais { TEXT-e44 } { wl_linha ALPHA = OUT }|
                ) TO tg_msg_ret.

          ENDCASE.
        ENDIF.
      ELSE.

        CLEAR wl_duplicado.

        wl_duplicado =
               REDUCE #( INIT x = 0
                        FOR wa IN tg_pgt_ant
                        WHERE ( posnr EQ tg_pgt_ant-posnr AND
                                valdt EQ tg_pgt_ant-valdt )
                        NEXT x = x + 1 ).

        CASE wl_duplicado.
          WHEN 0 OR 1.
          WHEN OTHERS.

            APPEND VALUE #(
                field = 'ADIANT'
                aba   = c_tab_strip-tab4
                obj   = 'GRID2'
                tabix = wl_linha
                msg   = | { TEXT-e12 } "{ TEXT-w30 } e Dta { TEXT-e45 }" iguais  { TEXT-e44 } { wl_linha ALPHA = OUT }|
              ) TO tg_msg_ret.

        ENDCASE.
      ENDIF.

*** Valor (DMBTR)
      IF tg_pgt_ant-dmbtr IS INITIAL.
        MOVE: 'DMBTR'                   TO tg_msg_ret-field,
              c_tab_strip-tab4          TO tg_msg_ret-aba,
              'GRID2'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e82 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF ( wg_cond_pgt-pgto_ant EQ c_x
      OR   wg_cond_pgt-pgto_ant EQ c_n )
      AND wg_header-param_espec EQ c_p.

*** Taxa de câmbio (KURSF)
        IF tg_pgt_ant-kursf IS INITIAL.
          MOVE: 'KURSF'                   TO tg_msg_ret-field,
                c_tab_strip-tab4          TO tg_msg_ret-aba,
                'GRID2'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e01 TEXT-e68 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
      MOVE: tg_pgt_ant-posnr TO tl_vlr_adiant-posnr,
            tg_pgt_ant-dmbtr TO tl_vlr_adiant-dmbtr.

      COLLECT tl_vlr_adiant.
      ADD tg_pgt_ant-dmbtr TO wl_vlrtot.
    ENDLOOP.

    IF wg_header-param_espec NE c_p
    AND wg_header-param_espec NE c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
    AND wg_header-param_espec NE c_ax
      " 07.02.2024 - RAMON -->
      AND wg_header-param_espec NE c_z.
      " 07.02.2024 - RAMON <--
*-CS2021000615 - 17.06.2021 - JT - fim
      LOOP AT tl_vlr_adiant.
        wl_linha = sy-tabix.

        "CHECK tg_itens[] IS NOT INITIAL. " 07.02.2024 - RAMON

        READ TABLE tg_itens
          WITH KEY posnr = tl_vlr_adiant-posnr.

        IF sy-subrc IS NOT INITIAL.

          MOVE: 'POSNR'                   TO tg_msg_ret-field,
                c_tab_strip-tab4          TO tg_msg_ret-aba,
                'GRID2'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e16 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ELSEIF wg_redistribuir NE c_x.

          IF tl_vlr_adiant-dmbtr GT tg_itens-vlrtot.

            MOVE: 'DMBTR'               TO tg_msg_ret-field,
              c_tab_strip-tab4          TO tg_msg_ret-aba,
              'GRID2'                   TO tg_msg_ret-obj.
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
            CONCATENATE TEXT-e15 TEXT-e83 tl_vlr_adiant-posnr INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.

            CLEAR: tg_msg_ret.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ELSEIF ( wg_header-param_espec NE c_a AND
*-CS2021000615 - 17.06.2021 - JT - inicio
             wg_header-param_espec NE c_ax ).
*-CS2021000615 - 17.06.2021 - JT - fim
      PERFORM retorna_preco_item USING '3'
                                       space
                                 CHANGING wl_dmbtr.
      IF NOT ( wl_dmbtr IS INITIAL ).
        IF wl_vlrtot GT wl_dmbtr.
          MOVE: c_tab_strip-tab6          TO tg_msg_ret-aba.
*            'GRID5'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
          CONCATENATE TEXT-e23 tg_preco-bezei TEXT-e80 INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*  Aba - Adiantamento Externo >
*** Tabela TG_ADTO_EXT ()
  IF wg_header-param_espec EQ c_p
  AND ( wg_cond_pgt-pgto_ant EQ c_x
   OR   wg_cond_pgt-pgto_ant EQ c_n ).
    CLEAR: wl_dmbtr, wl_vlrtot.

    IF tg_adto_ext[] IS INITIAL.
      MOVE: c_tab_strip-tab6          TO tg_msg_ret-aba.
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-e06 TEXT-e84 INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.
  ENDIF.
  LOOP AT tg_adto_ext.
    wl_linha = sy-tabix.
*** Empresa (BUKRS)

    IF tg_adto_ext-bukrs IS INITIAL.
      MOVE: 'BUKRS'                   TO tg_msg_ret-field,
            c_tab_strip-tab6          TO tg_msg_ret-aba,
            'GRID5'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e85 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

*** Dta.Vencimento (VALDT)
    IF tg_adto_ext-valdt IS INITIAL.
      MOVE: 'VALDT'                   TO tg_msg_ret-field,
            c_tab_strip-tab6          TO tg_msg_ret-aba,
            'GRID5'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e64 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      CLEAR wl_duplicado.
      LOOP AT tg_adto_ext TRANSPORTING NO FIELDS
        WHERE valdt EQ tg_adto_ext-valdt
          AND posnr EQ tg_adto_ext-posnr.
        IF sy-tabix NE wl_linha.
          wl_duplicado = c_x.
        ENDIF.
      ENDLOOP.
      IF wl_duplicado IS NOT INITIAL.
        MOVE: 'VALDT'                   TO tg_msg_ret-field,
              c_tab_strip-tab6          TO tg_msg_ret-aba,
             'GRID5'                    TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e24 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

*** Valor (DMBTR)
    IF tg_adto_ext-dmbtr IS INITIAL.
      MOVE: 'DMBTR'                   TO tg_msg_ret-field,
            c_tab_strip-tab6          TO tg_msg_ret-aba,
            'GRID5'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e82 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

*** Moeda (WAERS)
    IF tg_adto_ext-waers IS INITIAL.
      MOVE: 'WAERS'                   TO tg_msg_ret-field,
            c_tab_strip-tab6          TO tg_msg_ret-aba,
            'GRID5'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e86 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

*** Fornecedor (LIFNR)
    IF tg_adto_ext-lifnr IS INITIAL.
      MOVE: 'LIFNR'                   TO tg_msg_ret-field,
            c_tab_strip-tab6          TO tg_msg_ret-aba,
            'GRID5'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e87 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wg_cond_pgt-pgto_ant EQ space
    AND wg_header-param_espec NE c_p.
*** País (BANKS)
      IF tg_adto_ext-banks IS INITIAL.
        MOVE: 'BANKS'                   TO tg_msg_ret-field,
              c_tab_strip-tab6          TO tg_msg_ret-aba,
              'GRID5'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e88 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Chave de Banco (BANKL)
      IF tg_adto_ext-bankl IS INITIAL.
        MOVE: 'BANKL'                   TO tg_msg_ret-field,
              c_tab_strip-tab6          TO tg_msg_ret-aba,
              'GRID5'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e89 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Código SWIFT (SWIFT)
      IF tg_adto_ext-swift IS INITIAL.
        MOVE: 'SWIFT'                   TO tg_msg_ret-field,
              c_tab_strip-tab6          TO tg_msg_ret-aba,
              'GRID5'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e90 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Conta Bancária (BANKN)
      IF tg_adto_ext-bankn IS INITIAL.
        MOVE: 'BANKN'                   TO tg_msg_ret-field,
              c_tab_strip-tab6          TO tg_msg_ret-aba,
              'GRID5'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e91 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.


*      ENDIF.

    ENDIF.
    ADD tg_adto_ext-dmbtr TO wl_vlrtot.
  ENDLOOP.
  IF wg_header-param_espec EQ c_p
  AND ( wg_cond_pgt-pgto_ant EQ c_x
   OR   wg_cond_pgt-pgto_ant EQ c_n ).

    PERFORM retorna_preco_item USING '1'
                                  space
                                   CHANGING wl_dmbtr.

    IF wl_vlrtot GT wl_dmbtr.
      MOVE: c_tab_strip-tab6          TO tg_msg_ret-aba.
*            'GRID5'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
      CONCATENATE TEXT-e21 tg_preco-bezei TEXT-e80 INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
*  Aba - Logistica >
  DELETE tg_logistica WHERE data_progr   IS INITIAL
*                        AND ZMENG        IS INITIAL
                        AND zieme        IS INITIAL
                        AND cadencia_qte IS INITIAL.

*** Tabela TG_LOGISTICA ()
  IF tg_logistica[] IS INITIAL
  AND wg_header-inco1 EQ 'CIF'
    AND wg_header-param_espec NE 'A'  "192341-CS2025001146 Ajuste p gerar exportação de algodão CIF/CRF - SMC
    AND wg_header-param_espec NE 'Z'. "192341-CS2025001146 Ajuste p gerar exportação de algodão CIF/CRF - SMC
    MOVE: c_tab_strip-tab5          TO tg_msg_ret-aba.
*            'GRID2'                   TO TG_MSG_RET-OBJ,
*            WL_LINHA                  TO TG_MSG_RET-TABIX.
    CONCATENATE TEXT-e06 TEXT-e92 INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

  LOOP AT tg_logistica.
    wl_linha = sy-tabix.
*** Data Programada (DATA_PROGR)
    IF tg_logistica-data_progr IS INITIAL.
      MOVE: 'DATA_PROGR'                   TO tg_msg_ret-field,
            c_tab_strip-tab5          TO tg_msg_ret-aba,
            'GRID3'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e93 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.

    ENDIF.

    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
      IF tg_logistica-fixacao IS INITIAL.
*      Obriga o preenchimento da fixação na Logistica quando Parametro especial igual a M
        MOVE: 'FIXACAO'                 TO tg_msg_ret-field,
              c_tab_strip-tab5          TO tg_msg_ret-aba,
              'GRID3'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e94 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

*** UM (ZIEME)
    IF tg_logistica-zieme IS INITIAL.
      MOVE: 'ZIEME'                   TO tg_msg_ret-field,
            c_tab_strip-tab5          TO tg_msg_ret-aba,
            'GRID3'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e70 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

*-IR065241 - 06.07.2021 - JT - inicio
    IF tg_logistica-zieme <> 'KG' AND
       tg_logistica-zieme <> 'TO' AND
      " 02.09.2024 - RAMON --> bug 150168
      tg_logistica-zieme <> 'L' AND
      tg_logistica-zieme <> 'M3'.
      " 02.09.2024 - RAMON --> bug 150168

      MOVE: 'ZIEME'                   TO tg_msg_ret-field,
            c_tab_strip-tab5          TO tg_msg_ret-aba,
            'GRID3'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-ee6 TEXT-e70 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.
*-IR065241 - 06.07.2021 - JT - fim

*** Quantidade Diária (CADENCIA_QTE)
    IF tg_logistica-cadencia_qte IS INITIAL
    AND  tg_logistica-zmeng IS INITIAL
    AND tg_logistica-fixacao IS INITIAL.
      MOVE: 'CADENCIA_QTE'            TO tg_msg_ret-field,
            c_tab_strip-tab5          TO tg_msg_ret-aba,
            'GRID3'                   TO tg_msg_ret-obj,
            wl_linha                  TO tg_msg_ret-tabix.
      CONCATENATE TEXT-e01 TEXT-e95 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.

    CASE wg_header-risco_sacado.
      WHEN: 'S'.
        IF ( tg_logistica-valdt_hedge IS INITIAL ).
          MOVE: 'VALDT_HEDGE'             TO tg_msg_ret-field,
                c_tab_strip-tab5          TO tg_msg_ret-aba,
                'GRID3'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-e01 TEXT-e96 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
    ENDCASE.
    ADD tg_logistica-cadencia_qte TO wl_qtd_total.


  ENDLOOP.

*** Tabela TG_LOGISTICA ()

  IF wg_header-param_espec NE c_m AND  wg_header-param_espec NE c_z .
    IF wl_qtd_total NE wl_qtd_total_item
    AND wl_qtd_total IS NOT INITIAL
    AND wg_header-param_espec <> c_f.
      MOVE: 'CADENCIA_QTE'            TO tg_msg_ret-field,
            c_tab_strip-tab5          TO tg_msg_ret-aba,
            'GRID3'                   TO tg_msg_ret-obj,
              1                       TO tg_msg_ret-tabix.
      MOVE: TEXT-e08  TO  tg_msg_ret-msg.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

*  Aba - Preço >
  IF <fs_table> IS ASSIGNED.

    LOOP AT <fs_table> INTO <fs_line>.

      " 17.01.2024 - RAMON - Trocando o local onde define o wl_linha -->
      wl_linha = sy-tabix.
      " 17.01.2024 - RAMON - Trocando o local onde define o wl_linha --<

      CLEAR: wg_valor.

      PERFORM get_set_valores USING 'TIPO_CALC'
                                    'G'
                           CHANGING wg_valor.
      IF wg_valor NE c_c.
        LOOP AT tg_fields.
          CLEAR: wg_valor.
          PERFORM get_set_valores USING tg_fields-field
                                        'G'
                               CHANGING wg_valor.
          IF wg_valor NE space.
            wl_flag = c_x.
*     WHERE TIPO_CALC NE C_C
*       AND FORMULA2  NE SPACE.

*          IF WG_HEADER-PARAM_ESPEC EQ C_M.
*            AT NEW POSNR.
*              TL_PRECO[] = TG_PRECO[].
*              DELETE TL_PRECO WHERE POSNR NE TG_PRECO-POSNR.
*
*            ENDAT.
            CLEAR: wg_valor.
            PERFORM get_set_valores USING 'NIVEL'
                                          'G'
                                 CHANGING wg_valor.

            CLEAR: wg_valor_aux.
            PERFORM get_set_valores USING 'COD_FP'
                                          'G'
                                 CHANGING wg_valor_aux.

            " 17.01.2024 - RAMON - Trocando o local onde define o wl_linha -->
**            READ TABLE tg_preco_n ASSIGNING FIELD-SYMBOL(<fs_preco_n>)
**                WITH KEY nivel  = wg_valor
**                         cod_fp = wg_valor_aux.
**
**            wl_linha = sy-tabix.
***          ELSE.
***            WL_LINHA = SY-TABIX.
***          ENDIF.
            " 17.01.2024 - RAMON - Trocando o local onde define o wl_linha --<

            CLEAR: wg_valor_aux.
            PERFORM get_set_valores USING 'WAERS'
                                          'G'
                                 CHANGING wg_valor_aux.
            IF wg_valor_aux IS INITIAL.
              MOVE: 'WAERS'                   TO tg_msg_ret-field,
                    c_tab_strip-tab3          TO tg_msg_ret-aba,
                    'GRID4'                   TO tg_msg_ret-obj,
                    wl_linha                  TO tg_msg_ret-tabix.
              CONCATENATE TEXT-e01 TEXT-e86 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.

            ENDIF.

*** REF.CBOT (CBOT)
            CLEAR: wg_valor.
            PERFORM get_set_valores USING 'CBOT'
                                          'G'
                                 CHANGING wg_valor.
            CLEAR: wg_valor_aux.
            PERFORM get_set_valores USING 'OCBOT'
                                          'G'
                                 CHANGING wg_valor_aux.
            IF wg_valor IS INITIAL
            AND wg_valor_aux IS NOT INITIAL.
              MOVE: 'CBOT'                   TO tg_msg_ret-field,
                    c_tab_strip-tab3          TO tg_msg_ret-aba,
                    'GRID4'                   TO tg_msg_ret-obj,
                    wl_linha                  TO tg_msg_ret-tabix.
              CONCATENATE TEXT-e01 TEXT-e97 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

              IF wg_header-param_espec EQ c_z. " #93180 permitir salvar fixação caso quantidade fixada em branco 83960
                PERFORM get_set_valores USING 'QTDFIXADA'
                                                     'G'
                                        CHANGING wg_valor.
                IF wg_valor EQ '0,00'.
                  CLEAR  tg_msg_ret.
                  CLEAR:wg_valor.
                ELSE.
                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.

              ELSE.

                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ENDIF.

            ENDIF.
          ENDIF.

          " 20.12.2023 - 128467 - RBL -->
**          IF wg_header-param_espec EQ c_z.
**
**            DATA lv_preco TYPE bezei60.
**            DATA lv_bezei TYPE bezei30.
**
**            PERFORM get_set_valores USING 'PRECO' 'G' CHANGING lv_preco.
**            PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING lv_bezei.
**
**            IF ( lv_preco <> '' AND lv_preco <> '0,00') AND
**              ( lv_bezei IN r_bezei OR
**                lv_bezei IN p_bezei OR
**                lv_bezei IN c_bezei OR
**                lv_bezei IN n_bezei ).
**
**              PERFORM f_valida_campo_alv_initial USING 'CBOT' wl_linha 'GRID1'.
**              PERFORM f_valida_campo_alv_initial USING 'WAERS' wl_linha 'GRID1'.
**              PERFORM f_valida_campo_alv_initial USING 'QTDFIXADA' wl_linha 'GRID1'.
**              PERFORM f_valida_campo_alv_initial USING 'MONAT' wl_linha 'GRID1'.
**              PERFORM f_valida_campo_alv_initial USING 'SAFRA' wl_linha 'GRID1'.
**              PERFORM f_valida_campo_alv_initial USING 'VALDT' wl_linha 'GRID1'.
**              PERFORM f_valida_campo_alv_initial USING 'WAERS' wl_linha 'GRID1'.
**
**            ENDIF.
**
**          ENDIF.
          " 20.12.2023 - 128467 - RBL --<

        ENDLOOP.

      ENDIF.

      " 17.01.2024 - RAMON - Correção validação -->

      IF wg_header-param_espec EQ c_z.

        DATA lv_preco TYPE bezei60.
        DATA lv_bezei TYPE bezei30.

        PERFORM get_set_valores USING 'PRECO' 'G' CHANGING lv_preco.
        PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING lv_bezei.

        IF ( lv_preco <> '' AND lv_preco <> '0,00') AND
          ( lv_bezei IN r_bezei OR
            lv_bezei IN p_bezei OR
            lv_bezei IN c_bezei OR
            lv_bezei IN n_bezei OR
            lv_bezei IN b_bezei ). " 29.10.2024 - 147331 - RAMON -

          PERFORM f_valida_campo_alv_initial USING 'CBOT' wl_linha 'GRID1'.
          PERFORM f_valida_campo_alv_initial USING 'WAERS' wl_linha 'GRID1'.
          PERFORM f_valida_campo_alv_initial USING 'QTDFIXADA' wl_linha 'GRID1'.
          PERFORM f_valida_campo_alv_initial USING 'MONAT' wl_linha 'GRID1'.
          PERFORM f_valida_campo_alv_initial USING 'SAFRA' wl_linha 'GRID1'.
          PERFORM f_valida_campo_alv_initial USING 'VALDT' wl_linha 'GRID1'.
          PERFORM f_valida_campo_alv_initial USING 'WAERS' wl_linha 'GRID1'.

        ENDIF.

      ENDIF.
      " 17.01.2024 - RAMON - Correção validação --<

      "Validação Quantidade a Fixar aba preço, não ultrapassar quantidade do contrato + tolerância   #93180 v_quantidade 83960
      IF wg_header-param_espec EQ c_z.


        CLEAR: wl_sqtd_valor,wg_valor.
        PERFORM get_set_valores_z USING 'BEZEI'
                                             'G'
                                   CHANGING wg_valor.
        TRANSLATE wg_valor USING '. '.
        CONDENSE wg_valor NO-GAPS.
        TRANSLATE wg_valor USING ',.'.
*
*            wl_sqtd_valor = wg_valor.
*            TRANSLATE wl_sqtd_valor USING ', '.
*
*            TRY.
*            CALL METHOD cl_fdt_quantity_conv=>convert_string_to_quantity
*              EXPORTING
*                iv_text     = wl_sqtd_valor
*              receiving
*                rs_quantity = wl_mqtd_valor
*                .
*            CATCH cx_fdt_conversion .
*            ENDTRY.

        wl_mqtd_valor = wg_valor.
        wl_qtd_fixar  = wl_qtd_fixar + wl_mqtd_valor.
        CLEAR wg_valor.

      ENDIF.

      IF ( wg_header-param_espec IS INITIAL ).

        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'BEZEI'
                                      'G'
                             CHANGING wg_valor.

        CASE wg_valor.
          WHEN: 'PREMIO' OR 'SPREAD'.

            CLEAR: wg_valor.
            PERFORM get_set_valores USING 'MONAT'
                                          'G'
                                 CHANGING wg_valor.
            IF ( wg_valor EQ '00' ).

              MOVE: 'MONAT'                   TO tg_msg_ret-field,
                    c_tab_strip-tab3          TO tg_msg_ret-aba,
                    'GRID4'                   TO tg_msg_ret-obj,
                    wl_linha                  TO tg_msg_ret-tabix.
              CONCATENATE TEXT-e98 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
        ENDCASE.
      ENDIF.

      IF wg_header-param_espec EQ c_m ."OR wg_header-param_espec EQ c_z.
        CLEAR: wg_valor.

        PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
        CASE strlen( wg_valor ).
          WHEN 2 OR 3.
            PERFORM get_set_valores USING 'QTDFIXADA' 'G' CHANGING wg_valor.
            IF wg_valor NE '0,00'.
              PERFORM get_set_valores USING 'POSNR1' 'G' CHANGING wg_valor.
              IF wg_valor EQ '000000'.

                MOVE: 'POSNR1'                  TO tg_msg_ret-field,
                      c_tab_strip-tab3          TO tg_msg_ret-aba,
                      'GRID4'                   TO tg_msg_ret-obj,
                      wl_linha                  TO tg_msg_ret-tabix.

                PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.
                CONCATENATE TEXT-e01 TEXT-d21 wg_valor INTO  tg_msg_ret-msg SEPARATED BY space.

                PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
                CONCATENATE tg_msg_ret-msg wg_valor INTO  tg_msg_ret-msg SEPARATED BY space.

                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.

              ENDIF.
*               BLOQUEIO PARA O PREMIO E CHICAGO
*               OBRIGA O PREENCHIMENTO DO CAMPO CBOT E MONAT
              PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
              IF wg_valor(1) NE 'T'.

                CLEAR wg_valor.
                PERFORM get_set_valores USING 'CBOT' 'G' CHANGING wg_valor.
                IF wg_valor IS INITIAL.

                  MOVE: 'CBOT'              TO tg_msg_ret-field,
                  c_tab_strip-tab3          TO tg_msg_ret-aba,
                  'GRID4'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.

                  PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.
                  tg_msg_ret-msg = |{ TEXT-e01 } "REF. CBOT" na Fixação{ wg_valor }|.

                  PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
                  tg_msg_ret-msg = | { tg_msg_ret-msg } { wg_valor }|.

                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.

                IF var_modred IS INITIAL.
                  PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.
                  IF wg_valor EQ '00'.
                    MOVE: 'CBOT'              TO tg_msg_ret-field,
                                          c_tab_strip-tab3          TO tg_msg_ret-aba,
                                          'GRID4'                   TO tg_msg_ret-obj,
                                          wl_linha                  TO tg_msg_ret-tabix.

                    PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.
                    tg_msg_ret-msg = |{ TEXT-e01 } "Mês Fixação" na Fixação { wg_valor }|.

                    PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
                    tg_msg_ret-msg = | { tg_msg_ret-msg } { wg_valor }|.

                    APPEND tg_msg_ret.
                    CLEAR: tg_msg_ret.
                  ENDIF.
                ENDIF.

              ENDIF.


            ENDIF.
        ENDCASE.
      ENDIF.
    ENDLOOP.



*    *   Block a atualização da tabela 59 quando não existir Ts Livres
    IF erro_59 CS 'E_C'.
      CONCATENATE TEXT-m31 TEXT-m32 INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF erro_59 CS 'E_P'.
      CONCATENATE TEXT-m31 TEXT-m33 INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF erro_59 CS 'E_T'.
      CONCATENATE TEXT-m31 TEXT-m34 INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

  IF wl_flag IS INITIAL.
    MOVE: "'CBOT'                   TO TG_MSG_RET-FIELD,
          c_tab_strip-tab3          TO tg_msg_ret-aba.
*          'GRID4'                   TO TG_MSG_RET-OBJ
*          WL_LINHA                  TO TG_MSG_RET-TABIX.
    CONCATENATE TEXT-e06 TEXT-e71 INTO  tg_msg_ret-msg SEPARATED BY space.

    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

*  Aba - Instrução >
  IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
     wg_header-param_espec EQ c_ax OR
    wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
    IF NOT erro_ins IS INITIAL.
      MOVE: 'QUANTIDADE'                   TO tg_msg_ret-field,
            c_tab_strip-tab7          TO tg_msg_ret-aba,
            'GRID6'                   TO tg_msg_ret-obj.

      tg_msg_ret-msg = |Quantidade de Instrução Superior a Quantidade do Contrato!|.

      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    CALL METHOD grid6->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.

    obj_frete->verifica_erros( EXPORTING i_instrucao = tg_instrucao[]
                                         i_msg       = tg_msg_ret[]
                                         i_fcat      = t_fieldcatalog
                                         i_aba       = c_tab_strip-tab7
                               IMPORTING e_msg       = tg_msg_ret[] ).

  ENDIF.

*  Inicio Lote.
*  Aba - Formação de Lote >
  IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
     wg_header-param_espec EQ c_ax OR
      wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
    LOOP AT tg_form_lote.
      wl_linha = sy-tabix.
*** Material (BUKRS)
      IF tg_form_lote-matnr IS INITIAL.
        MOVE: 'MATNR'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e34 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

***CLASSIFICACAO
      IF tg_form_lote-classificacao IS INITIAL.
        MOVE: 'CLASSIFICACAO'           TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d01 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSEIF tg_form_lote-classificacao IS NOT INITIAL.
        IF NOT 'C_R' CS tg_form_lote-classificacao.
          MOVE: 'CLASSIFICACAO'           TO tg_msg_ret-field,
                c_tab_strip-tab7          TO tg_msg_ret-aba,
                'GRID7'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.
          CONCATENATE TEXT-d01 TEXT-e02 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

*** Centro (WERKS)
      IF tg_form_lote-werks IS INITIAL.
        MOVE: 'WERKS'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d02 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Depósito (LGORT)
      IF tg_form_lote-lgort IS INITIAL.
        MOVE: 'LGORT'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d03 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Lote (CHARG)
      IF tg_form_lote-charg IS INITIAL.
        MOVE: 'CHARG'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d04 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Quantidade Prevista (ZMENG)
      IF tg_form_lote-zmeng IS INITIAL.
        MOVE: 'ZMENG'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d05 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Quantidade Prevista (ZMENG)
      IF tg_form_lote-zmeng IS INITIAL.
        MOVE: 'ZMENG'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d05 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** UM (ZIEME)
      IF tg_form_lote-zieme IS INITIAL.
        MOVE: 'ZIEME'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e70 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Volume (VOLUM)
      "Comentado - USER STORY 69867 - Anderson Oenning - 13/05/2022

*      IF tg_form_lote-volum IS INITIAL.
*        MOVE: 'VOLUM'                   TO tg_msg_ret-field,
*              c_tab_strip-tab7          TO tg_msg_ret-aba,
*              'GRID7'                   TO tg_msg_ret-obj,
*              wl_linha                  TO tg_msg_ret-tabix.
*        CONCATENATE text-e01 text-e66 text-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*
*        APPEND tg_msg_ret.
*        CLEAR: tg_msg_ret.
*
*      ENDIF.
*Comentado - USER STORY 69867 - Anderson Oenning - 13/05/2022

*** Unidade de Volume (VOLEH)
      IF tg_form_lote-voleh IS INITIAL.
        MOVE: 'VOLEH'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e67 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Valor (DMBTR)
      IF tg_form_lote-dmbtr IS INITIAL.
        MOVE: 'DMBTR'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e82 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
*** Moeda  (WAERK)
      IF tg_form_lote-waerk IS INITIAL.
        MOVE: 'WAERK'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e86 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Vlr.Libra TO (LIBRA_TO)
      IF tg_form_lote-libra_to IS INITIAL.
        MOVE: 'LIBRA_TO'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d06 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
*** UM preço (PMEIN)
      IF tg_form_lote-pmein IS INITIAL.
        MOVE: 'PMEIN'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e70 TEXT-e71 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Terminal/Porto (TERMINAL)
      IF tg_form_lote-terminal IS INITIAL.
        MOVE: 'TERMINAL'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e59 '/' TEXT-d07 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Incoterms (INCO1)
      IF tg_form_lote-inco1 IS INITIAL.
        MOVE: 'INCO1'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-e41 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
*WSBWSBWSB
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Local de Entrega (LENTREGA)
      IF tg_form_lote-lentrega IS INITIAL.
        MOVE: 'LENTREGA'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d19 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*** Cliente (KUNNR)
      IF tg_form_lote-kunnr IS INITIAL.
        MOVE: 'KUNNR'                   TO tg_msg_ret-field,
              c_tab_strip-tab7          TO tg_msg_ret-aba,
              'GRID7'                   TO tg_msg_ret-obj,
              wl_linha                  TO tg_msg_ret-tabix.
        CONCATENATE TEXT-e01 TEXT-d09 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.


    ENDLOOP.
  ENDIF.
*       VERIFICA SE O LOTE ESTA COM AS INFORMAÇÕES CORRETAS """"LOTE WSB""""
  IF tg_form_lote[] IS NOT INITIAL.
    DATA: vlgort TYPE t001l-lgort.

    LOOP AT tg_form_lote.
      wl_linha = sy-tabix.

      CLEAR: wa_mard_66, wa_mvke_66, wa_knvv_66.

      IF tg_form_lote-matnr IS NOT INITIAL.
        IF tg_form_lote-werks IS NOT INITIAL.
          IF tg_form_lote-lgort IS NOT INITIAL.

            SELECT SINGLE *
               FROM mard
               INTO wa_mard_66
               WHERE matnr EQ tg_form_lote-matnr
                 AND werks EQ tg_form_lote-werks
                 AND lgort EQ tg_form_lote-lgort.

          ENDIF.

          SELECT SINGLE *
             FROM marc
             INTO wa_marc_66
             WHERE matnr EQ tg_form_lote-matnr
               AND werks EQ tg_form_lote-werks.

        ENDIF.

        IF wg_header-vkorg IS NOT INITIAL.

          SELECT SINGLE *
              FROM mvke
              INTO wa_mvke_66
              WHERE matnr EQ tg_form_lote-matnr
                AND vkorg EQ wg_header-vkorg
                AND vtweg EQ '10'.

        ENDIF.
      ENDIF.

** INICIO VERIFICA EMISSOR
      SELECT SINGLE *
        FROM knvv
        INTO wa_knvv_66
        WHERE kunnr EQ tg_form_lote-kunnr
          AND vkorg EQ wg_header-vkorg
          AND vtweg EQ '10'
          AND spart EQ wg_header-spart.
** FIM VERIFICA EMISSOR



** Verifica se o emissor esta expandido
      IF    tg_form_lote-kunnr IS NOT INITIAL
        AND wg_header-vkorg IS NOT INITIAL
        AND wg_header-spart IS NOT INITIAL.

        IF wa_knvv_66 IS INITIAL.

          DATA: kunnr TYPE kunag.
          kunnr = tg_form_lote-kunnr.
          MOVE: 'KNVV66'                  TO tg_msg_ret-field.
          SHIFT kunnr LEFT DELETING LEADING '0'.
          CONCATENATE TEXT-d10 kunnr TEXT-d11 wg_header-vkorg
          TEXT-d12 wg_header-spart TEXT-e44
          wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret, kunnr.

        ENDIF.
      ENDIF.

      IF tg_form_lote-matnr IS NOT INITIAL.
        mat = tg_form_lote-matnr.
        SHIFT mat LEFT DELETING LEADING '0'.
        IF tg_form_lote-werks IS NOT INITIAL.
          IF tg_form_lote-lgort IS NOT INITIAL.

            IF wa_mard_66 IS INITIAL.
              MOVE: 'MARD66'                  TO tg_msg_ret-field,
                    c_tab_strip-tab7          TO tg_msg_ret-aba,
                    'GRID7'                   TO tg_msg_ret-obj,
                    wl_linha                  TO tg_msg_ret-tabix.
              CONCATENATE TEXT-e34 mat TEXT-d13
              tg_form_lote-werks TEXT-d14 tg_form_lote-lgort TEXT-e44
              wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF. " Fim Lgort

          IF wa_marc_66 IS INITIAL.
            MOVE: 'MARC66'                  TO tg_msg_ret-field,
                  c_tab_strip-tab7          TO tg_msg_ret-aba,
                  'GRID7'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e34 mat TEXT-d13 tg_form_lote-werks TEXT-e44
            wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.

        ENDIF. " Fim Werks
        IF wg_header-vkorg IS NOT INITIAL.

          IF wa_mvke_66 IS INITIAL.
            MOVE: 'MVKE66'                  TO tg_msg_ret-field,
                  c_tab_strip-tab7          TO tg_msg_ret-aba,
                  'GRID7'                   TO tg_msg_ret-obj,
                  wl_linha                  TO tg_msg_ret-tabix.
            CONCATENATE TEXT-e34 mat TEXT-d11 wg_header-vkorg TEXT-d15 TEXT-e44
            wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.

        ENDIF.
        CLEAR mat.
      ENDIF. " Fim Matnr
*** BUG - 58231 - CSB - Inicio
      IF tg_form_lote-lgort IS NOT INITIAL.

        SELECT SINGLE lgort
          INTO vlgort
          FROM t001l
          WHERE werks EQ tg_form_lote-werks
            AND lgort EQ tg_form_lote-lgort.

        IF vlgort IS INITIAL.

          MOVE: 'LGORT'                   TO tg_msg_ret-field,
                c_tab_strip-tab7          TO tg_msg_ret-aba,
                'GRID7'                   TO tg_msg_ret-obj,
                wl_linha                  TO tg_msg_ret-tabix.

          CONCATENATE TEXT-e04 TEXT-e44 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.

          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.

        ENDIF.
      ENDIF.
*** BUG - 58231 - CSB - Fim

    ENDLOOP.

  ENDIF.

*    Fim Lote.
  " ELIMINAR  ERRO DE CAMPOS NÃO OBRIGATORIOS
  DATA vaba TYPE zsdt0086-aba.
  LOOP AT tg_msg_ret.
    wl_tabix = sy-tabix.
    IF tg_msg_ret-aba IS INITIAL. " DADOS GERAIS
      READ TABLE tl_0086 WITH KEY aba   = 'DADOS GERAIS'
                                  campo = tg_msg_ret-field.
      IF sy-subrc = 0.
        DELETE tg_msg_ret INDEX wl_tabix.
      ENDIF.
    ELSE.
      CLEAR vaba.
      CASE tg_msg_ret-aba.
        WHEN c_tab_strip-tab1.
          vaba = 'Condição de Pagamento'.
        WHEN c_tab_strip-tab2.
          vaba = 'Produto/Quantidade'.
        WHEN c_tab_strip-tab4.
          vaba = 'Pagamento Antecipado'.
        WHEN c_tab_strip-tab6.
          vaba = 'Adiantamento Externo'.
        WHEN c_tab_strip-tab5.
          vaba = 'Logistica'.
        WHEN c_tab_strip-tab3.
          vaba = 'Preço'.
        WHEN c_tab_strip-tab6.
          vaba = 'Formação de Lote'.
        WHEN c_tab_strip-tab7.
          vaba = 'Instrução'.
        WHEN OTHERS.

      ENDCASE.

      IF vaba IS NOT INITIAL.
        TRANSLATE  vaba  TO UPPER CASE.
        READ TABLE tl_0086 WITH KEY aba   = vaba
                                      campo = tg_msg_ret-field.

        IF sy-subrc = 0.
          DELETE tg_msg_ret INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

  IF  wg_header-param_espec EQ 'Z'.

    DATA tl_0143_qtd TYPE TABLE OF zsdt0143 WITH HEADER LINE.
    DATA lv_qtde TYPE zsdt0143-quatidade.
    DATA lv_quantidade TYPE zsdt0143-quatidade.


    FREE wl_linha.
    CLEAR: lv_quantidade.

    LOOP AT tg_itens.
      wl_linha = sy-tabix.
      SELECT SINGLE ort01
       FROM lfa1
       INTO  @DATA(lv_porto)
      WHERE lifnr EQ @tg_itens-terminal.

      IF  lv_porto NE wg_header-porto.
        IF lv_porto EQ 'GUARUJA' AND wg_header-porto NE 'SANTOS'.

          CONCATENATE TEXT-d23 wl_linha TEXT-d24  INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      wl_qprod_total = tg_itens-zmeng. "Quantidade total aba produto/quantidade

    ENDLOOP.

*    #128361 - PQ
*    "#93180 v_quantidade Validar total da quantidade fixada com contrato.
*    SELECT SINGLE quatidade , tolerancia
*     FROM zsdt0143
*     INTO  (@DATA(lv_quantidade), @DATA(lv_tolerancia) )
*     WHERE id_contrato EQ @wg_header-id_contrato.
*
*    lv_quantidade = lv_quantidade + ( ( lv_quantidade * lv_tolerancia   ) / 100 ).

    SELECT *
     FROM zsdt0143
     INTO TABLE tl_0143_qtd
     WHERE contrato = wg_header-bstkd
       AND empresa = wg_header-vkorg.

    IF sy-subrc = 0.
      LOOP AT tl_0143_qtd.
        CLEAR: lv_qtde.

        lv_qtde = tl_0143_qtd-quatidade + ( ( tl_0143_qtd-quatidade * tl_0143_qtd-tolerancia ) / 100 ).
        lv_quantidade = lv_quantidade + lv_qtde.

      ENDLOOP.
    ENDIF.

*    #128361 - PQ

    IF wl_qtd_fixar > lv_quantidade   .
      MOVE : TEXT-z01  TO   tg_msg_ret-msg .
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
*      CLEAR: lv_quantidade ,lv_tolerancia.

    ENDIF.

    "Validar quantidade total aba produto quantidade

    IF wl_qprod_total > wl_qtd_fixar .
      MOVE : TEXT-z02  TO   tg_msg_ret-msg .
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
*      CLEAR: lv_quantidade ,lv_tolerancia.

    ENDIF.

  ENDIF.

  " 20.09.2024 - 147331 - RAMON -->
  PERFORM f_valida_saldo_fixacao.
  " 20.09.2024 - 147331 - RAMON --<


  PERFORM f_valida_lote_obg. "#180502 28.05.2025 - Renato

*  "16.01.2024 - RAMON - sort na tabela pq estava duplicando -->
*  SORT tg_msg_ret.
*  DELETE ADJACENT DUPLICATES FROM tg_msg_ret COMPARING ALL FIELDS.
*  "16.01.2024 - RAMON --<

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
*     I_SHOW        = C_X
      i_repid       = sy-repid
      i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm,

        tl_0064      TYPE TABLE OF zsdt0064 WITH HEADER LINE,
        tl_t052      TYPE TABLE OF t052 WITH HEADER LINE,
        wl_tipo_calc TYPE zsdt0059-tipo_calc,
        wl_vbfa      TYPE vbfa,
        wl_style     LIKE cl_gui_alv_grid=>mc_style_enabled,
        wa_t052      TYPE t052,
        block_r      TYPE posnr,
        tabix_r      TYPE sy-tabix.

  CHECK lcl_busca_tela=>exit_tela( sy-ucomm ) IS INITIAL.

  CLEAR: tl_ucomm, tl_ucomm[], tl_0064[].

  IF tg_cond_esp[] IS NOT INITIAL.
    SELECT *
      FROM t052
      INTO TABLE tl_t052
       FOR ALL ENTRIES IN tg_cond_esp
        WHERE zterm EQ tg_cond_esp-zterm.

  ENDIF.

  IF NOT block_botao IS INITIAL.

    MOVE: c_liberar TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

  ENDIF.

  IF gv_0235_db IS INITIAL. "# PERFORMANCE

    gv_0235_db = abap_true.

    SELECT * FROM zsdt0235
     INTO TABLE it_0235
      WHERE tp_venda EQ wg_header-tp_venda.

  ENDIF.

  IF ( wg_acao NE c_add ) AND ( wg_acao NE c_modif ) AND
     ( wg_acao NE c_modif_qtd ) AND ( wg_acao NE c_modif_c_ov ).

    IF ( wg_header-param_espec EQ space
    OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
    OR wg_header-param_espec EQ c_ax
*-CS2021000615 - 17.06.2021 - JT - fim
    OR wg_header-param_espec EQ c_m
    OR wg_header-param_espec EQ c_z  )" CS2020000373 23/08/2022 -LP
    AND ( wg_header-auart NE 'ZEXP'
      AND wg_header-auart NE 'ZEXI' ).

      IF ( wg_header-status EQ  c_p ).
        MOVE: c_sol_aprov TO tl_ucomm.
        APPEND tl_ucomm.
        CLEAR: tl_ucomm.
      ENDIF.

*   "// PBI-59125 Melhoria de perfomace Inicio
      READ TABLE tl_0064 WITH KEY uname = sy-uname TRANSPORTING NO FIELDS.
*          SELECT *
*              FROM zsdt0064
*              INTO TABLE tl_0064
*               WHERE uname EQ sy-uname.
*   "// PBI-59125 Melhoria de perfomace Fim

      IF sy-subrc IS NOT INITIAL.

        MOVE: c_liberar TO tl_ucomm.

        APPEND tl_ucomm.
        CLEAR: tl_ucomm.

        MOVE: c_reprov TO tl_ucomm.

        APPEND tl_ucomm.
        CLEAR: tl_ucomm.


      ELSE.
        CLEAR: tl_0064.
        READ TABLE tl_0064
          WITH KEY werks = wg_header-vkbur.

        IF sy-subrc IS NOT INITIAL.
          MOVE: c_liberar TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

          MOVE: c_reprov TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

        ENDIF.
        CLEAR: tl_0064.
        READ TABLE tl_0064
          WITH KEY solic = c_x.
        IF tl_0064-solic IS INITIAL.
          MOVE: c_add TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

          MOVE: c_modif TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

          MOVE: c_modif_qtd TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

          MOVE: c_redist TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

*          MOVE: C_SHOW_MSGRE TO TL_UCOMM.
*
*          APPEND TL_UCOMM.
*          CLEAR: TL_UCOMM.

          MOVE: c_sol_aprov TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

          MOVE: c_enviar TO tl_ucomm.

          APPEND tl_ucomm.
          CLEAR: tl_ucomm.

*-BUG 120423-10.08.2023-JT-inicio
          MOVE: c_trace_cotton TO tl_ucomm.
          APPEND tl_ucomm.
          CLEAR: tl_ucomm.
*-BUG 120423-10.08.2023-JT-fim

        ENDIF.
      ENDIF.
    ELSE.

      IF ( wg_header-param_espec EQ 'A' OR
*-CS2021000615 - 17.06.2021 - JT - inicio
           wg_header-param_espec EQ 'X' )
*-CS2021000615 - 17.06.2021 - JT - fim
        AND wg_header-vkorg EQ '0001'.
        CASE wg_header-status.
          WHEN c_l OR c_p.
            APPEND VALUE #( ucomm = c_sol_aprov ) TO tl_ucomm.
        ENDCASE.
      ELSE.
        APPEND VALUE #( ucomm = c_sol_aprov ) TO tl_ucomm.
      ENDIF.

    ENDIF.

    LOOP AT tg_pgt_ant WHERE adiant IS NOT INITIAL.

    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      MOVE: c_boleto TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ENDIF.

    MOVE: c_save TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_dele TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    LOOP AT tg_itens
       WHERE vbeln NE space.

    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      LOOP AT tg_form_lote
       WHERE vbeln NE space.

      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.
        MOVE: c_modif_qtd TO tl_ucomm.

        APPEND tl_ucomm.
        CLEAR: tl_ucomm.

      ENDIF.
    ENDIF.
    LOOP AT tg_itens
       WHERE vbeln NE space.

    ENDLOOP.
    IF sy-subrc IS NOT INITIAL.
      MOVE: c_redist TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ENDIF.

    IF wg_header-status EQ c_l
      OR sy-ucomm EQ c_liberar.

      MOVE: c_liberar TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ENDIF.

    IF wg_header-param_espec EQ c_p
    OR ( wg_header-auart EQ 'ZEXP'
      OR wg_header-auart EQ 'ZEXI' ).

      IF ( wg_header-param_espec NE 'A' AND
*-CS2021000615 - 17.06.2021 - JT - inicio
           wg_header-param_espec NE 'X' )
*-CS2021000615 - 17.06.2021 - JT - fim
        AND wg_header-vkorg NE '0001'.
        APPEND VALUE #( ucomm = c_sol_aprov ) TO tl_ucomm.
      ENDIF.

      MOVE: c_reprov TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ENDIF.

    IF wg_acao IS INITIAL.
      MOVE: c_show_msgre TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      MOVE: c_reprov TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      MOVE: c_enviar TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      MOVE: c_sol_aprov TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      MOVE: c_redist TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      MOVE: c_liberar TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

      MOVE: c_estrat TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.

*-BUG 120423-10.08.2023-JT-inicio
      MOVE: c_trace_cotton TO tl_ucomm.
      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
*-BUG 120423-10.08.2023-JT-fim

    ENDIF.

*   "// Verifica se existe OV em Algum item caso exista o Botão de Solicitar Aprovação é ocultado.
    DATA(t_itens) = tg_itens[].
    DELETE t_itens WHERE vbeln EQ ' '.

    IF t_itens IS NOT INITIAL.
      APPEND VALUE #( ucomm = c_sol_aprov ) TO tl_ucomm.
    ENDIF.

    IF grid1 IS NOT INITIAL.
*      .
      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog ASSIGNING <fcat>.
        <fcat>-col_pos = <fcat>-col_id.
      ENDLOOP.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-edit = space.

        IF w_fieldcatalog-fieldname EQ 'DESC_ABSOLUTO'.
          CASE wg_header-param_espec.
*-CS2021000615 - 17.06.2021 - JT - inicio
            WHEN c_a OR c_p OR c_ax OR c_z .
*-CS2021000615 - 17.06.2021 - JT - fim
              w_fieldcatalog-no_out = abap_false.
            WHEN OTHERS.
              w_fieldcatalog-no_out = abap_true.
          ENDCASE.
        ENDIF.

        IF ( w_fieldcatalog-fieldname EQ 'BRGEW' )
        OR ( w_fieldcatalog-fieldname EQ 'KURSF' )
        OR ( w_fieldcatalog-fieldname EQ 'VOLUM' )
*        OR ( W_FIELDCATALOG-FIELDNAME EQ 'TERMINAL' )
        OR ( w_fieldcatalog-fieldname EQ 'VOLEH' )
        OR ( w_fieldcatalog-fieldname EQ 'FIXACAO' ).
          CASE wg_header-param_espec.

*-CS2021000615 - 17.06.2021 - JT - inicio
            WHEN: c_a OR c_e OR c_ax OR c_z.
*-CS2021000615 - 17.06.2021 - JT - fim

*-CS2021000615 - 17.06.2021 - AOENNING - inicio
              IF line_exists( it_0235[ tabela = 'ZSDT0053' campo = w_fieldcatalog-fieldname ] ).
                w_fieldcatalog-no_out = c_x.
              ELSE.
                w_fieldcatalog-no_out = space.
              ENDIF.
*-CS2021000615 - 17.06.2021 - AOENNING - fim

*              IF ( wg_header-auart EQ 'ZEXI' OR wg_header-auart EQ 'ZEXP' ).
*                w_fieldcatalog-no_out = space.
*              ELSE.
*                w_fieldcatalog-no_out = c_x.
*              ENDIF.


            WHEN: c_m OR c_z.
              IF (  wg_header-auart EQ 'ZMIT' ) OR (  wg_header-auart EQ 'ZFEX' ) .
                w_fieldcatalog-no_out = space.
              ELSE.
                w_fieldcatalog-no_out = c_x.
              ENDIF.
            WHEN OTHERS.
*              IF ( WG_HEADER-AUART EQ 'ZMIT' )." AND ( W_FIELDCATALOG-FIELDNAME EQ 'TERMINAL' ).
*                W_FIELDCATALOG-NO_OUT = SPACE.
*              ELSEIF ( WG_HEADER-AUART EQ 'ZFEX' )." AND ( W_FIELDCATALOG-FIELDNAME EQ 'TERMINAL' ).
*                W_FIELDCATALOG-NO_OUT = SPACE.
*              ELSE.
              w_fieldcatalog-no_out = c_x.
*              ENDIF.
          ENDCASE.
        ENDIF.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit no_out.
      ENDLOOP.

      SORT t_fieldcatalog BY col_pos.

      " 28.11.2024 - 159383 - RAMON -->
      PERFORM f_tratar_search_help CHANGING t_fieldcatalog[].
      " 28.11.2024 - 159383 - RAMON --<

      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      " 23.09.2024 - 147331 - RAMON -->
      PERFORM f_set_soma_prod_qtde.
      " 23.09.2024 - 147331 - RAMON --<

    ENDIF.

    IF grid2 IS NOT INITIAL.
      CALL METHOD grid2->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
      CALL METHOD grid2->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

    IF grid3 IS NOT INITIAL.

      CALL METHOD grid3->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-edit = space.

        CASE w_fieldcatalog-fieldname.
          WHEN: 'FIXACAO'.
            IF ( wg_header-param_espec NE 'M' OR wg_header-param_espec EQ 'Z' ).
              w_fieldcatalog-no_out = c_x.
            ELSE.
              w_fieldcatalog-no_out = space.
            ENDIF.
        ENDCASE.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.

      CALL METHOD grid3->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

    IF grid4 IS NOT INITIAL.
      CALL METHOD grid4->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.

        READ TABLE tg_fields TRANSPORTING NO FIELDS
          WITH KEY field = w_fieldcatalog-fieldname.

        IF sy-subrc IS INITIAL.
          w_fieldcatalog-edit = space.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDIF.


        IF w_fieldcatalog-fieldname EQ 'CBOT'.
          w_fieldcatalog-edit = abap_true.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDIF.

        IF w_fieldcatalog-fieldname EQ 'SAFRA'.
          w_fieldcatalog-edit = abap_true.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDIF.

      ENDLOOP.

      CALL METHOD grid4->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      LOOP AT <fs_table> ASSIGNING <fs_line>.
        REFRESH: style. "TG_PRECO-STYLE.
        CLEAR: wg_valor, wl_tipo_calc.
*        PERFORM GET_SET_VALORES
*                    USING
*                       'STYLE'
*                       'S'
*                    CHANGING
*                       WG_VALOR.
        ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
        <fs_campo> = style[].

        PERFORM get_set_valores
                    USING
                       'TIPO_CALC'
                       'G'
                    CHANGING
                        wl_tipo_calc.

        IF wl_tipo_calc EQ c_c
        OR wl_tipo_calc EQ c_r.
*          CLEAR: TG_PRECO-NIVEL, TG_PRECO-COD_FP.
          "CLEAR: WG_VALOR.
*          PERFORM GET_SET_VALORES
*                     USING
*                        'COD_FP'
*                        'S'
*                     CHANGING
*                        WG_VALOR.
*
*          PERFORM GET_SET_VALORES
*                     USING
*                        'NIVEL'
*                        'S'
*                     CHANGING
*                        WG_VALOR.
        ENDIF.
        wa_style-fieldname = 'BEZEI'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.
          wa_style-style = alv_style_font_bold_no.
        ELSE.
          wa_style-style =  alv_style_font_bold.
          wa_style-style2 =  alv_style2_no_border_left.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        LOOP AT tg_fields.
          wa_style-fieldname = tg_fields-field. "'FORMULA2'.
          IF wl_tipo_calc NE c_c
          AND wl_tipo_calc NE c_f
          AND wl_tipo_calc NE c_r.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
          ELSE.
            IF wl_tipo_calc EQ c_c
            OR wl_tipo_calc EQ c_r.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold.
            ELSE.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
            ENDIF.
          ENDIF.
          INSERT  wa_style INTO TABLE style .
        ENDLOOP.
        wa_style-fieldname = 'WAERS'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'CBOT'.
        IF wg_acao EQ c_chg_dtv.
          IF wl_tipo_calc EQ c_c  OR wl_tipo_calc EQ c_r.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          ENDIF.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .


* RJF
        wa_style-fieldname = 'SAFRA'.
        IF wg_acao EQ c_chg_dtv.
          IF wl_tipo_calc EQ c_c  OR wl_tipo_calc EQ c_r.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          ENDIF.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

*        IF wg_header-param_espec EQ c_m.
*          wa_style-fieldname = 'ZMENG'.
*          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          INSERT  wa_style INTO TABLE style .
*          wa_style-fieldname = 'VALDT'.
*          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          INSERT  wa_style INTO TABLE style .
*          wa_style-fieldname = 'MONAT'.
*          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          INSERT  wa_style INTO TABLE style.
*        ENDIF.
*        INSERT LINES OF style INTO TABLE tg_preco-style.
*        PERFORM GET_SET_VALORES
*                   USING
*                      'STYLE'
*                      'S'
*                   CHANGING
*                      STYLE[].
        ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
        <fs_campo> = style[].
        IF wl_tipo_calc EQ 'C'
        OR wl_tipo_calc EQ c_r.
          wg_valor = 'C305'.
*          MOVE: 'C305' TO TG_PRECO-LINE_COLOR.
          PERFORM get_set_valores
                   USING
                      'LINE_COLOR'
                      'S'
                   CHANGING
                      wg_valor.
        ELSE.
*          CLEAR TG_PRECO-LINE_COLOR.
          CLEAR: wg_valor.

          PERFORM get_set_valores
                   USING
                      'LINE_COLOR'
                      'S'
                   CHANGING
                      wg_valor.
        ENDIF.
*        MODIFY tg_preco.
      ENDLOOP.
    ENDIF.

*jaime
    IF grid5 IS NOT INITIAL.
      CALL METHOD grid5->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-edit = space.
        IF w_fieldcatalog-fieldname EQ 'NR_PROVIS_INV'.
          IF ( wg_header-param_espec <> 'A' AND
               wg_header-param_espec <> 'X' ).
            w_fieldcatalog-no_out = abap_true.
          ELSE.
            w_fieldcatalog-no_out = abap_false.
          ENDIF.
        ENDIF.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
      CALL METHOD grid5->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

    IF grid6 IS NOT INITIAL.
      CALL METHOD grid6->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog ASSIGNING <fcat>.
        <fcat>-edit = abap_false.
      ENDLOOP.

      CALL METHOD grid6->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

    IF grid7 IS NOT INITIAL.
      CALL METHOD grid7->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog ASSIGNING <fcat>.
        <fcat>-edit = abap_false.
      ENDLOOP.
      CALL METHOD grid7->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

*    PRECO FRAME - INICIO
    IF grid8 IS NOT INITIAL.

      CALL METHOD grid8->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      IF wg_prec EQ c_chg_dtv.

        LOOP AT t_fieldcatalog INTO w_fieldcatalog   WHERE fieldname EQ 'CBOT'.
          w_fieldcatalog-edit = c_x.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.

        LOOP AT t_fieldcatalog INTO w_fieldcatalog   WHERE fieldname EQ 'SAFRA'. "RJF
          w_fieldcatalog-edit = c_x.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.

        LOOP AT <fs_table> ASSIGNING <fs_line>.
          REFRESH: style.
          CLEAR: wg_valor, wl_tipo_calc.


          PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.
          READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY fixacao     = wg_valor
                                                              status_itm  = c_f.
          IF sy-subrc IS INITIAL.
            PERFORM get_set_valores USING 'TIPO_CALC' 'G' CHANGING wl_tipo_calc.

            CASE wl_tipo_calc.
              WHEN c_c OR c_r.
              WHEN OTHERS.
                ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
                <fs_campo> = style[].
            ENDCASE.
          ELSE.
            ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.

            wa_style-fieldname = 'CBOT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            <fs_campo> = style[].

          ENDIF.
        ENDLOOP.

        FREE: wg_acao.

      ELSE.

        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname EQ 'FORMULA2'
             OR fieldname EQ 'WAERS'
             OR fieldname EQ 'SAFRA' "RJF
             OR fieldname EQ 'CBOT'.

          w_fieldcatalog-edit = c_x.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.

      ENDIF.

      CALL METHOD grid8->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.


    ENDIF.

    IF grid10 IS NOT INITIAL.
      CALL METHOD grid10->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog ASSIGNING <fcat>.
        <fcat>-edit = abap_false.
      ENDLOOP.

      CALL METHOD grid10->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.


  ELSEIF wg_acao EQ c_add
      OR wg_acao EQ c_modif
      OR wg_acao EQ c_modif_c_ov.


    MOVE: c_boleto TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.


    MOVE: c_atual TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    IF wg_acao NE c_modif.
      MOVE: c_dele TO tl_ucomm.

      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ENDIF.

    MOVE: c_modif_qtd TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_redist TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

*     IF WG_HEADER-PARAM_ESPEC EQ C_P.
    MOVE: c_sol_aprov TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_liberar TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_reprov TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

*    ENDIF.

    IF grid1 IS NOT INITIAL.
*.
      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-col_pos = w_fieldcatalog-col_id.

        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING col_pos.
      ENDLOOP.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname EQ 'MATNR'
            OR fieldname EQ 'FIXACAO'
            OR fieldname EQ 'WERKS'
            OR fieldname EQ 'PONTO_C'
            OR fieldname EQ 'TERMINAL'
            OR fieldname EQ 'LGORT'
            OR fieldname EQ 'CHARG'
            OR fieldname EQ 'ZMENG'
            OR fieldname EQ 'ZIEME'
            OR fieldname EQ 'PMEIN'
            OR fieldname EQ 'VALDT'
            OR fieldname EQ 'BRGEW'
            OR fieldname EQ 'KURSF'
            OR fieldname EQ 'AUART'
*            OR FIELDNAME EQ 'VOLUM'
            OR fieldname EQ 'VOLEH'
            OR fieldname EQ 'DESC_ABSOLUTO'
            OR fieldname EQ 'KUNNR'.

        IF w_fieldcatalog-fieldname EQ 'DESC_ABSOLUTO'.
          CASE wg_header-param_espec.
*-CS2021000615 - 17.06.2021 - JT - inicio
            WHEN c_a OR c_p OR c_ax OR c_z.
*-CS2021000615 - 17.06.2021 - JT - fim
              w_fieldcatalog-no_out = abap_false.
            WHEN OTHERS.
              w_fieldcatalog-no_out = abap_true.
          ENDCASE.
        ENDIF.

*        IF W_FIELDCATALOG-FIELDNAME EQ 'KUNNR'.
*          IF WG_REDISTRIBUIR EQ C_X.
*            W_FIELDCATALOG-EDIT = C_X.
*          ELSE.
*            W_FIELDCATALOG-EDIT = SPACE.
*          ENDIF.
*        ELSE.
        IF w_fieldcatalog-fieldname EQ 'BRGEW'
        OR w_fieldcatalog-fieldname EQ 'KURSF'
        OR w_fieldcatalog-fieldname EQ 'VOLUM'
*        OR W_FIELDCATALOG-FIELDNAME EQ 'TERMINAL'
        OR w_fieldcatalog-fieldname EQ 'FIXACAO'
        OR w_fieldcatalog-fieldname EQ 'VOLEH'.


          IF ( wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
               wg_header-param_espec EQ c_ax OR
                wg_header-param_espec EQ c_z )" CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
             AND ( wg_header-auart EQ 'ZEXI' OR wg_header-auart EQ 'ZEXP' ).
*-CS2021000615 - 17.06.2021 - AOENNING - inicio
*            w_fieldcatalog-no_out = space. "Comentado.
*-CS2021000615 - 17.06.2021 - AOENNING - fim

            IF  w_fieldcatalog-fieldname EQ 'FIXACAO'.
*-CS2021000615 - 17.06.2021 - AOENNING - inicio
*              w_fieldcatalog-no_out = c_x.  "Comentado.
*-CS2021000615 - 17.06.2021 - AOENNING - fim
            ENDIF.
          ELSEIF ( wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z ) AND ( w_fieldcatalog-fieldname EQ 'FIXACAO' ).
            w_fieldcatalog-no_out = space.
*          ELSEIF ( WG_HEADER-AUART EQ 'ZMIT' ) AND ( W_FIELDCATALOG-FIELDNAME EQ 'TERMINAL' ).
*            W_FIELDCATALOG-NO_OUT = SPACE.
*          ELSEIF ( WG_HEADER-AUART EQ 'ZFEX' ) AND ( W_FIELDCATALOG-FIELDNAME EQ 'TERMINAL' ).
*            W_FIELDCATALOG-NO_OUT = SPACE.
          ELSE.
*-CS2021000615 - 17.06.2021 - AOENNING - inicio
*            w_fieldcatalog-no_out = c_x. "Comentado.
*-CS2021000615 - 17.06.2021 - AOENNING - fim
          ENDIF.

        ENDIF.

        IF w_fieldcatalog-no_out IS INITIAL.
          w_fieldcatalog-edit = c_x.
        ENDIF.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit no_out.
      ENDLOOP.
      IF wg_header-param_espec EQ c_p.

        CLEAR: w_fieldcatalog.
        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'VLRTOT'.

        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'DMBTR'.

        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'DESC_ABSOLUTO'.

      ELSEIF ( wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
               wg_header-param_espec EQ c_ax OR
               wg_header-param_espec EQ c_z )." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
        CLEAR: w_fieldcatalog.
        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'DMBTR'.

        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'VLRTOT'.
      ELSEIF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'CHARG'.
      ELSE.
        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit
        WHERE fieldname EQ 'VLRTOT'
           OR fieldname EQ 'DMBTR'.
      ENDIF.
**          Bloquear campos
      REFRESH: style.
      CLEAR: wa_style.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
       WHERE edit EQ c_x
         AND fieldname NE 'ZMENG'
         AND fieldname NE 'FIXACAO'
         AND fieldname NE 'VALDT'.
        IF wg_header-param_espec EQ c_p
        AND w_fieldcatalog-fieldname EQ 'VLRTOT'.
          CONTINUE.
        ELSEIF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z
        AND  w_fieldcatalog-fieldname EQ 'CHARG'.
          CONTINUE.
        ENDIF.
        wa_style-fieldname = w_fieldcatalog-fieldname.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_no_delete_row.
        INSERT  wa_style INTO TABLE style .
      ENDLOOP.


      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        IF ( wg_header-param_espec EQ c_u ).
          CASE w_fieldcatalog-fieldname.
            WHEN 'MATNR'    OR 'WERKS' OR 'PONTO_C' OR 'FORNECEDOR'
              OR 'LENTREGA' OR 'LGORT' OR 'CHARG'   OR 'CLASSIFICACAO'
              OR 'ZMENG'    OR 'ZIEME' OR 'VOLUM'   OR 'VOLEH'
              OR 'DMBTR'    OR 'PMEIN' OR 'VLRTOT'.

              w_fieldcatalog-edit = c_x.

            WHEN OTHERS.
              w_fieldcatalog-edit = ''.
          ENDCASE.

        ENDIF.

      ENDLOOP.

*      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
***         FIM DE BLOQUEI DE CAMPOS

      LOOP AT tg_itens.

        CLEAR: wa_style.
        REFRESH: tg_itens-style, style.

        IF ( tg_itens-item_edit IS INITIAL ) AND ( tg_itens-status_itm NE 'F' ).
          INSERT LINES OF style INTO TABLE tg_itens-style.
        ELSE.

          IF ( tg_itens-status_itm EQ 'F' ).

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'FIXACAO'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'WERKS'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PONTO_C'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'TERMINAL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LGORT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'CHARG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZMENG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'DESC_ABSOLUTO'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

          ENDIF.
          INSERT LINES OF style INTO TABLE tg_itens-style.
          MODIFY tg_itens.
        ENDIF.
      ENDLOOP.


*** Bloqueia campos com das linhas com OV geradas
      IF ( wg_header-param_espec EQ c_p
      OR wg_header-param_espec EQ c_m
      OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
      OR wg_header-param_espec EQ c_ax
        OR wg_header-param_espec EQ c_z )" CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
      AND wg_acao EQ c_modif_c_ov.
        REFRESH: style.
        CLEAR: wa_style.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE edit EQ c_x.

          wa_style-fieldname = w_fieldcatalog-fieldname.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_no_delete_row.
          INSERT  wa_style INTO TABLE style .
        ENDLOOP.
        LOOP AT tg_itens
          WHERE vbeln IS NOT INITIAL.
          REFRESH: tg_itens-style.
          INSERT LINES OF style INTO TABLE tg_itens-style.
          MODIFY tg_itens.
        ENDLOOP.

      ENDIF.


*       Bloqueia o Campo VALDT Data de Vencimento quando Venda Simples.
      IF wg_header-param_espec IS INITIAL.
        IF wg_header-auart EQ 'ZMIT' OR wg_header-auart EQ 'ZFEX'.


          SELECT SINGLE *
              FROM t052
              INTO wa_t052
            WHERE zterm EQ wg_cond_pgt-zterm.

          LOOP AT tg_itens.

            REFRESH: style.
            CLEAR: wa_style.

            IF wa_t052-ztag1 IS NOT INITIAL.

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
              CLEAR: tg_itens-valdt.
            ELSE.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .
            ENDIF.

            DELETE tg_itens-style WHERE fieldname EQ 'VALDT'.
            INSERT LINES OF style INTO TABLE tg_itens-style.
            MODIFY tg_itens.

          ENDLOOP.

        ENDIF.
      ENDIF.

      IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
        LOOP AT tg_itens.

          REFRESH: style.
          CLEAR: wa_style.
          READ TABLE tg_cond_esp WITH KEY fixacao = tg_itens-fixacao.
          READ TABLE tl_t052 WITH KEY zterm = tg_cond_esp-zterm.

          IF tl_t052-ztag1 IS NOT INITIAL OR tg_itens-status_itm EQ c_f OR tg_itens-vbeln IS NOT INITIAL.
            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .
            IF tl_t052-ztag1 IS NOT INITIAL.
              CLEAR: tg_itens-valdt.
            ENDIF.
          ELSE.
            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            INSERT  wa_style INTO TABLE style .
          ENDIF.

          DELETE tg_itens-style WHERE fieldname EQ 'VALDT'.
          INSERT LINES OF style INTO TABLE tg_itens-style.
          MODIFY tg_itens.

        ENDLOOP.
      ENDIF.

      IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
         wg_header-param_espec EQ c_ax OR
         wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
        LOOP AT tg_itens.
          FREE: style.
          CLEAR: wa_style.

          IF NOT line_exists( tg_itens-style[ fieldname = 'LOTES_VOL' ] ).

            wa_style-fieldname = 'LOTES_VOL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_button.

            INSERT wa_style INTO TABLE style.
            INSERT LINES OF style INTO TABLE tg_itens-style.
            MODIFY tg_itens.

          ENDIF.

        ENDLOOP.
      ENDIF.

      SORT t_fieldcatalog BY col_pos.

      " 28.11.2024 - 159383 - RAMON -->
      PERFORM f_tratar_search_help CHANGING t_fieldcatalog[].
      " 28.11.2024 - 159383 - RAMON --<


      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

    ENDIF.

    IF grid4 IS NOT INITIAL.
      CALL METHOD grid4->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        READ TABLE tg_fields TRANSPORTING NO FIELDS
        WITH KEY field = w_fieldcatalog-fieldname.
        IF sy-subrc IS INITIAL
        OR w_fieldcatalog-fieldname EQ 'WAERS'
        OR w_fieldcatalog-fieldname EQ 'CBOT'
        OR w_fieldcatalog-fieldname EQ 'SAFRA' "RJF
        OR w_fieldcatalog-fieldname EQ 'MONAT'
        OR w_fieldcatalog-fieldname EQ 'VALDT'
        OR w_fieldcatalog-fieldname EQ 'POSNR1'.
*
          IF (   w_fieldcatalog-fieldname EQ 'VALDT'
             OR  w_fieldcatalog-fieldname EQ 'ZMENG' )
          AND wg_header-param_espec NE c_m AND wg_header-param_espec NE c_z .
            CONTINUE.
          ENDIF.

          w_fieldcatalog-edit = c_x.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDIF.
      ENDLOOP.
      CALL METHOD grid4->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      LOOP AT <fs_table> ASSIGNING <fs_line>.
        REFRESH: style. "TG_PRECO-STYLE.
        CLEAR: wg_valor, wl_tipo_calc.
        ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
        <fs_campo> = style[].

        IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
          PERFORM get_set_valores
                     USING
                        'POSNR'
                        'G'
                     CHANGING
                         wg_valor.

*          READ TABLE TG_ITENS TRANSPORTING NO FIELDS
*            WITH KEY FIXACAO     = WG_VALOR
*                     VBELN       = SPACE
*                     STATUS_ITM  = SPACE.
*
*          IF SY-SUBRC IS INITIAL.
*            WL_STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*
*          ELSE.
*            WL_STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*          ENDIF.

          READ TABLE tg_itens TRANSPORTING NO FIELDS
            WITH KEY fixacao     = wg_valor
                     status_itm  = c_f.

          IF sy-subrc IS INITIAL.
            wl_style = cl_gui_alv_grid=>mc_style_disabled.
          ELSE.
            wl_style = cl_gui_alv_grid=>mc_style_enabled.
          ENDIF.
        ELSE.
          wl_style = cl_gui_alv_grid=>mc_style_enabled.
        ENDIF.

        PERFORM get_set_valores
                    USING
                       'TIPO_CALC'
                       'G'
                    CHANGING
                        wl_tipo_calc.
        LOOP AT tg_fields.
          wa_style-fieldname = tg_fields-field. "'FORMULA2'.
          CLEAR wg_valor.
          PERFORM get_set_valores
                    USING
                       'COD_FP'
                       'G'
                    CHANGING
                        wg_valor.

          READ TABLE tg_preco_n
            WITH KEY field = tg_fields-field
                     cod_fp = wg_valor.

          IF wl_tipo_calc NE c_c
          AND wl_tipo_calc NE c_f
          AND wl_tipo_calc NE c_r
          AND sy-subrc IS INITIAL.
            wa_style-style = wl_style + alv_style_font_bold_no.
          ELSE.
*          CLEAR: TG_PRECO-NIVEL, TG_PRECO-COD_FP.
            IF wl_tipo_calc EQ c_c
            OR wl_tipo_calc EQ c_r.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold.
            ELSEIF wl_tipo_calc EQ c_f.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
            ELSE.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
            ENDIF.
*          WA_STYLE-STYLE2 =  ALV_STYLE2_NO_BORDER_LEFT.
          ENDIF.
          INSERT  wa_style INTO TABLE style .
        ENDLOOP.
        wa_style-fieldname = 'BEZEI'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.
          wa_style-style = alv_style_font_bold_no.
        ELSE.
          wa_style-style =  alv_style_font_bold.
          wa_style-style2 =  alv_style2_no_border_left.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'WAERS'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.
          wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'CBOT'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.

          IF wg_prec EQ c_chg_dtv.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          ELSE.
            wa_style-style = wl_style.
          ENDIF.

        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.

        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'SAFRA'. "RJF
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.

          IF wg_prec EQ c_chg_dtv.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          ELSE.
            wa_style-style = wl_style.
          ENDIF.

        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.

        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'VALDT'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r
        AND wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
          wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .
*          wa_style-fieldname = 'ZMENG'.
*          IF tg_preco-tipo_calc NE c_c
*            AND wg_header-param_espec EQ c_m.
*            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*          ELSE.
*            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          ENDIF.
*          INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'MONAT'.
        IF wl_tipo_calc NE c_c AND wl_tipo_calc NE c_r AND ( wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z ).
          wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.

          "Regra nova solicitado no dia 10.03.2015 - Habilitar a coluna mês para edição.

*          CASE WG_HEADER-TP_VENDA.
*            WHEN: '2' OR '93' OR '23' OR '113'.

          PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
          CASE wg_valor.
            WHEN: 'PREMIO' OR 'SPREAD'.
              wa_style-style = wl_style.
            WHEN OTHERS.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDCASE.

*            WHEN OTHERS.
*              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*          ENDCASE.
*
*          IF ( WG_HEADER-TP_VENDA NE '2' ).
*
*              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*
*          ELSE.
*
*            PERFORM GET_SET_VALORES
*                      USING
*                         'BEZEI'
*                         'G'
*                      CHANGING
*                          WG_VALOR.
*
*            CASE WG_VALOR.
*              WHEN: 'PREMIO' OR 'SPREAD'.
*                WA_STYLE-STYLE = WL_STYLE.
*              WHEN OTHERS.
*                WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*            ENDCASE.
*
*
*          ENDIF.
        ENDIF.
        INSERT  wa_style INTO TABLE style .


        wa_style-fieldname = 'POSNR1'.
        IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r
          AND ( wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z ).
          wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

*          INSERT LINES OF style INTO TABLE tg_preco-style.
        ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
        <fs_campo> = style[].
        IF wl_tipo_calc EQ 'C'
        OR wl_tipo_calc EQ c_r.
          wg_valor = 'C305'.
*          MOVE: 'C305' TO TG_PRECO-LINE_COLOR.
          PERFORM get_set_valores
                   USING
                      'LINE_COLOR'
                      'S'
                   CHANGING
                      wg_valor.
        ELSE.
*          CLEAR TG_PRECO-LINE_COLOR.
          CLEAR: wg_valor.

          PERFORM get_set_valores
                   USING
                      'LINE_COLOR'
                      'S'
                   CHANGING
                      wg_valor.
        ENDIF.
*          MODIFY tg_preco.
      ENDLOOP.

    ENDIF.

*-CS2022000332-#78223-02.06.2022-JT-inicio
    IF wg_header-param_espec = c_a  OR
       wg_header-param_espec = c_x OR
      wg_header-param_espec = c_z.
      IF grid5 IS NOT INITIAL.
        CALL METHOD grid5->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname NE 'NAME1'
            AND fieldname NE 'ADIANT'
            AND fieldname NE 'AUGBL'
            AND fieldname NE 'AUGDT'.

          IF ( wg_cond_pgt-pgto_ant EQ c_x
          OR   wg_cond_pgt-pgto_ant EQ c_n )
          AND ( wg_header-param_espec EQ c_p
           OR   wg_header-param_espec EQ c_a
           OR   wg_header-param_espec EQ c_x
            OR   wg_header-param_espec EQ c_z )." CS2020000373 23/08/2022 -LP
            IF w_fieldcatalog-fieldname EQ 'BANKS'
            OR w_fieldcatalog-fieldname EQ 'BANKL'
            OR w_fieldcatalog-fieldname EQ 'BANKN'
            OR w_fieldcatalog-fieldname EQ 'SWIFT'.
              w_fieldcatalog-no_out = c_x.
              w_fieldcatalog-edit = space.
            ELSEIF w_fieldcatalog-fieldname EQ 'KURSF'
                OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.
              w_fieldcatalog-no_out = space.
              IF w_fieldcatalog-fieldname EQ 'KURSF'.
                w_fieldcatalog-edit = c_x.
              ELSE.
                w_fieldcatalog-edit = space.
              ENDIF.
            ELSE.
              w_fieldcatalog-no_out = space.
              w_fieldcatalog-edit = c_x.
            ENDIF.
          ELSE.
            IF w_fieldcatalog-fieldname EQ 'KURSF'
            OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.

              w_fieldcatalog-no_out = c_x.
              w_fieldcatalog-edit = space.
            ELSE.
              w_fieldcatalog-no_out = space.
              w_fieldcatalog-edit = c_x.
            ENDIF.
          ENDIF.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
        REFRESH: style.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
           WHERE edit  IS NOT INITIAL.

          wa_style-fieldname = w_fieldcatalog-fieldname.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDLOOP.
        LOOP AT tg_adto_ext.
          REFRESH: tg_adto_ext-style.
          IF tg_adto_ext-adiant IS NOT INITIAL.
            INSERT LINES OF style INTO TABLE tg_adto_ext-style.
          ENDIF.
          MODIFY tg_adto_ext TRANSPORTING style.
        ENDLOOP.
        CALL METHOD grid5->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.
    ELSE.
      IF grid5 IS NOT INITIAL.
        CALL METHOD grid5->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname  = 'NR_PROVIS_INV'.
          w_fieldcatalog-no_out = abap_true.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
        CALL METHOD grid5->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.
    ENDIF.
*jaime
*-CS2022000332-#78223-02.06.2022-JT-fim

*   Instrução
    IF grid6 IS NOT INITIAL.
      CALL METHOD grid6->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog ASSIGNING <fcat>.
* Somente Leitura conforme chamado CS2017001253
*        IF WG_HEADER-PARAM_ESPEC EQ C_A.
*          <FCAT>-EDIT = ABAP_FALSE.
*        ELSE.
*        <FCAT>-EDIT = ABAP_TRUE.
*        ENDIF.
      ENDLOOP.

      CALL METHOD grid6->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

*   PRECO FRAME - INICIO
    IF grid8 IS NOT INITIAL.

      CALL METHOD grid8->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog ASSIGNING <fcat>
        WHERE fieldname EQ 'FORMULA2'
           OR fieldname EQ 'WAERS'
           OR fieldname EQ 'CBOT'
           OR fieldname EQ 'SAFRA'. "RJF
        <fcat>-edit = abap_true.
      ENDLOOP.

      CALL METHOD grid8->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

    ENDIF.

* Pedido
    IF ( grid10 IS NOT INITIAL ).

      DATA: wl_pedido TYPE ty_pedido.

      IF ( tg_pedido[] IS INITIAL ).
        wl_pedido-posnr = 10.
        APPEND wl_pedido TO tg_pedido[].
      ENDIF.

      CALL METHOD grid10->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog ASSIGNING <fcat>
        WHERE fieldname EQ 'MATNR'    OR
              fieldname EQ 'WERKS'    OR
              fieldname EQ 'PONTO_C'  OR
              fieldname EQ 'FORNECEDOR'    OR
              fieldname EQ 'LGORT'    OR
              fieldname EQ 'CHARG'    OR
              fieldname EQ 'CLASSIFICACAO' OR
              fieldname EQ 'USD_TO'   OR
              fieldname EQ 'ZMENG'    OR
              fieldname EQ 'ZIEME'    OR
              fieldname EQ 'VOLUM'    OR
              fieldname EQ 'VOLEH'    OR
              fieldname EQ 'DMBTR'    OR
              fieldname EQ 'PMEIN'    OR
              fieldname EQ 'VLRTOT'   OR
              fieldname EQ 'WAERK'    OR
              fieldname EQ 'LIBRA_TO' OR
              fieldname EQ 'VLR_TOT_FRM_USD' OR
              fieldname EQ 'KUNNR'    OR
              fieldname EQ 'TERMINAL' OR
              fieldname EQ 'LENTREGA' OR
              fieldname EQ 'INCO1'    OR
              fieldname EQ 'INCO2'    OR
              fieldname EQ 'DCO'      OR
              fieldname EQ 'AVISO'    OR
              fieldname EQ 'VBELN'.

        <fcat>-edit = abap_true.
      ENDLOOP.

      CALL METHOD grid10->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

    ENDIF.

*      PRECO FRAME - FIM

    CALL METHOD grid7->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = t_fieldcatalog.
    LOOP AT t_fieldcatalog ASSIGNING <fcat>
              WHERE fieldname NE 'STATUS'
                AND fieldname NE 'ZSEQ_INST'
                AND fieldname NE 'POSNR'
                AND fieldname NE 'MAKTX'
                AND fieldname NE 'VLRTOT'
                AND fieldname NE 'USD_TO'
                AND fieldname NE 'VLR_TOT_FRM_USD'
                AND fieldname NE 'VBELN' .
* Somente Leitura conforme chamado CS2017001253
*      IF WG_HEADER-PARAM_ESPEC EQ C_A.
*        <FCAT>-EDIT = ABAP_FALSE.
*      ELSE.
*      <FCAT>-EDIT = ABAP_TRUE.
*      ENDIF.

    ENDLOOP.
**          Bloquear campos
*    REFRESH: STYLE.
*    CLEAR: WA_STYLE.
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG
*     WHERE EDIT EQ C_X
*       AND FIELDNAME NE 'ZMENG'
*       AND FIELDNAME NE 'VALDT'.
*
*      WA_STYLE-FIELDNAME = W_FIELDCATALOG-FIELDNAME.
*      WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED + ALV_STYLE_NO_DELETE_ROW.
*      INSERT  WA_STYLE INTO TABLE STYLE .
*    ENDLOOP.
*      INSERT LINES OF STYLE INTO TABLE TG_ITENS-STYLE.
***         FIM DE BLOQUEI DE CAMPOS
*    LOOP AT TG_FORM_LOTE.
*      REFRESH: TG_FORM_LOTE-STYLE.
*      INSERT LINES OF STYLE INTO TABLE TG_FORM_LOTE-STYLE.
*      MODIFY TG_FORM_LOTE.
*    ENDLOOP.

*** Bloqueia campos com das linhas com OV geradas
    IF  wg_acao EQ c_modif_c_ov.
      LOOP AT tg_form_lote.
        REFRESH: tg_form_lote-style, style.
        CLEAR: wa_style.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE edit EQ c_x.

*          WA_STYLE-FIELDNAME = W_FIELDCATALOG-FIELDNAME.
*          IF TG_FORM_LOTE-VBELN IS NOT INITIAL OR TG_FORM_LOTE-STATUS EQ 'D'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_no_delete_row.
*          ELSE.
*            WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          ENDIF.

          INSERT  wa_style INTO TABLE style .
        ENDLOOP.

        INSERT LINES OF style INTO TABLE tg_form_lote-style.
        MODIFY tg_form_lote.
      ENDLOOP.
    ENDIF.
    CALL METHOD grid7->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.


    IF wg_acao EQ c_modif_c_ov.
      IF ( wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
           wg_header-param_espec EQ c_ax OR
        wg_header-param_espec EQ c_z )." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
        IF grid2 IS NOT INITIAL.

          CALL METHOD grid2->get_frontend_fieldcatalog
            IMPORTING
              et_fieldcatalog = t_fieldcatalog.

          LOOP AT t_fieldcatalog INTO w_fieldcatalog
            WHERE fieldname EQ 'VALDT'
               OR fieldname EQ 'POSNR'
               OR fieldname EQ 'DMBTR'
               OR fieldname EQ 'KURSF'
               OR fieldname EQ 'VLR_REAL'
               OR fieldname EQ 'PERC_ADIANT'
            OR fieldname EQ 'NR_PROVIS_INV'.

            IF w_fieldcatalog-fieldname EQ 'KURSF'
              OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.
              w_fieldcatalog-edit = space.
              w_fieldcatalog-no_out = c_x.
            ELSE.
              w_fieldcatalog-edit = c_x.
            ENDIF.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.
          ENDLOOP.

          CALL METHOD grid2->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = t_fieldcatalog.
          LOOP AT tg_pgt_ant.
            REFRESH: style.
            CLEAR: wa_style.
            REFRESH: tg_pgt_ant-style.
            IF tg_pgt_ant-adiant IS INITIAL.
              wa_style-fieldname = 'POSNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'DMBTR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'KURSF'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'PERC_ADIANT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'NR_PROVIS_INV'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

            ELSE.
              wa_style-fieldname = 'POSNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'DMBTR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'KURSF'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'PERC_ADIANT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'NR_PROVIS_INV'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

            ENDIF.
            INSERT LINES OF style INTO TABLE tg_pgt_ant-style.
            MODIFY tg_pgt_ant.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wg_acao NE c_modif_c_ov OR wg_header-inco1 = 'CIF' OR wg_header-inco1 = 'CPT'. "ALRS
      IF grid2 IS NOT INITIAL.
        CALL METHOD grid2->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.

        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname EQ 'VALDT'
             OR fieldname EQ 'POSNR'
             OR fieldname EQ 'DMBTR'
             OR fieldname EQ 'KURSF'
             OR fieldname EQ 'VLR_REAL'
*             OR FIELDNAME EQ 'PERC_ADIANT'
*             OR FIELDNAME EQ 'NR_PROVIS_INV'
             OR fieldname EQ 'DT_EMISSAOPI'
             OR fieldname EQ 'DT_RETORNOPI'.

          IF ( wg_cond_pgt-pgto_ant EQ c_x
          OR  wg_cond_pgt-pgto_ant EQ c_n )
         AND wg_header-param_espec EQ c_p.

            IF w_fieldcatalog-fieldname EQ 'KURSF'
            OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.

              w_fieldcatalog-no_out = space.

              IF w_fieldcatalog-fieldname EQ 'KURSF'.
                w_fieldcatalog-edit = c_x.
              ELSE.
                w_fieldcatalog-edit = space.
              ENDIF.

            ELSE.

              w_fieldcatalog-no_out = space.
              w_fieldcatalog-edit = c_x.

            ENDIF.

          ELSE.

            IF w_fieldcatalog-fieldname EQ 'KURSF'
             OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.
              w_fieldcatalog-edit = space.
              w_fieldcatalog-no_out = c_x.
            ELSE.
              w_fieldcatalog-edit = c_x.
            ENDIF.
          ENDIF.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.

        CALL METHOD grid2->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.

        IF wg_header-param_espec EQ c_p
        OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
        OR wg_header-param_espec EQ c_ax
          OR wg_header-param_espec EQ c_z. " CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
          LOOP AT tg_pgt_ant.
            REFRESH: style.
            CLEAR: wa_style.
            REFRESH: tg_pgt_ant-style.
            IF tg_pgt_ant-adiant IS INITIAL.
              wa_style-fieldname = 'POSNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'DMBTR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'KURSF'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
                 wg_header-param_espec EQ c_ax OR
                wg_header-param_espec EQ c_z. " CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
                wa_style-fieldname = 'PERC_ADIANT'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .

                wa_style-fieldname = 'NR_PROVIS_INV'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .

              ENDIF.

            ELSE.
              wa_style-fieldname = 'POSNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'DMBTR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'KURSF'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
                 wg_header-param_espec EQ c_ax OR
                 wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
                wa_style-fieldname = 'PERC_ADIANT'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

                wa_style-fieldname = 'NR_PROVIS_INV'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

              ENDIF.

            ENDIF.
            INSERT LINES OF style INTO TABLE tg_pgt_ant-style.
            MODIFY tg_pgt_ant.
          ENDLOOP.
        ENDIF.
      ENDIF.

      IF grid3 IS NOT INITIAL.

        CALL METHOD grid3->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.

*        LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*          W_FIELDCATALOG-EDIT = C_X.
*          MODIFY T_FIELDCATALOG FROM W_FIELDCATALOG.
*        ENDLOOP.

        LOOP AT t_fieldcatalog INTO w_fieldcatalog.
          CASE w_fieldcatalog-fieldname.
            WHEN: 'VALDT_HEDGE'.
              IF ( wg_header-inco1 EQ 'CIF' ) AND ( wg_header-risco_sacado EQ 'N' ).
                w_fieldcatalog-no_out = c_x.
                w_fieldcatalog-edit = space.
              ELSEIF ( wg_header-inco1 EQ 'CIF' ) AND ( wg_header-risco_sacado EQ 'S' ).
                w_fieldcatalog-no_out = space.
                w_fieldcatalog-edit = c_x.
              ELSEIF ( wg_header-inco1 EQ 'FOB' ) AND ( wg_header-risco_sacado EQ 'N' ).
                w_fieldcatalog-no_out = c_x.
                w_fieldcatalog-edit = space.
              ELSEIF ( wg_header-inco1 EQ 'FOB' ) AND ( wg_header-risco_sacado EQ 'S' ).
                w_fieldcatalog-no_out = space.
                w_fieldcatalog-edit = c_x.
              ELSEIF (  wg_header-inco1 EQ 'CPT' ) AND ( wg_header-risco_sacado EQ 'N' ).
                w_fieldcatalog-no_out = c_x.
                w_fieldcatalog-edit = space.
              ELSEIF ( wg_header-inco1 EQ 'CPT' ) AND (  wg_header-risco_sacado EQ 'S' ).
                w_fieldcatalog-no_out = space.
                w_fieldcatalog-edit = c_x.
              ENDIF.
            WHEN: 'FIXACAO'.
              IF ( wg_header-param_espec NE 'M'  OR wg_header-param_espec EQ 'Z').
                w_fieldcatalog-no_out = c_x.
                w_fieldcatalog-edit = space.
              ELSE.
                w_fieldcatalog-no_out = space.
                w_fieldcatalog-edit = c_x.
              ENDIF.
            WHEN OTHERS.
              w_fieldcatalog-edit = c_x.
          ENDCASE.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.

        "Bloquea as linhas da aba logistica as fixações que já estão liberadas na aba produto quantidade.
        CASE wg_header-param_espec.
          WHEN: 'M' OR 'Z'. "Frame
            LOOP AT tg_itens WHERE status_itm EQ 'F'.
              LOOP AT tg_logistica WHERE fixacao EQ tg_itens-fixacao.

                REFRESH: style.
                CLEAR: wa_style.
                REFRESH: tg_logistica-style.

                wa_style-fieldname = 'FIXACAO'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

                wa_style-fieldname = 'DATA_PROGR'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

                wa_style-fieldname = 'CADENCIA_QTE'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

                wa_style-fieldname = 'ZIEME'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

                wa_style-fieldname = 'VALDT_HEDGE'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

                INSERT LINES OF style INTO TABLE tg_logistica-style.
                MODIFY tg_logistica.

              ENDLOOP.
            ENDLOOP.
        ENDCASE.


        CALL METHOD grid3->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.


      IF grid5 IS NOT INITIAL.
        CALL METHOD grid5->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname NE 'NAME1'
            AND fieldname NE 'ADIANT'
            AND fieldname NE 'AUGBL'
            AND fieldname NE 'AUGDT'.

          IF ( wg_cond_pgt-pgto_ant EQ c_x
          OR  wg_cond_pgt-pgto_ant EQ c_n )
          AND wg_header-param_espec EQ c_p.
            IF w_fieldcatalog-fieldname EQ 'BANKS'
            OR w_fieldcatalog-fieldname EQ 'BANKL'
            OR w_fieldcatalog-fieldname EQ 'BANKN'
            OR w_fieldcatalog-fieldname EQ 'SWIFT'.
              w_fieldcatalog-no_out = c_x.
              w_fieldcatalog-edit = space.
            ELSEIF w_fieldcatalog-fieldname EQ 'KURSF'
                OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.
              w_fieldcatalog-no_out = space.
              IF w_fieldcatalog-fieldname EQ 'KURSF'.
                w_fieldcatalog-edit = c_x.
              ELSE.
                w_fieldcatalog-edit = space.
              ENDIF.
            ELSE.
              w_fieldcatalog-no_out = space.
              w_fieldcatalog-edit = c_x.
            ENDIF.
          ELSE.
            IF w_fieldcatalog-fieldname EQ 'KURSF'
            OR w_fieldcatalog-fieldname EQ 'VLR_REAL'.

              w_fieldcatalog-no_out = c_x.
              w_fieldcatalog-edit = space.
            ELSE.
              w_fieldcatalog-no_out = space.
              w_fieldcatalog-edit = c_x.
            ENDIF.
          ENDIF.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
        REFRESH: style.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
           WHERE edit  IS NOT INITIAL.

          wa_style-fieldname = w_fieldcatalog-fieldname.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDLOOP.
        LOOP AT tg_adto_ext.
          REFRESH: tg_adto_ext-style.
          IF tg_adto_ext-adiant IS NOT INITIAL.
            INSERT LINES OF style INTO TABLE tg_adto_ext-style.
          ENDIF.
          MODIFY tg_adto_ext TRANSPORTING style.
        ENDLOOP.
        CALL METHOD grid5->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.

      IF grid6 IS NOT INITIAL.
        CALL METHOD grid6->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.

        LOOP AT t_fieldcatalog ASSIGNING <fcat>.
* Somente Leitura conforme chamado CS2017001253
*          IF WG_HEADER-PARAM_ESPEC EQ C_A.
*            <FCAT>-EDIT = ABAP_FALSE.
*          ELSE.
*          CASE <FCAT>-FIELDNAME.
*            WHEN 'ICON' OR 'DESC_MAT' OR 'CONTRATO'.
*              <FCAT>-EDIT = ''.
*            WHEN OTHERS.
*              <FCAT>-EDIT = ABAP_TRUE.
*          ENDCASE.
*          ENDIF.
        ENDLOOP.

        CALL METHOD grid6->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.


      IF grid7 IS NOT INITIAL.
        CALL METHOD grid7->get_frontend_fieldcatalog
          IMPORTING
            et_fieldcatalog = t_fieldcatalog.
        LOOP AT t_fieldcatalog ASSIGNING <fcat>
           WHERE fieldname NE 'STATUS'
             AND fieldname NE 'ZSEQ_INST'
             AND fieldname NE 'POSNR'
             AND fieldname NE 'MAKTX'
             AND fieldname NE 'USD_TO'
             AND fieldname NE 'VLR_TOT_FRM_USD'
             AND fieldname NE 'VLRTOT'
             AND fieldname NE 'VBELN' .
* Somente Leitura conforme chamado CS2017001253
*          IF WG_HEADER-PARAM_ESPEC EQ C_A.
*            <FCAT>-EDIT = ABAP_FALSE.
*          ELSE.
*          <FCAT>-EDIT = ABAP_TRUE.
*          ENDIF.
        ENDLOOP.
        CALL METHOD grid7->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = t_fieldcatalog.
      ENDIF.
    ENDIF.

***WSB-WSB
*    IF SY-UCOMM EQ 'COND_PG'.
*      IF OBG_DIALOGBOX IS NOT INITIAL.
*        REFRESH STYLE.
*        CLEAR: WA_STYLE.
*        LOOP AT TG_ITENS.
*          IF TG_ITENS-STATUS_ITM EQ C_F.
*            LOOP AT TG_COND_ESP.
*              IF TG_ITENS-FIXACAO EQ TG_COND_ESP-FIXACAO.
*
*                WA_STYLE-FIELDNAME = 'FIXACAO'.
*                WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                INSERT  WA_STYLE INTO TABLE STYLE .
*
*                WA_STYLE-FIELDNAME = 'ZTERM'.
*                WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                INSERT  WA_STYLE INTO TABLE STYLE .
*
*                WA_STYLE-FIELDNAME = 'QTE_VENC'.
*                WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*                INSERT  WA_STYLE INTO TABLE STYLE .
*
*                INSERT LINES OF STYLE INTO TABLE WL_COND_ESP-STYLE.
**            DELETE TG_COND_ESP WHERE FIXACAO EQ TG_ITENS-FIXACAO.
*                MODIFY TG_COND_ESP TRANSPORTING STYLE WHERE FIXACAO EQ TG_ITENS-FIXACAO.
*
*                CLEAR WA_STYLE.
*              ENDIF.
*
*            ENDLOOP.
*
*          ENDIF.
*        ENDLOOP.
*
*        CALL METHOD GRID9->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.
*      ENDIF.
*
*    ENDIF.


  ELSEIF wg_acao EQ c_modif_qtd.

    MOVE: c_boleto TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_dele TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_atual TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_modif TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_add TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_liberar TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

*     IF WG_HEADER-PARAM_ESPEC EQ C_P.
    MOVE: c_sol_aprov TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

    MOVE: c_reprov TO tl_ucomm.

    APPEND tl_ucomm.
    CLEAR: tl_ucomm.

*    Excluido o Redistribuição quando for Modif_QTD
    IF wg_redistribuir IS INITIAL.
      MOVE: c_redist TO tl_ucomm.
      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ELSE.
      MOVE: c_modif_qtd TO tl_ucomm.
      APPEND tl_ucomm.
      CLEAR: tl_ucomm.
    ENDIF.

*    ENDIF.

    IF grid1 IS NOT INITIAL.

      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.

        CASE w_fieldcatalog-fieldname.
          WHEN: 'KUNNR'.
            IF wg_redistribuir EQ c_x.
              w_fieldcatalog-no_out = space.
            ELSE.
              w_fieldcatalog-no_out = c_x.
            ENDIF.
          WHEN: 'FIXACAO'.
            CASE wg_header-param_espec.
              WHEN: c_m OR c_z.
                w_fieldcatalog-no_out = space.
              WHEN OTHERS.
                w_fieldcatalog-no_out = c_x.
            ENDCASE.
          WHEN: 'KURSF' OR 'VOLUM' OR 'VOLEH' OR 'BRGEW'.
            CASE wg_header-param_espec.
              WHEN: c_f.
                w_fieldcatalog-no_out = space.
              WHEN OTHERS.
                w_fieldcatalog-no_out = c_x.
            ENDCASE.
        ENDCASE.

        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING no_out.
      ENDLOOP.

      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.


    LOOP AT tg_itens.

      REFRESH: style.
      CLEAR: wa_style.
      REFRESH: tg_itens-style.
      tabix_r = sy-tabix.

      CASE wg_header-param_espec. "Venda Performance
        WHEN: 'P'.

          wa_style-fieldname = 'MATNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'WERKS'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'PONTO_C'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'TERMINAL'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'LGORT'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'CHARG'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'ZIEME'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'P_PORTO'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'PMEIN'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'VLT_PORTO'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          IF tg_itens-valdt IS NOT INITIAL.
            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            INSERT  wa_style INTO TABLE style .
          ELSE.
            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .
          ENDIF.

          wa_style-fieldname = 'NAVIO'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'PORTO'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

*-CS2021000615 - 17.06.2021 - JT - inicio
        WHEN: 'A' OR 'X'.
*-CS2021000615 - 17.06.2021 - JT - fim
          IF ( tg_itens-vbeln IS NOT INITIAL ).

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'WERKS'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PONTO_C'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .


            wa_style-fieldname = 'TERMINAL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LGORT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'CHARG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'INSTRUCAO'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            IF tg_itens-valdt IS NOT INITIAL.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .
            ELSE.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
            ENDIF.

          ELSEIF ( tg_itens-vbeln IS INITIAL ).

            IF tg_itens-item_edit EQ c_r.

              wa_style-fieldname = 'MATNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .

            ELSE.
              wa_style-fieldname = 'MATNR'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .

              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
            ENDIF.

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

          ENDIF.
        WHEN: 'M' OR 'Z'.

          IF ( tg_itens-vbeln IS NOT INITIAL ) AND ( tg_itens-status_itm EQ 'F' ) AND ( tg_itens-item_edit EQ 'X' ).

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'FIXACAO'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'WERKS'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PONTO_C'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'TERMINAL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LGORT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'CHARG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            IF ( tg_itens-doc_precedente IS NOT INITIAL ).
              wa_style-fieldname = 'ZMENG'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
            ENDIF.

          ELSEIF ( tg_itens-item_edit EQ 'R' ).

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .


            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .


*            WA_STYLE-FIELDNAME = 'VALDT'.
*            WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*            INSERT  WA_STYLE INTO TABLE STYLE .

          ENDIF.

          READ TABLE tg_cond_esp WITH KEY fixacao = tg_itens-fixacao.
          READ TABLE tl_t052 WITH KEY zterm = tg_cond_esp-zterm.

          IF ( tl_t052-ztag1 IS NOT INITIAL ) OR
             ( tg_itens-status_itm EQ c_f AND tg_itens-item_edit NE c_r ) OR
             ( tg_itens-vbeln IS NOT INITIAL ).

            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style.
            IF tl_t052-ztag1 IS NOT INITIAL.
              CLEAR: tg_itens-valdt.
            ENDIF.
          ELSE.
            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            INSERT  wa_style INTO TABLE style .
          ENDIF.

*      INICIO MODIFICAÇÃO WELGEM
*      BLOCK CAMPOS REDISTRIBUIÇÃO.
          IF wg_redistribuir EQ c_x.

            IF tg_itens-item_edit NE 'R'.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
              MODIFY TABLE style FROM wa_style TRANSPORTING style.
            ENDIF.

            LOOP AT t_fieldcatalog INTO w_fieldcatalog.
              wa_style-fieldname = w_fieldcatalog-fieldname.
*      BLOQUEIA TODOS OS CAMPOS DIFERENTE DE ZMENG COM STATUS EQ F
              IF wa_style-fieldname = 'ZMENG' AND
                tg_itens-vbeln IS NOT INITIAL AND
                tg_itens-status_itm EQ c_f AND
                ( tg_itens-status NE c_y OR
                tg_itens-status NE c_w ).
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              ELSE.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              ENDIF.

              IF ( tg_itens-item_edit EQ 'R'  ) AND
                 ( ( tg_itens-status    NE c_y  ) OR
                 ( tg_itens-status    NE c_w ) ) AND
                 ( wa_style-fieldname = 'FIXACAO' OR
                   wa_style-fieldname = 'WERKS'   OR
                   wa_style-fieldname = 'PONTO_C' OR
                   wa_style-fieldname = 'TERMINAL'OR
                   wa_style-fieldname = 'LGORT'   OR
                   wa_style-fieldname = 'CHARG'   OR
                   wa_style-fieldname = 'KUNNR' ) .

                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.

              ENDIF.
              INSERT  wa_style INTO TABLE style.
            ENDLOOP.

*      TRAVA O CAMPO QUANDO A FIXAÇÃO NÃO ESTIVER PREENCHIDA.
*      BLOCK ZMENG QUANDO FIXAÇÃO ESTIVER LIMPA.
*      TRAVA O CAMPO FIXAÇÃO QUANDO INSERIR INFORMAÇÃO NO ZMENG

            block_r = tg_itens-fixacao.
            IF ( wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z ).
              wa_style-fieldname = 'ZMENG'.
              IF ( tg_itens-fixacao   IS NOT INITIAL  ) AND
                 (  tg_itens-item_edit EQ 'R'         ) OR
                 (  tg_itens-vbeln     IS NOT INITIAL ).
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              ELSE.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              ENDIF.

              INSERT  wa_style INTO TABLE style.
              MODIFY TABLE style FROM wa_style TRANSPORTING style.

              IF ( ( tg_itens-zmeng NE 0 ) AND ( tg_itens-item_edit EQ 'R' ) ) .

                wa_style-fieldname = 'FIXACAO'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .
                MODIFY TABLE style FROM wa_style TRANSPORTING style.

                READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY fixacao = block_r
                                                                    status_itm = c_f.
                IF sy-subrc IS INITIAL.
                  tg_itens-status_itm = c_f.
                  MODIFY tg_itens INDEX tabix_r.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.

*     BLOCK CAMPOS BOTÃO MODIFICAÇÃO QUANTIDADE
          IF wg_acao EQ c_modif_qtd AND var_modred IS NOT INITIAL AND wg_redistribuir NE c_x.
            LOOP AT t_fieldcatalog INTO w_fieldcatalog.
              wa_style-fieldname = w_fieldcatalog-fieldname.
              IF ( wa_style-fieldname EQ 'ZMENG' ) AND ( tg_itens-vbeln IS NOT INITIAL ) AND ( ( tg_itens-status NE c_y ) OR ( tg_itens-status NE c_w ) ) .
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              ELSE.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              ENDIF.
              INSERT  wa_style INTO TABLE style.
            ENDLOOP.

            wa_style-fieldname = 'VALDT'.
            IF ( tg_itens-vbeln IS NOT INITIAL ) AND ( tg_itens-valdt IS NOT INITIAL ).
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            ELSE.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            ENDIF.
            INSERT  wa_style INTO TABLE style.
            MODIFY TABLE style FROM wa_style TRANSPORTING style.
          ENDIF.

*Bloqueia todos os Itens com Status Y W E
          IF tg_itens-status EQ c_y OR tg_itens-status EQ c_w OR tg_itens-status EQ c_e.
            REFRESH style.
            LOOP AT t_fieldcatalog INTO w_fieldcatalog.
              wa_style-fieldname = w_fieldcatalog-fieldname.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style.
            ENDLOOP.
          ENDIF.

*Fim parametro especial"M"

        WHEN OTHERS.

          IF ( tg_itens-vbeln IS NOT INITIAL ) AND ( tg_itens-doc_precedente IS NOT INITIAL ).

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'FIXACAO'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'WERKS'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PONTO_C'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'TERMINAL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LGORT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'CHARG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZMENG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'BRGEW'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'KURSK'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'VOLUM'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'VOLEH'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

          ELSEIF ( tg_itens-vbeln IS NOT INITIAL ).

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'WERKS'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PONTO_C'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'TERMINAL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LGORT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'CHARG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZMENG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'BRGEW'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'KURSK'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            IF tg_itens-valdt IS NOT INITIAL.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .
            ELSE.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
            ENDIF.

          ELSEIF  var_modred IS NOT INITIAL AND tg_itens-vbeln IS INITIAL AND tg_itens-item_edit NE c_r.

            wa_style-fieldname = 'MATNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'FIXACAO'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'WERKS'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PONTO_C'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'TERMINAL'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LIFNR'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'LGORT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'CHARG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZMENG'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'BRGEW'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'ZIEME'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'PMEIN'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'KURSK'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'VOLUM'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'VOLEH'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .

            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
            INSERT  wa_style INTO TABLE style .


          ELSE.
            CASE tg_itens-item_edit.
              WHEN: c_r.

                wa_style-fieldname = 'MATNR'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .
                wa_style-fieldname = 'ZMENG'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .
                wa_style-fieldname = 'WERKS'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .
                wa_style-fieldname = 'LGORT'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .
                wa_style-fieldname = 'CHARG'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .
                wa_style-fieldname = 'KUNNR'.
                wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                INSERT  wa_style INTO TABLE style .


                SELECT SINGLE *
                    FROM t052
                    INTO wa_t052
                  WHERE zterm EQ wg_cond_pgt-zterm.

                IF wa_t052-ztag1 IS NOT INITIAL.
                  wa_style-fieldname = 'VALDT'.
                  wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                  INSERT  wa_style INTO TABLE style .
                  CLEAR: tg_itens-valdt.
                ELSE.
                  wa_style-fieldname = 'VALDT'.
                  wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
                  INSERT  wa_style INTO TABLE style .
                ENDIF.


                wa_style-fieldname = 'ZIEME'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .
                wa_style-fieldname = 'PMEIN'.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
                INSERT  wa_style INTO TABLE style .

            ENDCASE.

          ENDIF.

          SELECT SINGLE *
            FROM vbfa
            INTO wl_vbfa
              WHERE vbelv   EQ tg_itens-vbeln
                AND vbtyp_n EQ 'J'
                AND vbtyp_v EQ 'C'.

          IF sy-subrc IS INITIAL.
            SELECT SINGLE *
              FROM vbfa
              INTO wl_vbfa
               WHERE vbelv   EQ wl_vbfa-vbeln
                 AND vbtyp_n EQ 'M'
                 AND vbtyp_v EQ 'J'.
*              BREAK-POINT.
*              IF SY-SUBRC IS NOT INITIAL OR TG_ITENS-STATUS_ITM EQ C_F OR TG_ITENS-VBELN IS NOT INITIAL.
            IF sy-subrc IS NOT INITIAL.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
              INSERT  wa_style INTO TABLE style .
            ELSE.
              wa_style-fieldname = 'VALDT'.
              wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
              INSERT  wa_style INTO TABLE style .
            ENDIF.
          ELSE.
            wa_style-fieldname = 'VALDT'.
            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
            INSERT  wa_style INTO TABLE style .
          ENDIF.

      ENDCASE.

*Bloqueia todos os Itens com Status Y W E
      IF tg_itens-status EQ c_y OR tg_itens-status EQ c_w OR tg_itens-status EQ c_e.
        REFRESH style.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog.
          wa_style-fieldname = w_fieldcatalog-fieldname.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style.
        ENDLOOP.
      ENDIF.

*      FIM MODICAÇÃO WELGEM

      INSERT LINES OF style INTO TABLE tg_itens-style.
      MODIFY tg_itens.
    ENDLOOP.

    IF grid7 IS NOT INITIAL.
      CALL METHOD grid7->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

** Criado uma novo cadastro da instrucao e formacao de lote
**      LOOP AT TG_FORM_LOTE.
**        REFRESH: STYLE.
**        CLEAR: WA_STYLE.
**        REFRESH: TG_FORM_LOTE-STYLE.
**        IF TG_FORM_LOTE-VBELN IS NOT INITIAL.
**          WA_STYLE-FIELDNAME = 'ZMENG'.
**          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**          INSERT  WA_STYLE INTO TABLE STYLE .
**
**          WA_STYLE-FIELDNAME = 'VOLUM'.
**          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**          INSERT  WA_STYLE INTO TABLE STYLE .
**
**          SELECT SINGLE *
**            FROM VBFA
**            INTO WL_VBFA
**              WHERE VBELV   EQ TG_FORM_LOTE-VBELN
**                AND VBTYP_N EQ 'J'
**                AND VBTYP_V EQ 'C'.
**
**          IF SY-SUBRC IS INITIAL.
**            SELECT SINGLE *
**              FROM VBFA
**              INTO WL_VBFA
**               WHERE VBELV   EQ WL_VBFA-VBELN
**                 AND VBTYP_N EQ 'M'
**                 AND VBTYP_V EQ 'J'.
**            IF SY-SUBRC IS NOT INITIAL.
**              WA_STYLE-FIELDNAME = 'VALDT'.
**              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
**              INSERT  WA_STYLE INTO TABLE STYLE .
**            ELSE.
**              WA_STYLE-FIELDNAME = 'VALDT'.
**              WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
**              INSERT  WA_STYLE INTO TABLE STYLE .
**            ENDIF.
**          ENDIF.

*      ELSE.
*        IF TG_ITENS-ITEM_EDIT EQ C_R.
*          WA_STYLE-FIELDNAME = 'ZMENG'.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          INSERT  WA_STYLE INTO TABLE STYLE .
*          WA_STYLE-FIELDNAME = 'WERKS'.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          INSERT  WA_STYLE INTO TABLE STYLE .
*          WA_STYLE-FIELDNAME = 'LGORT'.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          INSERT  WA_STYLE INTO TABLE STYLE .
*          WA_STYLE-FIELDNAME = 'CHARG'.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          INSERT  WA_STYLE INTO TABLE STYLE .
*          WA_STYLE-FIELDNAME = 'VALDT'.
*          WA_STYLE-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          INSERT  WA_STYLE INTO TABLE STYLE .
*
*        ENDIF.
** Criado uma novo cadastro da instrucao e formacao de lote
**        ENDIF.
**
**        INSERT LINES OF STYLE INTO TABLE TG_FORM_LOTE-STYLE.
**        MODIFY TG_FORM_LOTE.
**      ENDLOOP.

    ENDIF.

    IF grid2 IS NOT INITIAL.
      CALL METHOD grid2->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname = 'POSNR'
            OR fieldname = 'VALDT'
            OR fieldname = 'DMBTR'
            OR fieldname = 'KURSF'.

        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
      CALL METHOD grid2->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      LOOP AT tg_pgt_ant.
        REFRESH: style.
        CLEAR: wa_style.
        REFRESH: tg_pgt_ant-style.
        IF tg_pgt_ant-adiant IS INITIAL.
          wa_style-fieldname = 'POSNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'VALDT'.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'DMBTR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'KURSF'.
          wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
          INSERT  wa_style INTO TABLE style .

        ELSE.
          wa_style-fieldname = 'POSNR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'VALDT'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'DMBTR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'KURSF'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .
        ENDIF.
        INSERT LINES OF style INTO TABLE tg_pgt_ant-style.
        MODIFY tg_pgt_ant.
      ENDLOOP.
    ENDIF.

    IF grid3 IS NOT INITIAL.
      CALL METHOD grid3->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      LOOP AT t_fieldcatalog INTO w_fieldcatalog
         WHERE fieldname = 'CADENCIA_QTE'.

        w_fieldcatalog-edit = c_x.
        MODIFY t_fieldcatalog FROM w_fieldcatalog.
      ENDLOOP.
      CALL METHOD grid3->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      LOOP AT tg_logistica.

        REFRESH: style.
        CLEAR: wa_style.
        REFRESH: tg_logistica-style.

        IF ( tg_logistica-status EQ c_y ) OR ( tg_logistica-status EQ c_w ).
          wa_style-fieldname = 'FIXACAO'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'DATA_PROGR'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'ZMENG'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'ZIEME'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'VALDT_HEDGE'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'CADENCIA_QTE'.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT  wa_style INTO TABLE style .

          INSERT LINES OF style INTO TABLE tg_logistica-style.
          MODIFY tg_logistica.

        ENDIF.
      ENDLOOP.
    ENDIF.

    IF grid4 IS NOT INITIAL.
      CALL METHOD grid4->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.
      IF ( wg_header-param_espec NE c_m OR wg_header-param_espec NE c_z ).
        LOOP AT t_fieldcatalog INTO w_fieldcatalog
          WHERE fieldname EQ 'FORMULA2'.
          w_fieldcatalog-edit = space.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.
      ELSE.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog.
          READ TABLE tg_fields TRANSPORTING NO FIELDS
          WITH KEY field = w_fieldcatalog-fieldname.
          IF sy-subrc IS INITIAL
          OR w_fieldcatalog-fieldname EQ 'WAERS'
          OR w_fieldcatalog-fieldname EQ 'CBOT'
          OR w_fieldcatalog-fieldname EQ 'MONAT'
          OR w_fieldcatalog-fieldname EQ 'VALDT'
          OR w_fieldcatalog-fieldname EQ 'SAFRA'. " RJF
*

            w_fieldcatalog-edit = c_x.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.
          ENDIF.
        ENDLOOP.
      ENDIF.

      CALL METHOD grid4->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.

      LOOP AT tg_preco.
        REFRESH: style, tg_preco-style.
        IF tg_preco-tipo_calc EQ c_c.
*          CLEAR: TG_PRECO-NIVEL, TG_PRECO-COD_FP.
        ENDIF.
        wa_style-fieldname = 'BEZEI'.
        IF tg_preco-tipo_calc NE c_c
        AND tg_preco-tipo_calc NE c_r.
          wa_style-style = alv_style_font_bold_no.
        ELSE.
          wa_style-style =  alv_style_font_bold.
          wa_style-style2 =  alv_style2_no_border_left.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'FORMULA2'.
        IF tg_preco-tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
        ELSE.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold.
        ENDIF.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'WAERS'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        wa_style-fieldname = 'CBOT'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
        INSERT LINES OF style INTO TABLE tg_preco-style.

        IF tg_preco-tipo_calc EQ 'C'
        OR tg_preco-tipo_calc EQ c_r.
          MOVE: 'C305' TO tg_preco-line_color.
        ELSE.
          CLEAR tg_preco-line_color.
        ENDIF.
        MODIFY tg_preco.
      ENDLOOP.

      IF wg_redistribuir IS NOT INITIAL.

        LOOP AT <fs_table> ASSIGNING <fs_line>.

          REFRESH: style. "TG_PRECO-STYLE.
          CLEAR: wg_valor, wl_tipo_calc.
          ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
          <fs_campo> = style[].

          IF ( wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z ) .
            PERFORM get_set_valores
                       USING
                          'POSNR'
                          'G'
                       CHANGING
                           wg_valor.

*            READ TABLE TG_ITENS TRANSPORTING NO FIELDS
*              WITH KEY FIXACAO = WG_VALOR
*                       ITEM_EDIT = C_R.
*            IF SY-SUBRC IS INITIAL.
*              WL_STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*
*            ELSE.
*              WL_STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*            ENDIF.
*          ELSE.
*            WL_STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
*          ENDIF.

*   VERIFICA SE A FIXAÇÃO ESTA FINALIZADA E DESABILITA O CAMPO PREÇO
            READ TABLE tg_itens TRANSPORTING NO FIELDS
              WITH KEY fixacao = wg_valor
                       status_itm = c_f.

            IF sy-subrc IS INITIAL.
              wl_style = cl_gui_alv_grid=>mc_style_disabled.
            ELSE.
              wl_style = cl_gui_alv_grid=>mc_style_enabled.
            ENDIF.

          ELSE.
            wl_style = cl_gui_alv_grid=>mc_style_enabled.
          ENDIF.


          PERFORM get_set_valores
                      USING
                         'TIPO_CALC'
                         'G'
                      CHANGING
                          wl_tipo_calc.
          LOOP AT tg_fields.
            wa_style-fieldname = tg_fields-field. "'FORMULA2'.
            CLEAR wg_valor.
            PERFORM get_set_valores
                      USING
                         'COD_FP'
                         'G'
                      CHANGING
                          wg_valor.

            READ TABLE tg_preco_n
              WITH KEY field = tg_fields-field
                       cod_fp = wg_valor.

            IF wl_tipo_calc NE c_c
            AND wl_tipo_calc NE c_f
            AND wl_tipo_calc NE c_r
            AND sy-subrc IS INITIAL.
              wa_style-style = wl_style + alv_style_font_bold_no.
            ELSE.
*          CLEAR: TG_PRECO-NIVEL, TG_PRECO-COD_FP.
              IF wl_tipo_calc EQ c_c
              OR wl_tipo_calc EQ c_r.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold.
              ELSEIF wl_tipo_calc EQ c_f.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
              ELSE.
                wa_style-style = cl_gui_alv_grid=>mc_style_disabled + alv_style_font_bold_no.
              ENDIF.
*          WA_STYLE-STYLE2 =  ALV_STYLE2_NO_BORDER_LEFT.
            ENDIF.
            INSERT  wa_style INTO TABLE style .
          ENDLOOP.
          wa_style-fieldname = 'BEZEI'.
          IF wl_tipo_calc NE c_c
          AND wl_tipo_calc NE c_r.
            wa_style-style = alv_style_font_bold_no.
          ELSE.
            wa_style-style =  alv_style_font_bold.
            wa_style-style2 =  alv_style2_no_border_left.
          ENDIF.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'WAERS'.
          IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.
            wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.
          INSERT  wa_style INTO TABLE style .
          wa_style-fieldname = 'CBOT'.
          IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r.
            wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.
          INSERT  wa_style INTO TABLE style .
          wa_style-fieldname = 'VALDT'.
          IF wl_tipo_calc NE c_c
          AND wl_tipo_calc NE c_r
          AND wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z .
            wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.
          INSERT  wa_style INTO TABLE style .

          wa_style-fieldname = 'SAFRA'. "RJF
          IF wl_tipo_calc NE c_c
          AND wl_tipo_calc NE c_r
          AND wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z .
            wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.
          INSERT  wa_style INTO TABLE style .

*          wa_style-fieldname = 'ZMENG'.
*          IF tg_preco-tipo_calc NE c_c
*            AND wg_header-param_espec EQ c_m.
*            wa_style-style = cl_gui_alv_grid=>mc_style_enabled.
*          ELSE.
*            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
*          ENDIF.
*          INSERT  wa_style INTO TABLE style .
          wa_style-fieldname = 'MONAT'.
          IF wl_tipo_calc NE c_c
        AND wl_tipo_calc NE c_r
            AND ( wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z ).
            wa_style-style = wl_style. "CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
          ELSE.
            wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          ENDIF.
          INSERT  wa_style INTO TABLE style .
*          INSERT LINES OF style INTO TABLE tg_preco-style.
          ASSIGN COMPONENT 'STYLE' OF STRUCTURE <fs_line> TO <fs_campo>.
          <fs_campo> = style[].
          IF wl_tipo_calc EQ 'C'
          OR wl_tipo_calc EQ c_r.
            wg_valor = 'C305'.
*          MOVE: 'C305' TO TG_PRECO-LINE_COLOR.
            PERFORM get_set_valores
                     USING
                        'LINE_COLOR'
                        'S'
                     CHANGING
                        wg_valor.
          ELSE.
*          CLEAR TG_PRECO-LINE_COLOR.
            CLEAR: wg_valor.

            PERFORM get_set_valores
                     USING
                        'LINE_COLOR'
                        'S'
                     CHANGING
                        wg_valor.
          ENDIF.
*          MODIFY tg_preco.
        ENDLOOP.


* PRECO FRAME - INICIO
        IF grid8 IS NOT INITIAL.
          CALL METHOD grid8->get_frontend_fieldcatalog
            IMPORTING
              et_fieldcatalog = t_fieldcatalog.
          LOOP AT t_fieldcatalog INTO w_fieldcatalog
            WHERE fieldname EQ 'FORMULA2'
               OR fieldname EQ 'WAERS'
               OR fieldname EQ 'CBOT'
               OR fieldname EQ 'SAFRA'. "RJF

            w_fieldcatalog-edit = c_x.
            MODIFY t_fieldcatalog FROM w_fieldcatalog.
          ENDLOOP.
          CALL METHOD grid8->set_frontend_fieldcatalog
            EXPORTING
              it_fieldcatalog = t_fieldcatalog.
        ENDIF.
*      PRECO FRAME - FIM
      ENDIF.

    ENDIF.

    IF wg_redistribuir IS NOT INITIAL
    AND ( wg_header-param_espec EQ c_m OR  wg_header-param_espec EQ c_z ).

      LOOP AT tg_cond_esp.
        CLEAR: tg_cond_esp-qte_venc.
        LOOP AT tg_itens WHERE fixacao  EQ tg_cond_esp-fixacao.

          ADD 1 TO tg_cond_esp-qte_venc.
        ENDLOOP.
        MODIFY tg_cond_esp.
      ENDLOOP.
    ENDIF.
  ENDIF.

*  IF WG_HEADER-STATUS EQ C_L.
  LOOP AT tg_itens.
    IF tg_itens-status_itm EQ c_f.
      tg_itens-line_color =  'C510'.

    ELSE.
      tg_itens-line_color = space.
    ENDIF.

    MODIFY tg_itens.
  ENDLOOP.
*  ENDIF.
  IF wg_colaps EQ '@K1@'.
    wg_sub01 = c_0051.
  ELSE.
    wg_sub01 = c_0052.
  ENDIF.
  CALL METHOD cl_gui_cfw=>dispatch.

*  IF ( WG_HEADER-PARAM_ESPEC EQ SPACE
*     OR WG_HEADER-PARAM_ESPEC EQ C_A
*     OR WG_HEADER-PARAM_ESPEC EQ C_M )
*     AND ( WG_HEADER-AUART NE 'ZEXP'
*       AND WG_HEADER-AUART NE 'ZEXI' ).
*
*    SELECT *
*        FROM ZSDT0064
*        INTO TABLE TL_0064
*         WHERE UNAME EQ SY-UNAME
*           AND SOLIC EQ 'X'.
*
*    IF SY-SUBRC IS INITIAL.
*      SET PF-STATUS 'Z001' EXCLUDING TL_UCOMM  .
*    ELSE.
*      SET PF-STATUS 'Z002' EXCLUDING TL_UCOMM  .
*    ENDIF.
*  ELSE.

*-CS2023000189-#126959-07.11.2023-JT-inicio
  MOVE: c_trace_cotton TO tl_ucomm.
  APPEND tl_ucomm.
*-CS2023000189-#126959-07.11.2023-JT-fim

  SET PF-STATUS 'Z002' EXCLUDING tl_ucomm  .
*  ENDIF.


  SET TITLEBAR 'Z001'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0201 OUTPUT.
  DATA: tl_tvkbt TYPE TABLE OF tvkbt WITH HEADER LINE,
        values   TYPE vrm_values WITH HEADER LINE.

  SELECT *
        FROM zsdt0064
        INTO TABLE tl_0064
         WHERE uname EQ sy-uname
           AND solic EQ 'X'.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM tvkbt
      INTO TABLE tl_tvkbt
      FOR ALL ENTRIES IN tl_0064
       WHERE spras EQ sy-langu
         AND vkbur EQ tl_0064-werks.
  ENDIF.

  LOOP AT tl_0064.
    READ TABLE tl_tvkbt
      WITH KEY vkbur = tl_0064-werks.
    IF sy-subrc IS INITIAL.

      values-text = tl_tvkbt-bezei.
      values-key  = tl_0064-werks.
      APPEND values.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'WG_HEADER-VKBUR'
      values          = values[]
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  REFRESH: values.


  SET PF-STATUS 'Z003'.
  SET TITLEBAR 'Z03'.

ENDMODULE.                 " STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0201 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.
      enter.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'
      OR 'EXIT'.
      CLEAR: wg_header-vkbur, wg_desc_vkbur.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_0060 TYPE zsdt0060.
  DATA: tl_0061 TYPE TABLE OF zsdt0061 WITH HEADER LINE.
  DATA: wl_0053        TYPE zsdt0053,
        wl_0066        TYPE zsdt0066,
        wl_0054        TYPE zsdt0054,
        wl_0063        TYPE zsdt0063,
        wl_lines       TYPE sy-tabix,
        wl_posnr       TYPE zsdt0059-posnr,
        tl_good_cells  TYPE lvc_t_modi,
        ls_good        TYPE lvc_s_modi,
        var_valor_char TYPE c LENGTH 128,
        var_tabix      TYPE sy-tabix.

  DATA: wl_setleaf_aux TYPE setleaf.

  DATA: wl_0057 TYPE zsdt0057.
  DATA: wl_zsdt0075 TYPE zsdt0075.
  DATA: wl_code_cadastro TYPE sy-ucomm.


  DATA: lobj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.
  DATA: cx_exception TYPE REF TO zcx_webservice.
  DATA: var_msg   TYPE string.

  IF lcl_busca_tela=>exit_tela( sy-ucomm ) IS INITIAL.

    REFRESH tl_0061.
    CALL METHOD grid1->check_changed_data.
    CALL METHOD grid2->check_changed_data.
    CALL METHOD grid3->check_changed_data.
    CALL METHOD grid4->check_changed_data.
    CALL METHOD grid5->check_changed_data.
    CALL METHOD grid6->check_changed_data.
    CALL METHOD grid7->check_changed_data.
    IF grid8 IS NOT INITIAL.
      CALL METHOD grid8->check_changed_data.
    ENDIF.
    CALL METHOD grid10->check_changed_data.

  ENDIF.

*  "Atribuir o ultimo risco sacado.
* descomentado porque impactou na ação alterar quantodade
  wg_verifica_sacado   = wg_header-risco_sacado.
  wg_verifica_incoterm = wg_header-inco1.

  IF wg_acao EQ c_add
  OR wg_acao EQ c_modif
  OR wg_acao EQ c_modif_qtd.
    IF <fs_table> IS ASSIGNED
    AND <fs_line> IS ASSIGNED.
      UNASSIGN <fs_line>.
      CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
      ASSIGN t_new_line->* TO <fs_line>.
      LOOP AT <fs_table> INTO <fs_line>.
        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'POSNR'
                                      'G'
                              CHANGING wg_valor.
        CONDENSE wg_valor NO-GAPS.

        IF wg_valor NE wl_posnr
        OR sy-tabix EQ 1.

          wl_posnr = wg_valor.
          REFRESH: tl_good_cells.
          ls_good-row_id = sy-tabix.
          APPEND ls_good TO tl_good_cells.

          " 23.04.2025 - RAMON - Corrigir chamada da aba preço para preencher onde está vazio, com 0,00 -->
          " (estava chamando a todo momento, com isso deixava a transação muito lenta)
          IF sy-ucomm = 'SAVE' OR sy-ucomm = 'SHOW_MSGRE'.
            lcl_event_handler=>on_data_changed_finished_preco( et_good_cells = tl_good_cells ).
          ENDIF.
          " 23.04.2025--<
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.
  CASE sy-ucomm.
    WHEN c_chg_dtv.
**    VALIDA SE EXISTE DOCUMENTO PARA SER MODIFICADO.

      SELECT SINGLE nro_sol_ov
        FROM zsdt0051
        INTO wg_header-nro_sol_ov
         WHERE nro_sol_ov EQ wg_header-nro_sol_ov
           AND status     NE 'D'.

      IF sy-subrc IS INITIAL.

        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
          EXPORTING
            functioncode           = 'TAB_STRIP_FC3'
          EXCEPTIONS
            function_not_supported = 1.

        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.
        PERFORM monta_log.

        CALL FUNCTION 'ENQUEUE_EZSDT0051'
          EXPORTING
            nro_sol_ov     = wg_header-nro_sol_ov
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        wg_acao = c_chg_dtv.
        wg_prec = c_chg_dtv. " VARIAVEI PARA A ABA PREÇO

      ENDIF.

    WHEN c_atual_prec.

*-BUG 120423-10.08.2023-JT-inicio
    WHEN c_trace_cotton.
      PERFORM f_reenvia_trace.
*-BUG 120423-10.08.2023-JT-fim

    WHEN c_add.
      IF wg_flag IS INITIAL.
        PERFORM limpa_variavel USING sy-ucomm.
        PERFORM get_next_number  USING  'ZNR_SOL_OV'
                                        '01'
                                        space
                               CHANGING wg_header-nro_sol_ov.

        SELECT SINGLE *
          FROM zsdt0060
          INTO wl_0060
            WHERE usnam EQ sy-uname
             AND  programa EQ sy-cprog.

        IF sy-subrc IS INITIAL.
          MOVE: wl_0060-vkbur TO wg_header-vkbur,
                wl_0060-vkgrp TO wg_header-vkgrp,
                wl_0060-vkorg TO wg_header-vkorg,
                wl_0060-vtweg TO wg_header-vtweg,
                wl_0060-spart TO wg_header-spart,
                wl_0060-tp_venda TO wg_header-tp_venda,
                wl_0060-waerk TO wg_header-waerk,
                wl_0060-vkaus TO wg_header-vkaus,
                wl_0060-inco1 TO wg_header-inco1,
                wl_0060-inco2 TO wg_header-inco2,
*-IR065241 - 06.07.2021 - JT - inicio
                'N'           TO wg_header-risco_sacado.
*-IR065241 - 06.07.2021 - JT - fim

          IF wg_header-tp_venda IS NOT INITIAL.
            PERFORM refresh_preco.
          ENDIF.
        ENDIF.

        MOVE: sy-datum TO wg_header-data_venda,
              2        TO wg_header-tx_multa,
              1        TO wg_header-tx_juros.
*              sy-uzeit TO wg_header-erzet.  2% de Multa 1% de Juros;
      ENDIF.
      MOVE c_add TO wg_acao.
    WHEN c_atual.

      PERFORM limpa_variavel USING sy-ucomm.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.

      CASE wg_header-auart.
        WHEN 'ZRFL' OR 'ZUB' OR 'ZRDC'.
*
        WHEN OTHERS.

          CALL FUNCTION 'ZSDMF002_POSICAO_FINANCEIRA'
            EXPORTING          "BUKRS = WG_HEADER-VKORG
              kunnr                = wg_header-kunnr
              waerk                = wg_header-waerk
            IMPORTING
              vlr_vencido_brl      = wg_posicao-vlr_f_vencida
              vlr_avencer_brl      = wg_posicao-vlr_f_avencer
              vlr_adiantado_brl    = wg_posicao-vlr_f_adiant
              limite_credito       = wg_posicao-vlr_limite
              total_brl            = wg_posicao-vlr_total
              total_movimento_brl  = wg_posicao-vlr_total_mov
              saldo_disponivel_brl = wg_posicao-vlr_saldo
              utilizado_limite_brl = wg_posicao-utilizado.

      ENDCASE.

      wg_hora = sy-uzeit.
      wg_data = sy-datum.
      MOVE: c_atual TO wg_acao.

    WHEN c_search.
      PERFORM busca_dados.
      PERFORM: busca_posicao.
    WHEN c_copy.
**    Valida se existe documento para ser modificado.
*      SELECT SINGLE doc_simulacao
*        FROM zsdt0040
*        INTO wg_header-doc_simulacao
*         WHERE doc_simulacao EQ wg_header-doc_simulacao.

      IF sy-subrc IS INITIAL.
        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.
*        PERFORM get_next_number  USING  'ZSIMULACAO'
*                                       '1'
*                              CHANGING wg_heade  r-doc_simulacao.

*        MOVE: sy-datum TO wg_header-erdat,
*              sy-uzeit TO wg_header-erzet.

        MOVE c_modif TO wg_acao.
      ENDIF.
*    WHEN c_descp.
*      PERFORM inputa_desc.
    WHEN c_modif_qtd.

      var_edit = abap_false.

**    Valida se existe documento para ser modificado.
      SELECT SINGLE nro_sol_ov
        FROM zsdt0051
        INTO wg_header-nro_sol_ov
         WHERE nro_sol_ov EQ wg_header-nro_sol_ov
           AND status     NE 'D'.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM zsdt0060
           INTO wl_0060
           WHERE usnam EQ sy-uname
            AND  programa EQ sy-cprog.
        IF sy-subrc IS INITIAL
        OR wg_redistribuir IS NOT INITIAL.
          SELECT *
            FROM zsdt0061
            INTO TABLE tl_0061
             WHERE nro_sol_ov EQ wg_header-nro_sol_ov
               AND usnam      EQ sy-uname.
          SORT: tl_0061 BY item.
          DELETE ADJACENT DUPLICATES FROM tl_0061 COMPARING item.
          DESCRIBE TABLE tl_0061 LINES wl_lines.

          IF wl_lines LT wl_0060-qtd_alt_ov
          OR wg_redistribuir IS NOT INITIAL.


            PERFORM limpa_variavel USING c_modif_qtd.
            PERFORM busca_dados_doc.
            PERFORM busca_dados.
            PERFORM monta_log.
            SELECT SINGLE *
              FROM zsdt0053
              INTO wl_0053
               WHERE nro_sol_ov EQ wg_header-nro_sol_ov
                 AND vbeln      NE space.
            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE *
                FROM zsdt0066
                INTO wl_0066
                 WHERE nro_sol_ov EQ wg_header-nro_sol_ov
                   AND vbeln      NE space
                   AND status NE 'D'.
            ENDIF.
            IF sy-subrc IS INITIAL.
              PERFORM busca_posicao.
              IF wg_redistribuir IS INITIAL.
                REFRESH: tg_historico.
                LOOP AT tg_itens WHERE vbeln IS NOT INITIAL.
                  tg_historico-nro_sol_ov = wg_header-nro_sol_ov.
                  tg_historico-posnr = tg_itens-posnr.
                  tg_historico-matnr = tg_itens-matnr.
                  tg_historico-zmeng_old = tg_itens-zmeng.

                  APPEND tg_historico.
                  CLEAR:tg_historico.
                ENDLOOP.
              ELSE.

                " 08.10.2024 - RAMON -- 154003 - (comentado) -->
*****                 LOOP AT tg_itens.

******* Stefanini - IR193583 - 18/09/2024 - LAZAROSR - Início de Alteração
*****                  tg_itens-kunnr = wg_header-kunnr.
*****                  MODIFY tg_itens.
****                  IF tg_itens-vbeln IS INITIAL.
****
****                    tg_itens-kunnr = wg_header-kunnr.
****                    MODIFY tg_itens.
****
****                  ENDIF.
******* Stefanini - IR193583 - 18/09/2024 - LAZAROSR - Fim de Alteração

*****                ENDLOOP.

                " 08.10.2024 - RAMON -- 154003 - (comentado) --<

              ENDIF.
              MOVE c_modif_qtd TO wg_acao.
              CALL FUNCTION 'ENQUEUE_EZSDT0051'
                EXPORTING
                  nro_sol_ov     = wg_header-nro_sol_ov
                EXCEPTIONS
                  foreign_lock   = 1
                  system_failure = 2
                  OTHERS         = 3.
              IF sy-subrc <> 0.
                MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
              ENDIF.
              CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
                EXPORTING
                  functioncode           = 'TAB_STRIP_FC2'
                EXCEPTIONS
                  function_not_supported = 1.

            ELSE.
              MOVE c_atual TO wg_acao.
              CLEAR: wg_redistribuir.
            ENDIF.
          ELSE.
            MOVE c_atual TO wg_acao.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m10.
          ENDIF.
        ELSE.
          MOVE c_atual TO wg_acao.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m11.
        ENDIF.
      ENDIF.
    WHEN c_modif.
**    Valida se existe documento para ser modificado.
      SELECT SINGLE nro_sol_ov
        FROM zsdt0051
        INTO wg_header-nro_sol_ov
         WHERE nro_sol_ov EQ wg_header-nro_sol_ov
           AND status     NE 'D'.
      IF sy-subrc IS INITIAL.
        PERFORM limpa_variavel USING c_atual.
        PERFORM busca_dados_doc.
        PERFORM busca_dados.
        PERFORM monta_log.
        CLEAR: wl_0066.
        SELECT SINGLE *
          FROM zsdt0066
          INTO wl_0066
           WHERE nro_sol_ov EQ wg_header-nro_sol_ov
             AND vbeln      NE space
             AND status NE 'D'.

        CLEAR: wl_0053.
* RIM - CS1011302 - ANB - 20220810 - Begin
        IF wg_header-inco1 EQ 'FOB' AND wg_header-risco_sacado EQ 'S'.
          SELECT SINGLE *
            FROM zsdt0053
            INTO wl_0053
             WHERE nro_sol_ov EQ wg_header-nro_sol_ov
               AND vbeln      EQ ''
               AND status_itm NE 'F'.
          IF sy-subrc IS INITIAL.
            CLEAR wl_0053.
          ENDIF.
        ELSE.
* RIM - CS1011302 - ANB - 20220810 - End
          SELECT SINGLE *
            FROM zsdt0053
            INTO wl_0053
             WHERE nro_sol_ov EQ wg_header-nro_sol_ov
               AND status_itm EQ 'F'.
          IF ( wl_0053 IS INITIAL ).
            SELECT SINGLE *
              FROM zsdt0053
              INTO wl_0053
               WHERE nro_sol_ov EQ wg_header-nro_sol_ov
                 AND vbeln NE space.
          ENDIF.
* RIM - CS1011302 - ANB - 20220810 - Begin
        ENDIF.
* RIM - CS1011302 - ANB - 20220810 - End
        IF wl_0053 IS INITIAL
        AND wl_0066 IS INITIAL.
          CALL FUNCTION 'ENQUEUE_EZSDT0051'
            EXPORTING
              nro_sol_ov     = wg_header-nro_sol_ov
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

*          PERFORM REFRESH_PRECO.
          CASE wg_header-auart.
            WHEN 'ZRFL' OR 'ZUB' OR 'ZRDC'.
*
            WHEN OTHERS.
              CALL FUNCTION 'ZSDMF002_POSICAO_FINANCEIRA'
                EXPORTING
                  "BUKRS                      = WG_HEADER-VKORG
                  kunnr                = wg_header-kunnr
                  waerk                = wg_header-waerk
                IMPORTING
                  vlr_vencido_brl      = wg_posicao-vlr_f_vencida
*                 VLR_VENCIDO_USD      =
                  vlr_avencer_brl      = wg_posicao-vlr_f_avencer
*                 VLR_AVENCER_USD      =
                  vlr_adiantado_brl    = wg_posicao-vlr_f_adiant
*                 VLR_ADIANTADO_USD    =
                  limite_credito       = wg_posicao-vlr_limite
                  total_brl            = wg_posicao-vlr_total
*                 TOTAL_USD            =
                  total_movimento_brl  = wg_posicao-vlr_total_mov
*                 TOTAL_MOVIMENTO_USD  =
                  saldo_disponivel_brl = wg_posicao-vlr_saldo
*                 SALDO_DISPONIVEL_USD =
                  utilizado_limite_brl = wg_posicao-utilizado.
          ENDCASE.
*   UTILIZADO_LIMITE_USD       =
          wg_hora = sy-uzeit.
          wg_data = sy-datum.
          MOVE c_modif TO wg_acao.
*        call function 'ENQUEUE_EZSDT0040'
*          exporting
*            doc_simulacao  = wg_header-doc_simulacao
*          exceptions
*            foreign_lock   = 1
*            system_failure = 2
*            others         = 3.
*        if sy-subrc <> 0.
*          message id sy-msgid type sy-msgty number sy-msgno
*                  with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        endif.
        ELSEIF wg_header-param_espec EQ c_p
            OR wg_header-param_espec EQ c_m
            OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
            OR wg_header-param_espec EQ c_ax
            OR wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
          CALL FUNCTION 'ENQUEUE_EZSDT0051'
            EXPORTING
              nro_sol_ov     = wg_header-nro_sol_ov
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CALL FUNCTION 'ZSDMF002_POSICAO_FINANCEIRA'
            EXPORTING
              "BUKRS                      = WG_HEADER-VKORG
              kunnr                = wg_header-kunnr
              waerk                = wg_header-waerk
            IMPORTING
              vlr_vencido_brl      = wg_posicao-vlr_f_vencida
*             VLR_VENCIDO_USD      =
              vlr_avencer_brl      = wg_posicao-vlr_f_avencer
*             VLR_AVENCER_USD      =
              vlr_adiantado_brl    = wg_posicao-vlr_f_adiant
*             VLR_ADIANTADO_USD    =
              limite_credito       = wg_posicao-vlr_limite
              total_brl            = wg_posicao-vlr_total
*             TOTAL_USD            =
              total_movimento_brl  = wg_posicao-vlr_total_mov
*             TOTAL_MOVIMENTO_USD  =
              saldo_disponivel_brl = wg_posicao-vlr_saldo
*             SALDO_DISPONIVEL_USD =
              utilizado_limite_brl = wg_posicao-utilizado.
*   UTILIZADO_LIMITE_USD       =
          wg_hora = sy-uzeit.
          wg_data = sy-datum.
          MOVE c_modif_c_ov TO wg_acao.

          MESSAGE s836(sd) DISPLAY LIKE 'W' WITH TEXT-m12.
        ELSE.
          MOVE c_atual TO wg_acao.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m12.
        ENDIF.
      ENDIF.


      "Atribuir o ultimo risco sacado.
      wg_verifica_sacado   = wg_header-risco_sacado.
      wg_verifica_incoterm = wg_header-inco1.

    WHEN c_cancel.
      CALL FUNCTION 'DEQUEUE_EZSDT0051'
        EXPORTING
          nro_sol_ov = wg_header-nro_sol_ov.

      PERFORM limpa_variavel USING sy-ucomm.
    WHEN c_show_msgre.

      " 05.02.2024 - RAMON - Correção verificação de erros da aba preço -->
      PERFORM busca_bezei_56.
      " 05.02.2024 - RAMON --<

      PERFORM verifica_erros.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = 'X'
          i_repid       = sy-repid
          i_popup       = 0
          i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
          i_set_cell    = 'WG_CELL'
          i_set_obj     = 'WG_OBJ'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

    WHEN c_save.

      CALL METHOD grid1->check_changed_data.

      PERFORM: busca_posicao.

** Preenche o mes de fixação para Redistribuição e alterar Quantidade antes da tela de Erros.
*      IF VAR_MODRED IS NOT INITIAL
*      AND WG_HEADER-PARAM_ESPEC EQ C_M.
*        PERFORM INPUT_MONAT. "INSERE OS MONAT NA TABELA 59 PARA C AND P
*      ENDIF.

      PERFORM check_ins.
      PERFORM verifica_erros.

      IF tg_msg_ret[] IS INITIAL.

        wg_st_change =
        SWITCH #( wg_header-param_espec
*                  WHEN 'A' THEN COND #( WHEN WG_HEADER-VKORG EQ '0001' THEN COND #( WHEN WG_ST_CHANGE EQ C_L  THEN C_L ELSE C_A ) ELSE C_L )
*-CS2021000615 - 17.06.2021 - JT - inicio
                  WHEN 'P' OR 'A' THEN c_l " OR 'X' THEN c_l
*-CS2021000615 - 17.06.2021 - JT - fim
                  ELSE COND #( WHEN wg_header-nro_sol_ov(1) EQ '$' THEN c_i ELSE c_a  ) ).

        IF wg_header-nro_sol_ov(1) EQ '$'.
          PERFORM get_next_number  USING  'ZNR_SOL_OV'
                                          '01'
                                          c_x
                                 CHANGING wg_header-nro_sol_ov.
        ENDIF.

        "Modificar todas as fixações quando for do tipo M (FRAME).
        "e houver alguma mudança no risco sacado para N.
        IF ( wg_header-risco_sacado NE wg_verifica_sacado ) AND ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z' ).
          PERFORM: limpar_data_hedge_lib USING 'G' ''.
          CLEAR: wg_verifica_sacado.
        ELSEIF ( wg_header-inco1 NE wg_verifica_incoterm ) AND ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z'  ).
          PERFORM: limpar_data_hedge_lib USING 'G' ''.
          CLEAR: wg_verifica_incoterm.
        ENDIF.
        PERFORM grava_dados.
      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m13.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = 'X'
          i_repid       = sy-repid
          i_popup       = 0
          i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
          i_set_cell    = 'WG_CELL'
          i_set_obj     = 'WG_OBJ'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

    WHEN 'BACK'
      OR 'EXIT'.
      CALL FUNCTION 'DEQUEUE_EZSDT0051'
        EXPORTING
          nro_sol_ov = wg_header-nro_sol_ov.
      LEAVE TO SCREEN 0.
    WHEN c_obs.
      MOVE TEXT-p01 TO convert_texto.
      PERFORM busca_textos_obs USING convert_texto"'Texto de Observação' "text-P01
                                  wg_acao
                           CHANGING wg_header-observacao
                                    pb_obs(4).
    WHEN c_logist.
      MOVE TEXT-p02 TO convert_texto.
      PERFORM busca_textos USING convert_texto"'Texto de Comentario Logistica'"text-P02
                                  wg_acao
                           CHANGING wg_header-coment_logistica
                                    pb_coment_logistica(4).
    WHEN c_pf_vencidas.
*    WHEN C_APROV.
    WHEN c_liberar.

      MOVE abap_true TO block_botao.

      PERFORM libera_ov.

    WHEN c_reprov.
      PERFORM modifica_status USING 'R'
                              CHANGING sy-subrc.
    WHEN c_sol_aprov.
      PERFORM modifica_status USING 'P'
                              CHANGING sy-subrc.
    WHEN c_enviar.
      PERFORM limpa_variavel USING c_atual.
      PERFORM busca_dados_doc.
      PERFORM busca_dados.
      PERFORM enviar_documento.
      wg_acao = c_atual.

    WHEN c_dele.

      PERFORM verifica_erros.

      SELECT SINGLE *
       FROM zsdt0063
       INTO wl_0063
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov
        AND adiant NE ''.

** Tem Antencipação de pagamento
      IF sy-subrc = 0.
        MESSAGE s836(sd) WITH TEXT-m14
                        wg_header-nro_sol_ov
                        TEXT-m15.
        EXIT.
      ENDIF.

      SELECT SINGLE *
       FROM zsdt0054
       INTO wl_0054
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov
        AND adiant NE ''.

** Tem Antencipação de pagamento
      IF sy-subrc = 0.
        MESSAGE s836(sd) WITH TEXT-m14
                        wg_header-nro_sol_ov
                        TEXT-m15.
        EXIT.
      ENDIF.
      IF tg_msg_ret[] IS INITIAL.
        PERFORM modifica_status USING 'D'
                                CHANGING sy-subrc.
        IF sy-subrc IS INITIAL .
          "/ Se solicitação é de Performance Tp. Venda 12 e ligada a uma Tranche, ao deletar é
          "/ necessário desvincula-la da tranche correspondente na tablea zsdt0148
          PERFORM modifica_zsdt0148 USING wg_header-nro_sol_ov.

          SELECT SINGLE * FROM setleaf INTO wl_setleaf_aux WHERE setname EQ 'MAGGI_ZSDT0062_HEDGE'
                                                         AND valfrom EQ wg_header-tp_venda.

          IF ( sy-subrc EQ 0 ).
            "Disparar para negativar todo o HEDGE.
            CASE wg_header-waerk.
              WHEN: 'BRL'.
                FREE: lobj_zcl_webservice_taxa_curva.
                CREATE OBJECT lobj_zcl_webservice_taxa_curva.
                TRY.
                    lobj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                              i_tipo   = 'DEL'
                                                              i_ucomm  = 'DEL'
                                                              i_tcode  = sy-tcode
                                                              ).
                  CATCH zcx_webservice INTO cx_exception.
                    var_msg = cx_exception->get_text( ).
                    MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
                ENDTRY.
            ENDCASE.
          ENDIF.


          PERFORM limpa_variavel USING c_dele.
          wg_acao = c_atual.
          MESSAGE s836(sd) WITH TEXT-m14
                           wg_header-nro_sol_ov
                           TEXT-m16.
          LEAVE TO SCREEN 100.
        ENDIF.
      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m13.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = 'X'
          i_repid       = sy-repid
          i_popup       = 0
          i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
          i_set_cell    = 'WG_CELL'
          i_set_obj     = 'WG_OBJ'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.
    WHEN c_redist.
      PERFORM redistribuir_qtd.
      acao c_modif_qtd.
    WHEN c_show_hist.
      PERFORM show_historico.
    WHEN c_estrat.
      PERFORM show_estrat.
    WHEN c_col_exp.
      IF wg_colaps EQ '@K1@'.
        wg_colaps = '@K2@'.
      ELSE.
        wg_colaps = '@K1@'.
      ENDIF.
    WHEN c_boleto.
      PERFORM gera_boleto. "PSA
    WHEN c_cond_pg.
      PERFORM montar_layout USING 'COND_ESP'.

      IF "WG_ACAO NE C_MODIF_QTD
          wg_acao NE c_add
     AND wg_acao NE c_modif
     AND wg_acao NE c_modif_c_ov.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog.
          w_fieldcatalog-edit = space.
          MODIFY t_fieldcatalog FROM w_fieldcatalog.
        ENDLOOP.

      ELSEIF wg_acao NE c_modif_qtd
         AND wg_acao NE c_add
         AND wg_acao NE c_modif
         AND wg_acao NE c_modif_c_ov.
        DATA: "LS_GOOD TYPE LVC_S_MODI,
              tl_good TYPE lvc_t_modi.

        LOOP AT tg_cond_esp.
          ls_good-row_id = sy-tabix.
          APPEND ls_good TO tl_good.
        ENDLOOP.
        lcl_event_handler=>changed_finished_cond_esp( et_good_cells = tl_good ).
      ENDIF.
      PERFORM exibe_cond_esp.
*      CALL SCREEN 201 ENDING AT 51 9 STARTING AT 3 3.

  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

  DATA: wl_repid    TYPE sy-repid,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE.

  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.
*     create Hierarchy-header
  DATA: l_hierarchy_header TYPE treev_hhdr,
        ls_variant         TYPE disvariant.

  IF sy-ucomm IS INITIAL.
    CONCATENATE '@6Y@' TEXT-b06 INTO pb_obs.
    CONCATENATE '@6Y@' TEXT-b07 INTO pb_coment_logistica.
  ENDIF.

  CHECK lcl_busca_tela=>exit_tela( sy-ucomm ) IS INITIAL.

  wl_repid = sy-repid.

  PERFORM f_preenche_cname.

  "PERFORM verifica_erros. "Comentado 28.05.21

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
*     I_SHOW        = 'X'
      i_repid       = sy-repid
      i_popup       = 0
      i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
      i_set_cell    = 'WG_CELL'
      i_set_obj     = 'WG_OBJ'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.

** Itens da OV ( Produto/Quantidade)
  IF container1 IS NOT INITIAL.
    CALL METHOD grid1->free.
    CALL METHOD container1->free.
    CLEAR: container1, grid1.
  ENDIF.

  IF container1 IS INITIAL.  "*-CS2023000189-#126959-07.11.2023-JT
    CLEAR: wa_layout.
    REFRESH tg_sort.

    wa_layout =
    VALUE #(
              zebra      = c_x
*             no_rowmark = c_x    "*-CS2023000189-#126959-07.11.2023-JT
              cwidth_opt = c_x
              stylefname = 'STYLE'
              info_fname = 'LINE_COLOR'
              sel_mode   = 'C'    "*-CS2023000189-#126959-07.11.2023-JT
           ).

    CREATE OBJECT container1
      EXPORTING
        container_name = 'CC_01'.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.
*        i_appl_events = 'X'.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar_itens FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command_itens FOR grid1.

    PERFORM cl_gui_alv_grid USING 'ITENS'. "Exclui os Botão da ALV

    REFRESH lt_f4.
    DEFINE f4.
      lt_f4-fieldname = &1.
      lt_f4-register  = &2.
      lt_f4-getbefore = &3.
      APPEND lt_f4 .
      CLEAR lt_f4.
    END-OF-DEFINITION.

    f4:
        'INSTRUCAO'     abap_true abap_true,
        'INSTRUCAO_ANT' abap_true abap_true,
        "'NAVIO'         abap_true abap_true, Comentando selecão por nome Navio - US 73163
        "'PORTO'         abap_true abap_true, Comentando selecão por nome Porto - US 73163
        'PMEIN'         abap_true abap_true, " 18.03.2025
        'TERMINAL'      abap_true abap_true.

*  BREAK WBARBOSA.
    PERFORM montar_layout USING 'ITENS'.
    PERFORM build_dropdown.

    PERFORM fm_trata_fieldcatalog.

    " 28.11.2024 - 159383 - RAMON -->
    PERFORM f_tratar_search_help CHANGING t_fieldcatalog[].
    " 28.11.2024 - 159383 - RAMON --<

    " 23.09.2024 - 147331 - RAMON -->
    PERFORM f_set_soma_prod_qtde.
    " 23.09.2024 - 147331 - RAMON --<

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

*    CALL METHOD grid1->set_ready_for_input
*      EXPORTING
*        i_ready_for_input = 1.

  ELSE.
    CALL METHOD grid1->refresh_table_display "*-CS2023000189-#126959-07.11.2023-JT
      EXPORTING
        is_stable = wa_stable.
*   "// PBI-59125 Melhoria de perfomace Inicio
*  CALL METHOD grid1->refresh_table_display
*    EXPORTING
*      is_stable = wa_stable.
*   "// PBI-59125 Melhoria de perfomace Fim
  ENDIF.

  " 29.11.2023 - 128467 - RBL -->
  SET HANDLER: lcl_event_handler=>on_double_click          FOR grid1,
               lcl_event_handler=>on_hotspot_click         FOR grid1,
               lcl_event_handler=>handle_button_click      FOR grid1,
               lcl_event_handler=>on_data_changed_finished FOR grid1,
               lcl_event_handler=>on_data_changed          FOR grid1,
               lcl_event_handler=>on_f4                    FOR grid1.
  " 29.11.2023 - 128467 - RBL --<

** Pagamento Antecipado
  IF NOT container2 IS INITIAL.
    CALL METHOD grid2->free.
    CALL METHOD container2->free.
    CLEAR: container2, grid2.
  ENDIF.

  CLEAR: wa_layout.
  wa_layout-zebra      = c_x.
  wa_layout-stylefname = 'STYLE'.
  wa_layout-info_fname = space .
  wa_layout-sel_mode   = c_a .

  CREATE OBJECT container2
    EXPORTING
      container_name = 'CC_02'.

  CREATE OBJECT grid2
    EXPORTING
      i_parent = container2.

  CREATE OBJECT obg_toolbar
    EXPORTING
      io_alv_grid = grid2.

* Register event handler
  SET HANDLER: obg_toolbar->on_toolbar_pgt_ant          FOR grid2,
               obg_toolbar->handle_user_command_pgt_ant FOR grid2.

  PERFORM cl_gui_alv_grid USING 'PGT_ANT'. "Exclui os Botão da ALV

  PERFORM montar_layout USING 'PGT_ANT'.

  CALL METHOD grid2->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = tl_function
      is_layout            = wa_layout
      i_save               = 'X'
    CHANGING
      it_fieldcatalog      = t_fieldcatalog[]
      it_outtab            = tg_pgt_ant[].

  CALL METHOD grid2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD grid2->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    CALL METHOD GRID2->REGISTER_F4_FOR_FIELDS
*      EXPORTING
*        IT_F4 = LT_F4[].

  SET HANDLER: lcl_event_handler=>on_hotspot_click              FOR grid2,
               lcl_event_handler=>on_data_changed_finished_pgto FOR grid2,
               lcl_event_handler=>on_data_changed_pgto          FOR grid2.

** Logistica
  IF container3 IS INITIAL.
    CLEAR: wa_layout.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.
    wa_layout-stylefname = 'STYLE'.

    CREATE OBJECT container3
      EXPORTING
        container_name = 'CC_03'.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = container3.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid3.

* Register event handler
    SET HANDLER obg_toolbar->on_toolbar_logistica FOR grid3.
    SET HANDLER obg_toolbar->handle_user_command_logistica FOR grid3.

    PERFORM cl_gui_alv_grid USING 'LOGISTICA'. "Exclui os Botão da ALV

*-IR065241 - 06.07.2021 - JT - inicio
    f4: 'ZIEME'         abap_true abap_true.
*-IR065241 - 06.07.2021 - JT - fim

    PERFORM montar_layout USING 'LOGISTICA'.
    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_logistica[].

*-IR065241 - 06.07.2021 - JT - inicio
    CALL METHOD grid3->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].
*-IR065241 - 06.07.2021 - JT - fim

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid3->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: lcl_event_handler=>on_data_changed_finished_log FOR grid3,
                 lcl_event_handler=>on_data_changed_log FOR grid3,
*-IR065241 - 06.07.2021 - JT - inicio
                 lcl_event_handler=>on_f4           FOR grid3.
*-IR065241 - 06.07.2021 - JT - fim
  ELSE.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

** Preco
  IF container4 IS INITIAL.
    CLEAR: wa_layout.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-info_fname = 'LINE_COLOR'.

    CREATE OBJECT container4
      EXPORTING
        container_name = 'CC_04'.

    CREATE OBJECT grid4
      EXPORTING
        i_parent = container4.

    PERFORM cl_gui_alv_grid USING 'DINAM'. "Exclui os Botão da ALV

    REFRESH: t_fieldcatalog.

    " criação tabela dinamica #dinamica #incio #tabela
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = wg_header-tp_venda
      IMPORTING
        e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

* Cria uma field-symbol como tabela interna
    ASSIGN t_new_table->* TO <fs_table>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.

    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_double_click FOR grid4,
                 lcl_event_handler=>on_data_changed_finished_preco FOR grid4,
                 lcl_event_handler=>on_data_changed_preco FOR grid4.
  ELSE.
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
** Pagamento Adiantamento Externo
  IF container5 IS INITIAL.
    CLEAR: wa_layout.
    wa_layout-zebra      = c_x.
*    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-sel_mode   = c_a .  "*-#82210-jaime

    CREATE OBJECT container5
      EXPORTING
        container_name = 'CC_05'.

    CREATE OBJECT grid5
      EXPORTING
        i_parent = container5.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid5.

* Register event handler
    SET HANDLER: obg_toolbar->on_toolbar_pgt_ant            FOR grid5,
                 obg_toolbar->handle_hotspot_click_adto_ext FOR grid5,
                 obg_toolbar->handle_user_command_adto_ext  FOR grid5.

    PERFORM cl_gui_alv_grid USING 'ADTO_EXT'. "Exclui os Botão da ALV

    PERFORM montar_layout USING 'ADTO_EXT'.
    CALL METHOD grid5->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_adto_ext[].

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid5->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: lcl_event_handler=>on_data_changed_finished_adto FOR grid5,
                 lcl_event_handler=>on_data_changed_adto_ext FOR grid5.
  ELSE.

    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

** Instrução
  IF container6 IS INITIAL.
    CLEAR: wa_layout.
    REFRESH tg_sort.

    wa_layout-zebra      = c_x.
    wa_layout-stylefname = space .
    wa_layout-grid_title = space .
    wa_layout-info_fname = 'COLOR'.
    wa_layout-sel_mode   = 'C'.

    CREATE OBJECT container6
      EXPORTING
        container_name = 'CC_06'.

    CREATE OBJECT grid6
      EXPORTING
        i_parent = container6.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid6.

* Register event handler
    SET HANDLER: obg_toolbar->handle_user_command_ins FOR grid6,
                 obg_toolbar->on_toolbar_ins          FOR grid6.

    PERFORM cl_gui_alv_grid USING 'INSTRUCAO'. "Exclui os Botão da ALV

    PERFORM montar_layout USING 'INSTRUCAO'.

    CALL METHOD grid6->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
        i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_instrucao[]
*       IT_SORT              = TG_SORT[].
      .
    CALL METHOD grid6->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid6->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid6->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: lcl_event_handler=>on_data_changed_finished_ins  FOR grid6,
                 lcl_event_handler=>on_data_changed_ins           FOR grid6.

  ELSE.
    CALL METHOD grid6->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

** Formação de Lote
  IF container7 IS INITIAL.
    CLEAR: wa_layout.
    REFRESH tg_sort.

    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-no_toolbar = space.
    wa_layout-grid_title = space .
*    WA_STABLE-ROW        = C_X.
    wa_layout-info_fname = 'COLOR'.
    wa_layout-grid_title = space .

    CREATE OBJECT container7
      EXPORTING
        container_name = 'CC_07'.

    CREATE OBJECT grid7
      EXPORTING
        i_parent = container7.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid7.

* Register event handler
    SET HANDLER: obg_toolbar->on_toolbar_form          FOR grid7,
                 obg_toolbar->handle_user_command_form FOR grid7.

    PERFORM cl_gui_alv_grid USING 'FORM_LOTE'. "Exclui os Botão da ALV

    REFRESH: lt_f4.
    lt_f4-fieldname = 'INSTRUCAO'.
    lt_f4-register  = 'X' .
    lt_f4-getbefore = 'X' .
    APPEND lt_f4 .

    PERFORM montar_layout USING 'FORM_LOTE'.

    CALL METHOD grid7->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_form_lote[]
*       it_sort              = tg_sort[].
      .
    CALL METHOD grid7->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid7->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid7->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: lcl_event_handler=>on_hotspot_click_form         FOR grid7,
                 lcl_event_handler=>on_data_changed_finished_form FOR grid7,
                 lcl_event_handler=>on_data_changed_form          FOR grid7,
                 lcl_event_handler=>on_onf4                       FOR grid7.
  ELSE.
    CALL METHOD grid7->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

  IF container8 IS INITIAL.
    CREATE OBJECT tree_event_receiver.

    CREATE OBJECT container8
      EXPORTING
        container_name = 'CC_08'.

    CREATE OBJECT splitter
      EXPORTING
        parent  = container8
        rows    = 1
        columns = 2.

    CALL METHOD splitter->set_column_sash
      EXPORTING
        id    = 1
        type  = splitter->type_movable
        value = splitter->false.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_s1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_column_width
      EXPORTING
        id    = 1
        width = 100.

*    PERFORM MONTAR_LAYOUT USING 'PRECO_FRAME'.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = wg_header-tp_venda
      IMPORTING
        e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

* Cria uma field-symbol como tabela interna
    ASSIGN t_new_table->* TO <fs_table_frame>.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog.
      IF w_fieldcatalog-fieldname EQ 'BEZEI'.
        w_fieldcatalog-no_out = c_x.
      ENDIF.
      w_fieldcatalog-tabname = '<FS_TABLE_FRAME>'.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

* create tree control
    CREATE OBJECT tree1
      EXPORTING
        parent                      = container_s1
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
        item_selection              = 'X'
        no_html_header              = 'X'
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.

    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.

    PERFORM build_hierarchy_header CHANGING l_hierarchy_header.

    ls_variant-report = sy-repid.

* create emty tree-control
    CALL METHOD tree1->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = l_hierarchy_header
        i_save              = 'A'
        is_variant          = ls_variant
      CHANGING
        it_outtab           = <fs_table_frame>
        it_fieldcatalog     = t_fieldcatalog.

    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.

* define the events which will be passed to the backend
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
    CLEAR l_event.
    l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.
* register events
    CALL METHOD tree1->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

* set handler for tree1
    SET HANDLER: tree_event_receiver->handle_double_click       FOR tree1,
                 tree_event_receiver->handle_item_double_click  FOR tree1,
                 tree_event_receiver->handle_expand_no_children FOR tree1.
* calculate totals
    CALL METHOD tree1->update_calculations.
  ELSE.
* calculate totals
    CALL METHOD tree1->update_calculations.
* this method must be called to send the data to the frontend
    CALL METHOD tree1->frontend_update.
  ENDIF.


** Pedido
  IF container9 IS INITIAL.
    CLEAR: wa_layout.
    REFRESH tg_sort.

*    WA_LAYOUT-ZEBRA      = C_X.
*    WA_LAYOUT-NO_ROWMARK = C_X.
*    WA_STABLE-ROW        = C_X.
*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT-NO_TOOLBAR = SPACE.
*    WA_LAYOUT-GRID_TITLE = SPACE .
*    WA_STABLE-ROW        = C_X.
*    WA_LAYOUT-INFO_FNAME = 'COLOR'.
*    WA_LAYOUT-GRID_TITLE = SPACE .

    wa_layout-zebra      = c_x.
    wa_layout-stylefname = space .
    wa_layout-grid_title = space .
    wa_layout-info_fname = 'COLOR'.
    wa_layout-sel_mode   = 'C'.

    CREATE OBJECT container9
      EXPORTING
        container_name = 'CC_09'.

    CREATE OBJECT grid10
      EXPORTING
        i_parent = container9.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid10.

* Register event handler
*    SET HANDLER: OBG_TOOLBAR->ON_TOOLBAR_FORM          FOR GRID10,
*                 OBG_TOOLBAR->HANDLE_USER_COMMAND_FORM FOR GRID10.

    PERFORM cl_gui_alv_grid USING 'PEDIDO'. "Exclui os Botão da ALV

    REFRESH: lt_f4.
    lt_f4-fieldname = 'PEDIDO'.
    lt_f4-register  = 'X'.
    lt_f4-getbefore = 'X'.
    APPEND lt_f4.

    PERFORM montar_layout USING 'PEDIDO'.

    CALL METHOD grid10->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_pedido[]
*       it_sort              = tg_sort[].
      .
    CALL METHOD grid10->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid10->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid10->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: lcl_event_handler=>on_data_changed_pedido        FOR grid10,
                 lcl_event_handler=>on_data_changed_finished_ped  FOR grid10.

    SET HANDLER: obg_toolbar->handle_user_command_pedido          FOR grid10.

  ELSE.
    CALL METHOD grid10->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

*   "// PBI-59125 Melhoria de perfomace Inicio
*  CALL METHOD cl_gui_cfw=>flush.
*   "// PBI-59125 Melhoria de perfomace Fim


ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout USING p_alv .
  REFRESH: t_fieldcatalog, it_fcat1.
  seq_alv = 0.
  CASE p_alv.
    WHEN 'ITENS'.
      PERFORM montar_estrutura USING:
          " 1  2          3             4          5             6              7    8   9   10  11
          1 ' '         ' '           'TG_ITENS' 'STATUS_ICON' 'Status'       '4'  ' ' ' ' ' ' ' ',
          2 'ZSDT0053'  'POSNR'       'TG_ITENS' 'POSNR'       ' '            ' '  ' ' ' ' ' ' ' '.

      IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
         wg_header-param_espec EQ c_ax OR
        wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
        PERFORM montar_estrutura USING:
          3 ''  ''     'TG_ITENS' 'INSTRUCAO_ANT' 'Instrução Ant' '15'  ' ' ' ' ' ' ' ',
          "3 'ZSDT0053'  'INSTRUCAO_ANT' 'TG_ITENS' 'INSTRUCAO_ANT' 'Instrução Ant' ' '  'X' ' ' ' ' ' ',
          3 ''  ''     'TG_ITENS' 'INSTRUCAO'     'Instrução'     ' '  ' ' ' ' ' ' ' '."LG 25/11/2019


      ELSE.
        PERFORM montar_estrutura USING:
         20 'ZSDT0053'  'VOLUM'       'TG_ITENS' 'VOLUM'       'Volume'       ' '  'X' ' ' ' ' ' '.
      ENDIF.

      PERFORM montar_estrutura USING:
              4 'ZSDT0073'  'FIXACAO'     'TG_ITENS' 'FIXACAO'     'Fixação'      ' '  'X' ' ' ' ' ' ',
              5 'ZSDT0053'  'MATNR'       'TG_ITENS' 'MATNR'       ' '            '7'  'X' ' ' ' ' ' ',
              6 'MAKT'      'MAKTX'       'TG_ITENS' 'MAKTX'       ' '            ' '  ' ' ' ' ' ' ' ',
              7 'ZSDT0053'  'WERKS'       'TG_ITENS' 'WERKS'       ' '            ' '  'X' ' ' ' ' ' ',
              8 'ZSDT0053'  'PONTO_C'     'TG_ITENS' 'PONTO_C'     'Ponto Coleta' ' '  'X' ' ' ' ' ' ',
              9 ''          ''            'TG_ITENS' 'TERMINAL'    'Terminal'     ' '  'X' ' ' ' ' ' ',
              10 'ZSDT0053'  'LGORT'       'TG_ITENS' 'LGORT'       ' '            ' '  'X' ' ' ' ' ' ',
              11 'ZSDT0053'  'CHARG'       'TG_ITENS' 'CHARG'       ' '            ' '  'X' ' ' ' ' ' ',
              12 'ZSDT0053'  'ZMENG'       'TG_ITENS' 'ZMENG'       ' '            ' '  'X' ' ' ' ' ' ',
              13 'ZSDT0053'  'BRGEW'       'TG_ITENS' 'BRGEW'       ' '            ' '  'X' ' ' ' ' ' ',
              14 'ZSDT0053'  'ZIEME'       'TG_ITENS' 'ZIEME'       ' '            ' '  'X' ' ' ' ' ' ',
              15 'BSID'      'DMBTR'       'TG_ITENS' 'DMBTR'       'Preço'        ' '  ' ' ' ' ' ' ' ',
              16 'ZSDT0053'  'PMEIN'       'TG_ITENS' 'PMEIN'       'UM Preço'     ' '  'X' ' ' ' ' ' ',
              17 'VBKD'      'KURSK'       'TG_ITENS' 'KURSF'       ' '            ' '  'X' ' ' ' ' ' ',
              18 'ZSDT0053'  'DESC_ABSOLUTO' 'TG_ITENS' 'DESC_ABSOLUTO'       'Desc/Acresc' ' '  ' ' ' ' ' ' ' ',
              19 'BSID'      'DMBTR'       'TG_ITENS' 'VLRTOT'      'Valor Total'  ' '  ' ' ' ' ' ' ' ',

              23 'ZSDT0053'  'VOLEH'       'TG_ITENS' 'VOLEH'       'UM Volume'    ' '  'X' ' ' ' ' ' ',
              24 'ZSDT0053'  'VALDT'       'TG_ITENS' 'VALDT'       'Dta. Venc.'   ' '  'X' ' ' ' ' ' ',
              25 'ZSDT0053'  'VBELN'       'TG_ITENS' 'VBELN'       ' '            ' '  ' ' ' ' ' ' ' ',
              26 'ZSDT0053'  'CONTAINER'   'TG_ITENS' 'CONTAINER'   ' '            ' '  'X' ' ' ' ' ' ',
              27 'ZSDT0053'  'KUNNR'       'TG_ITENS' 'KUNNR'       ' '            ' '  ' ' ' ' ' ' ' '.

      CASE wg_header-param_espec.
        WHEN c_m.
          PERFORM montar_estrutura USING:
           27 'ZSDT0053'  'DATA_LIB_FRAME' 'TG_ITENS' 'DATA_LIB_FRAME'       'Dta. Liberação'            ' '  ' ' ' ' ' ' ' '.
        WHEN c_p.
          PERFORM montar_estrutura USING:
           12 'ZSDT0053'  'P_PORTO'             'TG_ITENS' 'P_PORTO'           'Preço Porto'  ' '  'X' ' ' ' ' ' ',
           15 'ZSDT0053'  'VLT_PORTO'           'TG_ITENS' 'VLT_PORTO'         'Val.Tl.Porto' ' '  'X' ' ' ' ' ' ',
           22 'ZSDT0053'  'ID_NOMEACAO_TRAN'    'TG_ITENS' 'ID_NOMEACAO_TRAN'  'Id.Nomeação'  ' '  'X' ' ' ' ' ' ',
           22 ''          ''                    'TG_ITENS' 'NAVIO'             'Navio'        ' '  ' ' ' ' ' ' ' ',
           23 'ZSDT0053'  'CODIGO_RA'           'TG_ITENS' 'CODIGO_RA'         'Codigo RA'    ' '  ' ' ' ' ' ' ' ',
           23 ''          ''                    'TG_ITENS' 'PORTO'             'Porto'        ' '  ' ' ' ' ' ' ' '.
*-CS2021000615 - 17.06.2021 - JT - inicio
        WHEN c_a OR c_ax.
*-CS2021000615 - 17.06.2021 - JT - fim
          PERFORM montar_estrutura USING:

            21 ' '         ' '             'TG_ITENS' 'LOTES_VOL'     'Lotes/Vol' ' '  ' ' ' ' ' ' ' ',
            22 'ZSDT0053'  'VOLUM'         'TG_ITENS' 'VOLUM'       'Volume'       ' '  ' ' ' ' ' ' ' ',
            23 'ZSDT0053'  'AUART'         'TG_ITENS' 'AUART'       'Tipo OV'       ' '  ' ' ' ' ' ' ' '.
        WHEN  c_z." CS2020000373 23/08/2022 -LP
          PERFORM montar_estrutura USING:
            27 'ZSDT0053'  'DATA_LIB_FRAME' 'TG_ITENS' 'DATA_LIB_FRAME'       'Dta. Liberação'            ' '  ' ' ' ' ' ' ' ',
            21 ' '         ' '             'TG_ITENS' 'LOTES_VOL'     'Lotes/Vol' ' '  ' ' ' ' ' ' ' ',
            22 'ZSDT0053'  'VOLUM'         'TG_ITENS' 'VOLUM'       'Volume'       ' '  ' ' ' ' ' ' ' ',
            23 'ZSDT0053'  'AUART'         'TG_ITENS' 'AUART'       'Tipo OV'       ' '  ' ' ' ' ' ' ' '.

      ENDCASE.

      CASE wg_header-param_espec.

          " 15.12.2023 - 128467 - RBL -->
        WHEN c_a OR c_ax OR c_z OR c_p." CS2020000373 23/08/2022 -LP
          PERFORM montar_estrutura USING:
            24 'ZSDT0053'  'NUMERO_RUC'    'TG_ITENS' 'NUMERO_RUC'  'RUC'          ' '  'X' ' ' ' ' ' '.
*        WHEN c_p.
*          PERFORM montar_estrutura USING:
*             24 'ZSDT0053'  'NUMERO_RUC'    'TG_ITENS' 'NUMERO_RUC'  'RUC'          ' '  ' ' ' ' ' ' ' '.
          " 15.12.2023 - 128467 - RBL --<
      ENDCASE.

      PERFORM montar_estrutura USING:
        28 'ZSDT0053'  'KVGR3'         'TG_ITENS' 'KVGR3'         'Convencional/RR'      ' '  'X' ' ' ' ' ' ',
        29 'ZSDT0053'  'CK_TROCA_NOTA' 'TG_ITENS' 'CK_TROCA_NOTA' 'Troca Nota?'          ' '  'X' ' ' ' ' ' ',
        30 'ZSDT0053'  'KVGR5'         'TG_ITENS' 'KVGR5'         'Frete Entrada?'       ' '  'X' ' ' ' ' ' '.

*-#113631-18.01.2024-JT-inicio
* RJF - Ini
      CASE wg_header-param_espec.
        WHEN c_a OR c_z .
          PERFORM montar_estrutura USING:
            31 'ZSDT0053'  'TP_ATO'         'TG_ITENS' 'TP_ATO'        'Tipo Drawback'             ' '  'X' ' ' ' ' ' ',
            32 'ZSDT0053'  'NR_DRAWBACK'    'TG_ITENS' 'NR_DRAWBACK'   'Numero do Ato'             ' '  'X' ' ' ' ' ' ',
            33 'ZSDT0053'  'QTD_DRAWBACK'   'TG_ITENS' 'QTD_DRAWBACK'  'Quantidade Drawback(KG)'   ' '  'X' ' ' ' ' ' '.
      ENDCASE.
* RJF - Fim
*-#113631-18.01.2024-JT-fim

    WHEN 'PGT_ANT'.
      PERFORM montar_estrutura USING:
          01 'ZSDT0054'  'POSNR'       'TG_PGT_ANT' 'POSNR'       ' '              ' ' 'X' ' ' ' ' ' ',
          02 'ZSDT0054'  'VALDT'       'TG_PGT_ANT' 'VALDT'       'Dta.Vencimento' ' ' 'X' ' ' ' ' ' ',
          03 'ZSDT0054'  'DMBTR'       'TG_PGT_ANT' 'DMBTR'       'Valor'          ' ' 'X' ' ' ' ' ' ',
          04 'ZSDT0054'  'KURSF'       'TG_PGT_ANT' 'KURSF'       ' '              ' ' 'X' ' ' ' ' ' ',
          05 'ZSDT0054'  'VLR_REAL'    'TG_PGT_ANT' 'VLR_REAL'    'Valor Real'     ' ' ' ' 'X' ' ' ' ',
          06 'ZSDT0054'  'ADIANT'      'TG_PGT_ANT' 'ADIANT'      ' '              ' ' ' ' ' ' ' ' ' ',
          07 'BSAD'      'AUGBL'       'TG_PGT_ANT' 'AUGBL'       ' '              ' ' ' ' ' ' ' ' ' ',
          08 'BSAD'      'AUGDT'       'TG_PGT_ANT' 'AUGDT'       ' '              ' ' ' ' ' ' ' ' ' '.

      IF wg_header-param_espec = c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
         wg_header-param_espec = c_ax OR
        wg_header-param_espec = c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
        PERFORM montar_estrutura USING:
          09 'ZSDT0054'  'PERC_ADIANT'   'TG_PGT_ANT' 'PERC_ADIANT'   'Adiantamento %'   '' '' '' '' '',
          10 'ZSDT0054'  'NR_PROVIS_INV' 'TG_PGT_ANT' 'NR_PROVIS_INV' 'Nr. provisão'     '' '' '' '' '',
          11 'BSAD'      'AUGDT'         'TG_PGT_ANT' 'DT_EMISSAOPI'  'Dt. Emissão PI'   '' 'X' '' '' '',
          12 'BSAD'      'AUGDT'         'TG_PGT_ANT' 'DT_RETORNOPI'  'Dt. Retorno PI'   '' 'X' '' '' ''.
      ENDIF.

    WHEN 'LOGISTICA'.
      PERFORM montar_estrutura USING:
          01 ''          ''                   'TG_LOGISTICA'   'FIXACAO'        'Fixação'            '' 'X' '' '' '',
          01 'ZSDT0055'  'DATA_PROGR'         'TG_LOGISTICA'   'DATA_PROGR'     ' '                  '' 'X' '' '' '',
          01 'ZSDT0055'  'CADENCIA_QTE'       'TG_LOGISTICA'   'CADENCIA_QTE'   ' '                  '' 'X' '' '' '',
          01 'ZSDT0055'  'ZIEME'              'TG_LOGISTICA'   'ZIEME'          ' '                  '' 'X' '' '' '',
          01 'ZSDT0055'  'DATA_PROGR'         'TG_LOGISTICA'   'VALDT_HEDGE'    'Dt. Risco Sacado'   '' ' ' '' '' ''.

*-IR065241 - 06.07.2021 - JT - inicio
      READ TABLE t_fieldcatalog INTO DATA(w_fieldcat) WITH KEY fieldname = 'ZIEME'.
      IF sy-subrc = 0.
        w_fieldcat-ref_table = abap_false.
        w_fieldcat-ref_field = abap_false.
        w_fieldcat-coltext   = 'UM'.
        MODIFY t_fieldcatalog FROM w_fieldcat INDEX sy-tabix.
      ENDIF.
*-IR065241 - 06.07.2021 - JT - fim

    WHEN 'PRECO'.
      PERFORM montar_estrutura USING:
          01 'ZSDT0059'  'BEZEI'              'TG_PRECO' 'BEZEI'        ' '          '' ' ' ' ' ' ' ' ',
          01 ' '         ' '                  'TG_PRECO' 'FORMULA2'     'Preço'      '' ' ' ' ' ' ' ' ',
          01 'BSID'      'WAERS'              'TG_PRECO' 'WAERS'        ' '          '' ' ' ' ' ' ' ' ',
          01 ' '      ' '                     'TG_PRECO' 'CBOT'         'REF. CBOT'  '' ' ' ' ' ' ' ' '.

    WHEN 'PRECO_FRAME'.
      PERFORM montar_estrutura USING:
          01 ' '           ' '                'TG_PRECO' 'FORMULA2'     'Preço'        '20'  ' ' ' ' ' ' ' ',
          01 'BSID'        'WAERS'            'TG_PRECO' 'WAERS'        ' '            '7'   ' ' ' ' ' ' ' ',
          01 ' '           ' '                'TG_PRECO' 'CBOT'         'REF. CBOT'    '7'   ' ' ' ' ' ' ' ',
          01 'ZSDT0059'    'MONAT'            'TG_PRECO' 'MONAT'        'Mês Fixação'  '10'  ' ' ' ' ' ' ' ',
          01 'ZSDT0059'    'ZMENG'            'TG_PRECO' 'ZMENG'        'Qtd. Fixada'  '20'  ' ' ' ' ' ' ' ',
          01 'ZSDT0059'    'VALDT'            'TG_PRECO' 'VALDT'        'Dt. Fixação'  '15'  ' ' ' ' ' ' ' '.

    WHEN 'PRECO_FRAME2'.
      PERFORM montar_estrutura USING:
         01 'ZSDT0059'    'BEZEI'            'TG_PRECO' 'BEZEI'        ' '            '20' ' ' ' ' ' ' ' ',
         01 ' '           ' '                'TG_PRECO' 'FORMULA2'     'Preço'        '20' ' ' ' ' ' ' ' ',
         01 'BSID'        'WAERS'            'TG_PRECO' 'WAERS'        ' '            '20' ' ' ' ' ' ' ' ',
         01 ' '           ' '                'TG_PRECO' 'CBOT'         'REF. CBOT'    '20' ' ' ' ' ' ' ' ',
         01 'ZSDT0059'    'MONAT'            'TG_PRECO' 'MONAT'        'Mês Fixação'  '10' ' ' ' ' ' ' ' ',
         01 'ZSDT0059'    'ZMENG'            'TG_PRECO' 'ZMENG'        'Qtd. Fixada'  '20' ' ' ' ' ' ' ' ',
         01 'ZSDT0059'    'VALDT'            'TG_PRECO' 'VALDT'        'Dt. Fixação'  '15' ' ' ' ' ' ' ' '.

    WHEN 'SALDO'.
      REFRESH: estrutura.
      PERFORM montar_estrutura_alv USING:
         1 'VBFA'      'VBELV'              'TG_SALDO' 'VBELN'        ' '           ' ' ' ' ' ' ' ',
         1 'ZSDT0053'  'WERKS'              'TG_SALDO' 'WERKS'        ' '           ' ' ' ' ' ' ' ',
         1 'ZSDT0053'  'ZMENG'              'TG_SALDO' 'ZMENG'        'Qtd.Ov.'     ' ' ' ' ' ' ' ',
         1 'VBFA'      'RFMNG'              'TG_SALDO' 'TOTAL'        'Qtd.Remessa' ' ' ' ' ' ' ' ',
         1 'ZSDT0053'  'ZMENG'              'TG_SALDO' 'SALDO'        'Saldo Qtd.'  ' ' ' ' ' ' ' '.

    WHEN 'COND_ESP'.
      PERFORM montar_estrutura USING:
         1 'ZSDT0073'  'FIXACAO'            'TG_COND_ESP' 'FIXACAO'      'Fixação'        '7' ' ' ' ' ' ' '',
         2 'ZSDT0073'  'ZTERM'              'TG_COND_ESP' 'ZTERM'        ' '              '7' 'X' ' ' ' ' '',
         3 'ZSDT0073'  'QTE_VENC'           'TG_COND_ESP' 'QTE_VENC'     'Qtd.Vencimento' '8' 'X' ' ' ' ' ''.

    WHEN 'SAIDA_EXEC'.
      REFRESH: estrutura.
      PERFORM montar_estrutura_alv USING:
         1  'ZSDT0053'   'NRO_SOL_OV'       'TL_SAIDA_EXEC' 'NRO_SOL_OV' ' '            ' '  ' '  ' ' ' ',
         1  'ZSDT0053'   'POSNR'            'TL_SAIDA_EXEC' 'POSNR'      ' '            ' '  ' '  ' ' ' ',
         1  'ZSDT0053'   'ZMENG'            'TL_SAIDA_EXEC' 'ZMENG'      ' '            ' '  ' '  ' ' ' ',
         2  'ZSDT0053'   'VALDT'            'TL_SAIDA_EXEC' 'VALDT'      ' '            ' '  ' '  ' ' ' ',
         3  'ZSDT0053'   'VLRTOT'           'TL_SAIDA_EXEC' 'VLRTOT'     ' '            ' '  ' '  ' ' ' ',
         4  'VBAK'       'VBELN'            'TL_SAIDA_EXEC' 'VBELN'      ' '            ' '  ' '  ' ' ' ',
         4  ' '          ' '                'TL_SAIDA_EXEC' 'MSG'        'Msg de bapi'  '80' ' '  ' ' ' '.

    WHEN 'MOTIVO'.
      REFRESH: estrutura.
      PERFORM montar_estrutura_alv USING:
         1  'ZSDT0069'    'ID_HISTORICO'     'TL_MOTIVO' 'ID_HISTORICO'  ' '            ' ' ' '  ' ' ' ',
         2  ''            ''                 'TL_MOTIVO' 'NIVEL'         'Nivel'        ' ' ' '  ' ' ' ',
         3  ' '           ' '                'TL_MOTIVO' 'STATUS'        'Status'       ' ' ' '  ' ' ' ',
         4  'ZSDT0069'    'MOTIVO'           'TL_MOTIVO' 'MOTIVO'        ' '            ' ' ' '  ' ' ' ',
         5  'ZSDT0069'    'USNAM'            'TL_MOTIVO' 'USNAM'         'Usuário'      ' ' ' '  ' ' ' ',
         6  'ZSDT0069'    'DATA_ATUAL'       'TL_MOTIVO' 'DATA_ATUAL'    'Data Atual.'  ' ' ' '  ' ' ' ',
         6  'ZSDT0069'    'HORA_ATUAL'       'TL_MOTIVO' 'HORA_ATUAL'    'Hora Ataul.'  ' ' ' '  ' ' ' '.

    WHEN 'ESTRAT'.
      REFRESH: estrutura.
      PERFORM montar_estrutura_alv USING:
         1  'ZSDT0161'    'NIVEL'       'TG_ESTRAT' 'NIVEL'      ''           ' ' ' '  ' ' ' ',
         2  'ZSDT0161'    'APROVADOR'   'TG_ESTRAT' 'APROVADOR'  ''           ' ' ' '  ' ' ' ',
         3  'ZSDT0161'    'VALOR_DE'    'TG_ESTRAT' 'VALOR_DE'   'Valor De'   ' ' ' '  ' ' ' ',
         4  'ZSDT0161'    'VALOR_ATE'   'TG_ESTRAT' 'VALOR_ATE'  'Valor Até'  ' ' ' '  ' ' ' '.
*         5  'ZSDT0161'    'USNAM'       'TG_ESTRAT' 'USNAM'         'Usuário'      ' ' ' '  ' ' ' ',
*         6  'ZSDT0161'    'DATA_ATUAL'  'TG_ESTRAT' 'DATA_ATUAL'    'Data Atual.'  ' ' ' '  ' ' ' ',
*         6  'ZSDT0161'    'HORA_ATUAL'  'TG_ESTRAT' 'HORA_ATUAL'    'Hora Ataul.'  ' ' ' '  ' ' ' '.

    WHEN 'ADTO_EXT'.
      PERFORM montar_estrutura USING:
         01 'ZSDT0063'     'BUKRS'         'TG_ADTO_EXT' 'BUKRS'         ' '               ' ' 'X' ' ' ' ' ' ',
         01 'ZSDT0063'     'POSNR'         'TG_ADTO_EXT' 'POSNR'         ' '               ' ' 'X' ' ' ' ' ' ',
         01 'ZSDT0063'     'VALDT'         'TG_ADTO_EXT' 'VALDT'         'Dta.Vencimento'  ' ' 'X' ' ' ' ' ' ',
         01 'ZSDT0063'     'DMBTR'         'TG_ADTO_EXT' 'DMBTR'         'Valor'           ' ' 'X' ' ' ' ' ' ',
         01 'ZSDT0063'     'WAERS'         'TG_ADTO_EXT' 'WAERS'         ' '               ' ' 'X' ' ' ' ' ' ',
         01 'ZSDT0063'     'LIFNR'         'TG_ADTO_EXT' 'LIFNR'         ' '               ' ' 'X' ' ' ' ' ' ',
         01 'LFA1'         'NAME1'         'TG_ADTO_EXT' 'NAME1'         ' '               ' ' ' ' ' ' ' ' ' ',
         01 'ZSDT0063'     'ADIANT'        'TG_ADTO_EXT' 'ADIANT'        ' '               ' ' ' ' ' ' ' ' ' ',
         01 'ZSDT0063'     'NR_PROVIS_INV' 'TG_ADTO_EXT' 'NR_PROVIS_INV' 'Nr. provisão'    ' ' 'X' ' ' ' ' ' ',
         01 'BSAD'         'AUGBL'         'TG_ADTO_EXT' 'AUGBL'         ' '               ' ' ' ' ' ' ' ' ' ',
         01 'BSAD'         'AUGDT'         'TG_ADTO_EXT' 'AUGDT'         ' '               ' ' ' ' ' ' ' ' ' '.

    WHEN 'PEDIDO'.
      PERFORM montar_estrutura USING:
        01 'ZSDT0184'    'POSNR'          'TG_PEDIDO' 'POSNR'           'Item'              '04'   'X'   ' '   ' '   ' ',
        02 'ZSDT0184'    'MATNR'          'TG_PEDIDO' 'MATNR'           'Material'          '06'   'X'   ' '   ' '   ' ',
        03 'MAKT'        'MAKTX'          'TG_PEDIDO' 'DESC_MATERIAL'   'Desc.Material'     '15'   'X'   ' '   ' '   ' ',
        03 'ZSDT0184'    'WERKS'          'TG_PEDIDO' 'WERKS'           'Filial'            '04'   'X'   ' '   ' '   ' ',
        04 'T001W'       'NAME1'          'TG_PEDIDO' 'DESC_CENTRO'     'Desc.Filial'       '15'   'X'   ' '   ' '   ' ',
        04 'ZSDT0184'    'PONTO_C'        'TG_PEDIDO' 'PONTO_C'         'PC'                '05'   'X'   ' '   ' '   ' ',
        05 'LFA1'        'NAME1'          'TG_PEDIDO' 'DESC_PC'         'Desc.PC'           '15'   'X'   ' '   ' '   ' ',
        05 'ZSDT0184'    'FORNECEDOR'     'TG_PEDIDO' 'FORNECEDOR'      'Fornecedor'        '07'   'X'   ' '   ' '   ' ',
        06 'LFA1'        'NAME1'          'TG_PEDIDO' 'DESC_FORN'       'Desc.Forn.'        '15'   'X'   ' '   ' '   ' ',
        06 'ZSDT0184'    'LENTREGA'       'TG_PEDIDO' 'LENTREGA'        'LE'                '07'   ' '   ' '   ' '   ' ',
        07 'KNA1'        'NAME1'          'TG_PEDIDO' 'DESC_LE'         'Desc.LE'           '15'   ' '   ' '   ' '   ' ',
        07 'ZSDT0184'    'LGORT'          'TG_PEDIDO' 'LGORT'           'Depósito'          '08'   ' '   ' '   ' '   ' ',
        08 'ZSDT0184'    'CHARG'          'TG_PEDIDO' 'CHARG'           'Lote'              '06'   ' '   ' '   ' '   ' ',
        08 'ZSDT0184'    'CLASSIFICACAO'  'TG_PEDIDO' 'CLASSIFICACAO'   'Classificação'     '08'   ' '   ' '   ' '   ' ',
        09 'ZSDT0184'    'ZMENG'          'TG_PEDIDO' 'ZMENG'           'Quantidade'        '06'   ' '   ' '   ' '   ' ',
        10 'ZSDT0184'    'ZIEME'          'TG_PEDIDO' 'ZIEME'           'Unidade'           '06'   ' '   ' '   ' '   ' ',
        11 'ZSDT0184'    'VOLUM'          'TG_PEDIDO' 'VOLUM'           'Volume'            '06'   ' '   ' '   ' '   ' ',
        12 'ZSDT0184'    'VOLEH'          'TG_PEDIDO' 'VOLEH'           'UN Volume'         '06'   ' '   ' '   ' '   ' ',
        13 'ZSDT0184'    'DMBTR'          'TG_PEDIDO' 'DMBTR'           'Valor UN'          '06'   ' '   ' '   ' '   ' ',
        14 'ZSDT0184'    'PMEIN'          'TG_PEDIDO' 'PMEIN'           'UN Valor'          '06'   ' '   ' '   ' '   ' ',
        15 'ZSDT0184'    'VLRTOT'         'TG_PEDIDO' 'VLRTOT'          'Valor Total'       '06'   ' '   ' '   ' '   ' ',
        16 'ZSDT0184'    'EBELN'          'TG_PEDIDO' 'EBELN'           'Nr.Doc.Compra'     '10'   ' '   ' '   ' '   ' '.


    WHEN 'INSTRUCAO'.
      PERFORM montar_catalog USING:
         01 ' '         ' '                'TG_INSTRUCAO' 'ICON'             'Status'             '2'   ' '  ' '  ' '  ' '  ' '  ' ',
         02 ' '         ' '                'TG_INSTRUCAO' 'INSTRUCAO'        'Instrução'          '50'  'X'  ' '  ' '  ' '  ' '  ' ',
         03 'ZSDT0045'  'CONTRATO'         'TG_INSTRUCAO' 'CONTRATO'         'Contrato'           '20'  'X'  ' '  ' '  ' '  ' '  ' ',
         04 'ZSDT0045'  'BUKRS'            'TG_INSTRUCAO' 'BUKRS'            'Empresa'            ' '   'X'  ' '  ' '  ' '  ' '  'X',
         05 'ZSDT0045'  'WERKS'            'TG_INSTRUCAO' 'WERKS'            'Centro'             ' '   'X'  ' '  ' '  ' '  ' '  'X',
         06 'ZSDT0045'  'MATNR'            'TG_INSTRUCAO' 'MATNR'            'Material'           ' '   'X'  ' '  ' '  ' '  ' '  'X',
         07 ''          ''                 'TG_INSTRUCAO' 'DESC_MAT'         'Desc. Material'     ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         08 'ZSDT0045'  'CHARG'            'TG_INSTRUCAO' 'CHARG'            'Lote'               ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         08 'ZPPT0004'  'TIPO'             'TG_INSTRUCAO' 'TP_FARDO'         'Tipo Fardo'         ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         09 'ZSDT0066'  'VOLUM'            'TG_INSTRUCAO' 'QUANTIDADE'       'Volume'             ' '   'X'  'X'  ' '  ' '  ' '  ' ',
         10 'ZSDT0045'  'VOLEH'            'TG_INSTRUCAO' 'VOLEH'            ' '                  ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         11 'ZSDT0066'  'TERMINAL'         'TG_INSTRUCAO' 'TERMINAL'         'Terminal'           ' '   'X'  ' '  ' '  ' '  ' '  'X',
         12 'ZSDT0045'  'PONTO_C'          'TG_INSTRUCAO' 'PONTO_C'          'Ponto de Coleta'    ' '   'X'  ' '  ' '  ' '  ' '  'X',
         13 'ZSDT0045'  'DMBTR'            'TG_INSTRUCAO' 'DMBTR'            'Montante MI'        ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         14 'ZSDT0045'  'PMEIN'            'TG_INSTRUCAO' 'PMEIN'            ' '                  ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         15 'ZSDT0045'  'BTGEW'            'TG_INSTRUCAO' 'BTGEW'            ' '                  ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         16 'ZSDT0045'  'GEWEI'            'TG_INSTRUCAO' 'GEWEI'            ' '                  ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         17 'ZSDT0045'  'DATA_INSTR'       'TG_INSTRUCAO' 'DATA_INSTR'       'Dt.Instrução'       ' '   'X'  ' '  ' '  ' '  ' '  'X',
         18 'ZSDT0045'  'DATA_RETIRADA'    'TG_INSTRUCAO' 'DATA_RETIRADA'    'Dt.Retirada'        ' '   'X'  ' '  ' '  ' '  ' '  'X',
         19 'ZSDT0045'  'DATA_IN_PORTO'    'TG_INSTRUCAO' 'DATA_IN_PORTO'    'Dt.Inicial Porto'   ' '   'X'  ' '  ' '  ' '  ' '  'X',
         20 'ZSDT0045'  'DATA_PORTO'       'TG_INSTRUCAO' 'DATA_PORTO'       'Dt.Final Porto'     ' '   'X'  ' '  ' '  ' '  ' '  'X',
         21 'ZSDT0045'  'DEADLINE_DRAFT'   'TG_INSTRUCAO' 'DEADLINE_DRAFT'   'Dt.DD. Draft'       ' '   'X'  ' '  ' '  ' '  ' '  'X',
         22 'ZSDT0045'  'DEADLINE_DOCUMEN' 'TG_INSTRUCAO' 'DEADLINE_DOCUMEN' 'Dt.DD. Carga'       ' '   'X'  ' '  ' '  ' '  ' '  'X',
         23 'ZSDT0045'  'PORTO_EMBARQUE'   'TG_INSTRUCAO' 'PORTO_EMBARQUE'   'Porto Embarque'     '20'  'X'  ' '  ' '  ' '  ' '  ' ',
         24 'ZSDT0045'  'SAFRA'            'TG_INSTRUCAO' 'SAFRA'            'Safra'              ' '   'X'  ' '  ' '  ' '  ' '  ' ',
         25 'ZSDT0045'  'OBSERVACAO'       'TG_INSTRUCAO' 'OBSERVACAO'       'Observação'         '50'  'X'  ' '  ' '  ' '  ' '  ' '.

      PERFORM montar_ordem USING:
         1  'INSTRUCAO'        'X' 'X' ' '.

    WHEN 'FORM_LOTE'.
      PERFORM montar_estrutura USING:
         1  'ZSDT0066'  'STATUS'           'TG_FORM_LOTE' 'STATUS'           ' '                 ' '   ' ' ' ' ' ' ' ',
         2  'ZSDT0066'  'POSNR'            'TG_FORM_LOTE' 'POSNR'            ' '                 '3'   ' ' ' ' ' ' ' ',
         3  ' '         ' '                'TG_FORM_LOTE' 'INSTRUCAO'        'Instrução'         '50'  'X' ' ' ' ' ' ',
         5  'ZSDT0066'  'MATNR'            'TG_FORM_LOTE' 'MATNR'            ' '                 '6'   'X' ' ' ' ' ' ',
         6  'MAKT'      'MAKTX'            'TG_FORM_LOTE' 'MAKTX'            ' '                 '15'  ' ' ' ' ' ' ' ',
         7  'ZSDT0066'  'WERKS'            'TG_FORM_LOTE' 'WERKS'            ' '                 '4'   'X' ' ' ' ' ' ',
         8  'ZSDT0066'  'PONTO_C'          'TG_FORM_LOTE' 'PONTO_C'          'Ponto Coleta'      '6'   'X' ' ' ' ' ' ',
         9  'ZSDT0066'  'LGORT'            'TG_FORM_LOTE' 'LGORT'            ' '                 '4'   'X' ' ' ' ' ' ',
         10  'ZSDT0066'  'CHARG'           'TG_FORM_LOTE' 'CHARG'            ' '                '4'   'X' ' ' ' ' ' ',
         11 'VBAK'      'KVGR3'            'TG_FORM_LOTE' 'CLASSIFICACAO'    ' '                 ' '   'X' ' ' ' ' ' ', "C - RR
         12 'ZSDT0066'  'ZMENG'            'TG_FORM_LOTE' 'ZMENG'            ' '                 '13'  'X' ' ' ' ' ' ',
         13 'ZSDT0066'  'ZIEME'            'TG_FORM_LOTE' 'ZIEME'            ' '                 '3'   'X' ' ' ' ' ' ',
         14 'ZSDT0066'  'VOLUM'            'TG_FORM_LOTE' 'VOLUM'            ' '                 '3'   'X' ' ' ' ' ' ',
         15 'ZSDT0066'  'VOLEH'            'TG_FORM_LOTE' 'VOLEH'            ' '                 '3'   'X' ' ' ' ' ' ',
         16 'ZSDT0066'  'DMBTR'            'TG_FORM_LOTE' 'DMBTR'            'Valor'             '13'  'X' ' ' ' ' ' ',
         17 'ZSDT0066'  'PMEIN'            'TG_FORM_LOTE' 'PMEIN'            ' '                 '2'   'X' ' ' ' ' ' ',
         18 'ZSDT0066'  'VLRTOT'           'TG_FORM_LOTE' 'VLRTOT'           ' Valor Total'       '13'  ' ' ' ' ' ' ' ',
         19 'ZSDT0066'  'WAERK'            'TG_FORM_LOTE' 'WAERK'            ' '                 '3'   'X' ' ' ' ' ' ',
         20 'ZSDT0066'  'LIBRA_TO'         'TG_FORM_LOTE' 'LIBRA_TO'         'C.USD LP'          '6'   'X' ' ' ' ' ' ',
         21 'ZSDT0066'  'USD_TO'           'TG_FORM_LOTE' 'USD_TO'           'Vlr.USD TO '       '13'  ' ' ' ' ' ' ' ',
         22 'ZSDT0066'  'VLR_TOT_FRM_USD'  'TG_FORM_LOTE' 'VLR_TOT_FRM_USD'  'Vlr.Tot.Frm.USD'   '13'  ' ' ' ' ' ' ' ',
         23 'ZSDT0066'  'KUNNR'            'TG_FORM_LOTE' 'KUNNR'            ' '                 '6'   'X' ' ' ' ' ' ',
         24 'ZSDT0066'  'TERMINAL'         'TG_FORM_LOTE' 'TERMINAL'         'Terminal/Porto'    '6'   'X' ' ' ' ' ' ',
         25 'ZSDT0066'  'LENTREGA'         'TG_FORM_LOTE' 'LENTREGA'         'Local de Entrega'  '6'   'X' ' ' ' ' ' ',
         26 'ZSDT0066'  'INCO1'            'TG_FORM_LOTE' 'INCO1'            ' '                 '4'   'X' ' ' ' ' ' ',
         27 'ZSDT0066'  'INCO2'            'TG_FORM_LOTE' 'INCO2'            ' '                 '4'   'X' ' ' ' ' ' ',
         28 'ZSDT0066'  'DCO'              'TG_FORM_LOTE' 'DCO'              ' '                 '20'  ' ' ' ' ' ' ' ',
         29 'ZSDT0066'  'AVISO'            'TG_FORM_LOTE' 'AVISO'            'Aviso'             '11'  ' ' ' ' ' ' ' ',
         30 'ZSDT0066'  'VBELN'            'TG_FORM_LOTE' 'VBELN'            ' '                 '5'   ' ' ' ' ' ' ' ',
         31 'VBAK'      'ZTROCANOTA'       'TG_FORM_LOTE' 'ZTROCANOTA'       'Troca Nota?'       ' '   'X' ' ' ' ' ' ',
         32 'ZSDT0066'  'KVGR5'            'TG_FORM_LOTE' 'KVGR5'            'Emite Frete de Entrada?'       ' '   'X' ' ' ' ' ' '.

      PERFORM montar_ordem USING: 3 'INSTRUCAO'     'X' 'X' ' '.

    WHEN 'TL_SAIDA_EXEC'.
      PERFORM montar_estrutura USING:
            1  ' '         ' '                'IT_MSGTEXT'   'TEXTO'           'Retorno'           ' '   ' ' ' ' ' ' ' '.

    WHEN 'POPUP'.
      PERFORM montar_popup USING:
          'SEQ'           'Sequencia'       ' '   ' '  'X' '',
          'NRO_SOL_OV'    'Nr. S.V.'        ' '   ' '  'X' '',
          'FIXACAO'       'Fixação'         ' '   ' '  'X' '',
          'VBELN'         'O.V.'            ' '   'X'  'X' '',
          'AUART'         'Tipo Doc.'       ' '   ' '  'X' '',
          'ZMENG'         'QTD'             ' '   ' '  'X' 'X',
          'ZIEME'         'UN'              ' '   ' '  'X' '',
          'VBELV'         'O.V. Precedente' ' '   ' '  'X' '',
          'USNAM'         'Usuário'         '10'  ' '  ''  '',
          'DATA_ATUAL'    'Data'            '10'  ' '  ''  '',
          'HORA_ATUAL'    'Hora'            '10'  ' '  ''  ''.

    WHEN 'LOTE'.

      DATA: str TYPE REF TO data.
      ASSIGN 'ZSDT0213' TO FIELD-SYMBOL(<fs_str>).
      CREATE DATA str TYPE (<fs_str>).
      DATA(it_fcat) = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

      LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<_fcat>).
        CASE <_fcat>-fieldname.
          WHEN 'LGORT'.
            <_fcat>-outputlen = '10'.
            <_fcat>-reptext   = 'Lote'.
            <_fcat>-f4availabl = abap_true.
            <_fcat>-scrtext_s     = <_fcat>-reptext.
            <_fcat>-scrtext_m     = <_fcat>-reptext.
            <_fcat>-scrtext_l     = <_fcat>-reptext.

            APPEND <_fcat> TO t_fieldcatalog.

          WHEN 'VOLUM'.
            <_fcat>-outputlen = '20'.
*            <_FCAT>-EDIT = ABAP_TRUE.
            APPEND <_fcat> TO t_fieldcatalog.
        ENDCASE.
      ENDLOOP.

*      PERFORM MONTAR_ESTRUTURA USING:
*          01 'ZSDT0213' 'LGORT' 'IT_0213' 'LGORT' 'Lote'   '10' 'X' ''  ''  '',
*          02 'ZSDT0213' 'VOLUM' 'IT_0213' 'VOLUM' 'Volume' '50' 'X' ''  ''  ''.
*          01 '' '' '' 'LGORT' '_____Lote_____'   '10' 'X' ''  ''  '',
*          02 '' '' '' 'VOLUM' '_____Volume_____' '50' 'X' ''  ''  ''.

  ENDCASE.

  IF p_alv EQ 'ITENS'.
  ELSEIF p_alv EQ 'PGT_ANT'.
  ELSEIF p_alv EQ 'LOGISTICA'.
  ELSEIF p_alv EQ 'PRECO'.
  ELSEIF p_alv EQ 'PRECO_FRAME'.
  ELSEIF p_alv EQ 'PRECO_FRAME2'.
  ELSEIF p_alv EQ 'SALDO'.
  ELSEIF p_alv EQ 'COND_ESP'.
  ELSEIF p_alv EQ 'SAIDA_EXEC'.
  ELSEIF p_alv EQ 'MOTIVO'.
  ELSEIF p_alv EQ 'ADTO_EXT'.
  ELSEIF p_alv EQ 'INSTRUCAO'.
  ELSEIF p_alv EQ 'FORM_LOTE'.
  ELSEIF p_alv EQ 'TL_SAIDA_EXEC'. "Monta ALV com resultados da execução do SHDB
  ELSEIF p_alv EQ 'POPUP'.
  ENDIF.

  LOOP AT t_fieldcatalog ASSIGNING <_fcat>.
    IF line_exists( it_0235[ tabela = 'ZSDT0053' campo = <_fcat>-fieldname ] ).

      IF <_fcat>-fieldname EQ 'MATNR'.
        IF line_exists( t_fieldcatalog[ fieldname = 'MAKTX' ] ).
          t_fieldcatalog[ fieldname = 'MAKTX' ]-no_out = abap_true.
        ENDIF.
      ENDIF.

      <_fcat>-no_out = abap_true.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
*      -->VALUE(P_F4)             text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_zero).

  CLEAR w_fieldcatalog.

*  IF WG_HEADER-PARAM_ESPEC EQ C_A.
  CASE p_field.
    WHEN 'INSTRUCAO_ANT' OR 'INSTRUCAO' OR 'TERMINAL'. "Retirado Field "NAVIO" "PORTO"- US 73163 - Fim
      w_fieldcatalog-f4availabl    = abap_true.
  ENDCASE.
*  ENDIF.

  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = abap_false.

  IF wg_display IS INITIAL.
    w_fieldcatalog-edit          = p_edit.
  ENDIF.

  w_fieldcatalog-do_sum        = p_sum.
  w_fieldcatalog-col_pos         = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.

  w_fieldcatalog-no_out        = abap_false.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.

  CASE p_field.
    WHEN 'CHARG' OR 'LGORT' OR 'PMEIN' OR 'ZIEME'. "Retirado Field "NAVIO" "PORTO"- US 73163 - Fim
      w_fieldcatalog-f4availabl = abap_true.
    WHEN 'FORMULA2'.
      w_fieldcatalog-just = 'R'.
    WHEN 'NIVEL'.
      w_fieldcatalog-no_zero = abap_true.
      w_fieldcatalog-just    = 'C'.
    WHEN 'TERMINAL'.
      w_fieldcatalog-no_zero = abap_true.
    WHEN 'VBELN' OR 'ADIANT'.
      w_fieldcatalog-hotspot = abap_true.
    WHEN 'VLRTOT' OR 'ZMENG' OR 'VOLUM' OR 'QUANTIDADE' OR 'CADENCIA_QTE' OR 'VLT_PORTO'.
      w_fieldcatalog-do_sum = abap_true.
    WHEN 'LOTES_VOL'.
      w_fieldcatalog-just = 'C'.
  ENDCASE.

  CONDENSE p_tabname NO-GAPS.
  IF p_tabname EQ 'TG_ADTO_EXT'
  OR p_tabname EQ 'TG_PGT_ANT'
  AND p_field   EQ 'DMBTR'.
    w_fieldcatalog-do_sum = abap_true.
  ELSEIF p_tabname EQ 'TG_INSTRUCAO'
    AND p_field EQ 'ZSEQ_INST'.
    w_fieldcatalog-f4availabl = abap_true.
  ELSEIF p_tabname EQ 'TG_FORM_LOTE'
  AND p_field EQ 'INSTRUCAO'.
    w_fieldcatalog-f4availabl = abap_true.
  ELSEIF p_tabname EQ 'TG_ITENS'
  AND ( p_field EQ 'INSTRUCAO'
     OR p_field EQ 'TERMINAL'
    OR p_field EQ 'PMEIN' " 18.03.2025
    ).
    w_fieldcatalog-f4availabl = abap_true.
  ENDIF.

  IF ( p_tabname EQ 'TG_PEDIDO' ).

    CASE p_field.
      WHEN 'MATNR'         OR 'WERKS'    OR
           'PONTO_C'       OR 'LGORT'    OR 'CHARG'  OR
           'CLASSIFICACAO' OR 'USD_TO'   OR 'ZMENG'  OR
           'ZIEME'         OR 'VOLUM'    OR 'VOLEH'  OR
           'DMBTR'         OR 'PMEIN'    OR 'VLRTOT' OR
           'WAERK'         OR 'LIBRA_TO' OR 'VLR_TOT_FRM_USD' OR
           'KUNNR'         OR 'TERMINAL' OR 'LENTREGA' OR
           'INCO1'         OR 'INCO2'    OR 'DCO'    OR
           'AVISO'         OR 'VBELN'.

        w_fieldcatalog-edit = 'X'.

    ENDCASE.
  ENDIF.

  w_fieldcatalog-drdn_hndl = SWITCH #( p_field WHEN 'KVGR3' THEN 2
*                                               WHEN 'KVGR5' THEN 3
                                               ELSE 0 ).

  w_fieldcatalog-checkbox = SWITCH #( p_field WHEN 'CK_TROCA_NOTA' THEN abap_true
                                               ELSE abap_false ).

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                     " montar_estrutura

*&---------------------------------------------------------------------*
*&      Form  MONTAR_CATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_COL_POS)        text
*      -->VALUE(P_REF_TABNAME)    text
*      -->VALUE(P_REF_FIELDNAME)  text
*      -->VALUE(P_TABNAME)        text
*      -->VALUE(P_FIELD)          text
*      -->VALUE(P_SCRTEXT_L)      text
*      -->VALUE(P_OUTPUTLEN)      text
*      -->VALUE(P_EDIT)           text
*      -->VALUE(P_SUM)            text
*      -->VALUE(P_EMPHASIZE)      text
*      -->VALUE(P_KEY)            text
*      -->VALUE(P_HOSTSPOT)       text
*      -->VALUE(P_F4AVAILABL)     text
*----------------------------------------------------------------------*
FORM montar_catalog USING VALUE(p_col_pos)       TYPE i
                          VALUE(p_ref_tabname)   LIKE dd02d-tabname
                          VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                          VALUE(p_tabname)       LIKE dd02d-tabname
                          VALUE(p_field)         LIKE dd03d-fieldname
                          VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                          VALUE(p_outputlen)
                          VALUE(p_edit)
                          VALUE(p_sum)
                          VALUE(p_emphasize)
                          VALUE(p_key)
                          VALUE(p_hostspot)
                          VALUE(p_f4availabl).
  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = p_key.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.
  w_fieldcatalog-col_pos       = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.

*  W_FIELDCATALOG-NO_OUT        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.
*    W_FIELDCATALOG-JUST = 'R'.
  w_fieldcatalog-hotspot = p_hostspot.
  w_fieldcatalog-f4availabl = p_f4availabl.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    "MONTAR_CATALOG

*&---------------------------------------------------------------------*
*&      Form  MONTAR_POPUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO    text
*      -->P_DESC     text
*      -->P_TAM      text
*      -->P_HOT      text
*      -->P_OPT      text
*      -->P_SUM      text
*----------------------------------------------------------------------*
FORM montar_popup USING    p_campo TYPE c
                           p_desc  TYPE c
                           p_tam   TYPE c
                           p_hot   TYPE c
                           p_opt   TYPE c
                           p_sum   TYPE c
                   .

  wa_fcat1-fieldname = p_campo.
  wa_fcat1-scrtext_s = p_desc.
  wa_fcat1-outputlen = p_tam.
  wa_fcat1-hotspot   = p_hot.
  wa_fcat1-col_opt   = p_opt.
  wa_fcat1-do_sum    = p_sum.

  APPEND wa_fcat1 TO it_fcat1.

ENDFORM.                    "MONTAR_POPUP

*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_variavel USING p_acao.

  DATA: wl_nro_sol_ov TYPE zsdt0051-nro_sol_ov,
        clear_tabix   TYPE sy-tabix.

  CLEAR: wl_nro_sol_ov.

*  Limpa a variavel para acessar atribuir novos valores para o ZMENG
  IF NOT ( var_modred IS INITIAL ).

    IF tg_msg_ret[] IS INITIAL AND ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z' ) .
      REFRESH: it_var_guar, it_block.
    ENDIF.

    IF p_acao EQ 'CANCEL'.
      REFRESH: it_var_guar, it_block, style.
      CLEAR wa_style.

      LOOP AT tg_itens.
        clear_tabix = sy-tabix.
        CLEAR: tg_itens-style.
        LOOP AT t_fieldcatalog INTO w_fieldcatalog.
          wa_style-fieldname = w_fieldcatalog-fieldname.
          wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT wa_style INTO TABLE style.
        ENDLOOP.
        INSERT LINES OF style INTO TABLE tg_itens-style.
        MODIFY tg_itens INDEX clear_tabix TRANSPORTING style.
      ENDLOOP.

    ENDIF.

  ENDIF.





  IF p_acao EQ c_cancel
   OR p_acao EQ c_atual.
    wl_nro_sol_ov = wg_header-nro_sol_ov.

    CLEAR: wg_header,
           wg_cond_pgt,
           wg_posicao,
           wg_acao,
           wg_flag,
           x_field,
           wg_cell,
           wg_status(4),
           wg_desc_kunnr,
           wg_desc_status,
           wg_desc_correto,
           wg_desc_auart,
           wg_desc_vkorg,
           wg_desc_vtweg,
           wg_desc_spart,
           wg_desc_vkgrp,
           wg_desc_vkbur,
           wg_desc_zlsch,
           wg_desc_zterm,
           wg_desc_hbkid,
           wg_desc_tp_venda,
           wg_desc_matnr,
           wg_hora,
           wg_data,
           wg_redistribuir,
           wg_redistribuir_saldo,
           wg_redistribuir_total,
           wg_redistribuir_titem,
           tg_logistica,
           tg_pedido,
           modred_hedge,
           block_botao.

    REFRESH: tg_itens,
             tg_msg_ret,
             tg_pgt_ant,
             tg_logistica,
             tg_preco,
             tg_bsad,
             tg_historico,
             tg_adto_ext,
             tg_instrucao,
             tg_form_lote,
             tg_pedido,
             tg_cond_esp,
             tg_fix_red,
             tg_motivo,
             tg_msg_ret,
             tg_preco_n,
             it_var_guar,
             it_block,
             r_bezei,
             p_bezei,
             c_bezei.


    IF <fs_table> IS ASSIGNED.
      REFRESH: <fs_table>.
    ENDIF.
*             style,
*             tg_itens-style,
*             tg_mglobal.
*
*    PB_OBS = '@6Y@ Observação'.
*    PB_COMENT_LOGISTICA = '@6Y@ Comt. Logistica'.
    CONCATENATE '@6Y@' TEXT-b06 INTO pb_obs.
    CONCATENATE '@6Y@' TEXT-b07 INTO pb_coment_logistica.
    wg_header-nro_sol_ov = wl_nro_sol_ov.
  ELSEIF p_acao EQ c_add.
    CLEAR: wg_header,
           wg_cond_pgt,
           wg_posicao,
           tg_itens,
           tg_pedido,
           wg_acao,
           x_field,
           tg_msg_ret,
           wg_status(4),
           wg_desc_kunnr,
           wg_desc_status,
           wg_desc_correto,
           wg_desc_auart,
           wg_desc_vkorg,
           wg_desc_vtweg,
           wg_desc_spart,
           wg_desc_vkgrp,
           wg_desc_vkbur,
           wg_desc_zlsch,
           wg_desc_zterm,
           wg_desc_hbkid,
           wg_desc_tp_venda,
           wg_desc_matnr,
           wg_hora,
           wg_data,
           wg_redistribuir,
           wg_redistribuir_saldo,
           wg_redistribuir_total,
           wg_redistribuir_titem,
           tg_logistica.

    REFRESH: tg_itens,
             tg_pgt_ant,
             tg_logistica,
             tg_preco,
             tg_bsad,
             tg_historico,
             tg_adto_ext,
             tg_instrucao,
             tg_form_lote,
             tg_pedido,
             tg_cond_esp,
             tg_fix_red,
             tg_motivo,
             tg_msg_ret,
             tg_preco_n.
*             style,
*             tg_itens-style,
*             tg_mglobal.
    IF <fs_table> IS ASSIGNED.
      REFRESH: <fs_table>.
    ENDIF.

*    PB_OBS = '@6Y@ Observação'.
*    PB_COMENT_LOGISTICA = '@6Y@ Comt. Logistica'.
    CONCATENATE '@6Y@' TEXT-b06 INTO pb_obs.
    CONCATENATE '@6Y@' TEXT-b07 INTO pb_coment_logistica.

  ELSEIF p_acao EQ c_dele.
*
    CLEAR: wg_header,
           wg_cond_pgt,
           wg_posicao,
           tg_itens,
           tg_pedido,
           wg_acao,
*           wg_flag,
           x_field,
           tg_msg_ret,
           wg_cell,
           wg_status(4),
           wg_desc_status,
           wg_desc_kunnr,
           wg_desc_correto,
           wg_desc_auart,
           wg_desc_vkorg,
           wg_desc_vtweg,
           wg_desc_spart,
           wg_desc_vkgrp,
           wg_desc_vkbur,
           wg_desc_zlsch,
           wg_desc_zterm,
           wg_desc_hbkid,
           wg_desc_tp_venda,
           wg_desc_matnr,
           wg_hora,
           wg_data,
           wg_redistribuir,
           wg_redistribuir_saldo,
           wg_redistribuir_total,
           wg_redistribuir_titem,
           tg_logistica.
*
    REFRESH: tg_itens,
             tg_pgt_ant,
             tg_logistica,
             tg_preco,
             tg_bsad,
             tg_historico,
             tg_adto_ext,
             tg_instrucao,
             tg_form_lote,
             tg_pedido,
             tg_cond_esp,
             tg_fix_red,
             tg_motivo,
             tg_msg_ret,
             tg_preco_n.
*             style,
*             tg_itens-style,
*             tg_mglobal.

    IF <fs_table> IS ASSIGNED.
      REFRESH: <fs_table>.
    ENDIF.
*
*    PB_OBS = '@6Y@ Observação'.
*    PB_COMENT_LOGISTICA = '@6Y@ Comt. Logistica'.
    CONCATENATE '@6Y@' TEXT-b06 INTO pb_obs.
    CONCATENATE '@6Y@' TEXT-b07 INTO pb_coment_logistica.

  ELSEIF p_acao EQ c_modif_qtd.
    wl_nro_sol_ov = wg_header-nro_sol_ov.
*
    CLEAR: wg_header,
           wg_cond_pgt,
           wg_posicao,
           tg_itens,
           wg_acao,
           wg_flag,
           x_field,
           tg_msg_ret,
           wg_cell,
           wg_status(4),
           wg_desc_kunnr,
           wg_desc_status,
           wg_desc_correto,
           wg_desc_auart,
           wg_desc_vkorg,
           wg_desc_vtweg,
           wg_desc_spart,
           wg_desc_vkgrp,
           wg_desc_vkbur,
           wg_desc_zlsch,
           wg_desc_zterm,
           wg_desc_hbkid,
           wg_desc_tp_venda,
           wg_desc_matnr,
           wg_hora,
           wg_data.
*           WG_REDISTRIBUIR.
*
    REFRESH: tg_itens,
             tg_pgt_ant,
             tg_logistica,
             tg_preco,
             tg_bsad,
             tg_historico,
             tg_adto_ext,
             tg_instrucao,
             tg_form_lote,
             tg_pedido,
             tg_cond_esp,
             tg_fix_red,
             tg_motivo,
             tg_msg_ret,
             tg_preco_n.
*             style,
*             tg_itens-style,
*             tg_mglobal.
*
    IF <fs_table> IS ASSIGNED.
      REFRESH: <fs_table>.
    ENDIF.
*    PB_OBS = '@6Y@ Observação'.
*    PB_COMENT_LOGISTICA = '@6Y@ Comt. Logistica'.
    CONCATENATE '@6Y@' TEXT-b06 INTO pb_obs.
    CONCATENATE '@6Y@' TEXT-b07 INTO pb_coment_logistica.


    wg_header-nro_sol_ov = wl_nro_sol_ov.
*    clear: tg_itens.
*    refresh: tg_itens, tg_itens-style.
  ENDIF.

ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  DATA:"" VALUES       TYPE VRM_VALUES WITH HEADER LINE,
    tl_tvlv  TYPE TABLE OF tvlv WITH HEADER LINE,
    tl_tvlvt TYPE TABLE OF tvlvt WITH HEADER LINE,
    wl_t052  TYPE t052,
    wl_0075  TYPE zsdt0075.
  "wl_setleaf TYPE setleaf. " " 29.10.2024 - 147331 - RAMON -
*  FIELD-SYMBOLS: <fs_campo> TYPE ANY.

  CHECK lcl_busca_tela=>exit_tela( sy-ucomm ) IS INITIAL.

  REFRESH: tl_tvlv, tl_tvlvt, values.

  IF init IS INITIAL.
    SELECT *
      FROM tvlv
      INTO TABLE tl_tvlv.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM tvlvt
         INTO TABLE tl_tvlvt
         FOR ALL ENTRIES IN tl_tvlv
          WHERE abrvw EQ tl_tvlv-abrvw
            AND spras EQ sy-langu.

    ENDIF.

    LOOP AT tl_tvlv.
      READ TABLE tl_tvlvt
        WITH KEY abrvw = tl_tvlv-abrvw.
      IF sy-subrc IS INITIAL.

        values-text = tl_tvlvt-bezei.
        values-key  = tl_tvlv-abrvw.
        APPEND values.
      ENDIF.
    ENDLOOP.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'WG_HEADER-VKAUS'
        values          = values[]
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    REFRESH: values.
    CLEAR: values.
*
*    IF ( WG_HEADER-PARAM_ESPEC EQ SPACE
*    OR WG_HEADER-PARAM_ESPEC EQ C_A
*    OR WG_HEADER-PARAM_ESPEC EQ C_M )
*    AND ( WG_HEADER-AUART NE 'ZEXP'
*      AND WG_HEADER-AUART NE 'ZEXI' ).
*
*      SELECT *
*          FROM ZSDT0064
*          INTO TABLE TL_0064
*           WHERE UNAME EQ SY-UNAME
*             AND SOLIC EQ 'X'.
*
*      IF SY-SUBRC IS INITIAL.
**      READ TABLE TL_0064
**        WITH KEY WERKS = WG_HEADER-VKBUR.
*
*        CALL SCREEN 201 ENDING AT 51 9 STARTING AT 3 3.
*      ENDIF.
*    ENDIF.
    init = c_x.

  ENDIF.

** Controle de campos de tela
  " 19.12.2023 - 128467 -	RBL -->
  IF wl_t052-zterm <> wg_cond_pgt-zterm.

    CLEAR: wl_t052.

    SELECT SINGLE *
      FROM t052
       INTO wl_t052
       WHERE zterm EQ wg_cond_pgt-zterm.

  ENDIF.

  IF wl_0075-kunnr <> wg_header-kunnr
    OR wl_0075-vkorg <> wg_header-vkorg.

    CLEAR: wl_0075.

    SELECT SINGLE *
     FROM zsdt0075
     INTO wl_0075
      WHERE kunnr EQ wg_header-kunnr
        AND vkorg EQ wg_header-vkorg.

    IF sy-subrc IS NOT INITIAL.
      SELECT SINGLE *
        FROM zsdt0075
        INTO wl_0075
         WHERE kunnr EQ wg_header-kunnr
           AND vkorg EQ space.

    ENDIF.

  ENDIF.

****** CLEAR: wl_t052.
******  SELECT SINGLE *
******    FROM t052
******     INTO wl_t052
******     WHERE zterm EQ wg_cond_pgt-zterm.
******
******  CLEAR: wl_0075.
******  SELECT SINGLE *
******   FROM zsdt0075
******   INTO wl_0075
******    WHERE kunnr EQ wg_header-kunnr
******      AND vkorg EQ wg_header-vkorg.
******
******  IF sy-subrc IS NOT INITIAL.
******    SELECT SINGLE *
******      FROM zsdt0075
******      INTO wl_0075
******       WHERE kunnr EQ wg_header-kunnr
******         AND vkorg EQ space.
******
******  ENDIF.

  " 08.02.2024 - RAMON -->
*  IF wg_cond_pgt-zterm IS NOT INITIAL.
*    wg_cond_pgt-qte_venc = '01'.
*  ENDIF.
  " 08.02.2024 - RAMON --<
  " 19.12.2023 - 128467 -	RBL --<



  IF wg_acao EQ c_add
  OR wg_acao EQ c_modif.
    LOOP AT SCREEN.
      IF screen-group2 EQ 'A1'.
        IF screen-name EQ 'WG_COND_PGT-QTE_VENC'
        OR screen-name EQ 'WG_COND_PGT-ZTERM'
        OR screen-name EQ 'WG_COND_PGT-ZLSCH'
        OR screen-name EQ 'WG_COND_PGT-HBKID'
        OR screen-name EQ 'WG_HEADER-DATA_VENDA'
        OR screen-name EQ 'TAB_STRIP_TAB4'
        OR screen-name EQ 'TAB_STRIP_TAB5'
        OR screen-name EQ 'TAB_STRIP_TAB6'
        OR screen-name EQ 'TAB_STRIP_TAB7'
        OR screen-name EQ 'TAB_STRIP_TAB8'
        OR screen-name EQ 'TAB_STRIP_TAB9'
        OR screen-name EQ 'WG_HEADER-NUM_FIXACAO'
        OR screen-name EQ 'NUM_FIXACAO_TXT'.
          IF  screen-name EQ 'WG_COND_PGT-QTE_VENC'.
*            IF WL_T052-ZTAGG IS NOT INITIAL
            IF wl_t052-zlsch EQ 'V'.
              screen-input = 1.
            ELSE.
              CLEAR: wg_cond_pgt-qte_venc.
              screen-input = 0.
            ENDIF.
*          ELSEIF SCREEN-NAME EQ 'WG_HEADER-VKBUR'.
*            IF ( WG_HEADER-PARAM_ESPEC EQ SPACE
*                OR WG_HEADER-PARAM_ESPEC EQ C_A
*                OR WG_HEADER-PARAM_ESPEC EQ C_M )
*                AND ( WG_HEADER-AUART NE 'ZEXP'
*                  AND WG_HEADER-AUART NE 'ZEXI' ).
*
*              SELECT *
*                  FROM ZSDT0064
*                  INTO TABLE TL_0064
*                   WHERE UNAME EQ SY-UNAME.
*
*              IF SY-SUBRC IS INITIAL.
*                SCREEN-INPUT = 0.
*              ELSE.
*                SCREEN-INPUT = 1.
*              ENDIF.
*            ELSE.
*              SCREEN-INPUT = 1.
*            ENDIF.

          ELSEIF screen-name EQ 'WG_COND_PGT-ZLSCH'
              OR screen-name EQ 'WG_COND_PGT-HBKID'.
            IF wl_0075 IS INITIAL.
*              SCREEN-INPUT = 0.
              screen-input = 1.
              IF wg_cond_pgt-pgto_ant IS INITIAL.
*                WG_COND_PGT-ZLSCH = c_D.
              ELSE.
*                WG_COND_PGT-ZLSCH = C_P.
              ENDIF.
*              WG_COND_PGT-HBKID = 'BBRA'.
            ELSE.
              screen-input = 1.
*              IF WG_ACAO EQ C_ADD.
*                CLEAR: WG_COND_PGT-ZLSCH, WG_COND_PGT-HBKID.
*              ENDIF.
            ENDIF.
          ELSEIF screen-name EQ 'WG_COND_PGT-ZTERM'.
            IF   wg_header-param_espec EQ c_m. "OR wg_header-param_espec EQ c_z .
              screen-input = 0.
              CLEAR: wg_cond_pgt-zterm.
            ELSE.
              screen-input = 1.
            ENDIF.

          ELSEIF screen-name EQ 'TAB_STRIP_TAB4'.
            IF wg_cond_pgt-pgto_ant IS NOT INITIAL.
              screen-active = 1.
            ELSE.
              REFRESH: tg_pgt_ant.
              screen-active = 0.
            ENDIF.
          ELSEIF screen-name EQ 'TAB_STRIP_TAB6'.
            IF wg_header-param_espec EQ c_p OR
               wg_header-param_espec EQ c_a OR "*-CS2022000332-#78223-02.06.2022-JT-inicio
              wg_header-param_espec EQ c_z .   " CS2020000373 23/08/2022 -LP
              screen-active = 1.
            ELSE.
              REFRESH: tg_adto_ext.
              screen-active = 0.
            ENDIF.
          ELSEIF screen-name EQ 'TAB_STRIP_TAB5'.

            IF ( wg_header-inco1 EQ 'FOB'
            OR   wg_header-inco1 EQ 'FAS' )
            AND  wg_header-risco_sacado EQ 'N'.
              screen-active = 0.
              REFRESH: tg_logistica.
              IF g_tab_strip-pressed_tab EQ c_tab_strip-tab5.
                CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
                  EXPORTING
                    functioncode           = 'TAB_STRIP_FC1'
                  EXCEPTIONS
                    function_not_supported = 1.
              ENDIF.

            ELSE.
              screen-active = 1.
            ENDIF.

          ELSEIF screen-name EQ 'TAB_STRIP_TAB7'.

            IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
               wg_header-param_espec EQ c_ax OR
               wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
              screen-active = 1.
            ELSE.
*              REFRESH: TG_INSTRUCAO, TG_FORM_LOTE.
              screen-active = 0.
            ENDIF.

          ELSEIF screen-name EQ 'TAB_STRIP_TAB9'.

            IF wg_header-param_espec EQ c_u.
              screen-active = 1.
            ELSE.
              screen-active = 0.
            ENDIF.

          ELSEIF screen-name EQ 'WG_HEADER-NUM_FIXACAO' OR  screen-name EQ 'NUM_FIXACAO_TXT'.


*            IF WG_HEADER-PARAM_ESPEC NE C_M.
*              CLEAR: WG_HEADER-NUM_FIXACAO.
*              REFRESH: TG_PRECO_FRAME.
*              SCREEN-ACTIVE = 0.
*              SCREEN-INVISIBLE = 1.
*            ELSE.
*              SCREEN-ACTIVE = 1.
*              SCREEN-INPUT = 1.
*              SCREEN-INVISIBLE = 0.
*            ENDIF.


            IF wg_header-param_espec NE c_m AND wg_header-param_espec NE c_z .
              CLEAR: wg_header-num_fixacao.
              REFRESH: tg_preco_frame.
              screen-active = 0.
              screen-invisible = 1.
            ELSE.
              IF ( screen-name EQ 'WG_HEADER-NUM_FIXACAO' ) AND ( wg_acao NE c_modif ).
                screen-active = 1.
                screen-input = 1.
                screen-invisible = 0.
              ENDIF.
            ENDIF.
          ELSEIF screen-name EQ 'WG_HEADER-DATA_VENDA'.
            IF wg_acao EQ c_modif.
*              SELECT SINGLE *
*                FROM SETLEAF
*                INTO WL_SETLEAF
*                 WHERE SETNAME EQ 'MAGGI_DATA_SOLICIT_VENDA'
*                  AND VALFROM  EQ SY-UNAME.
*              IF SY-SUBRC IS INITIAL.
*                SCREEN-INPUT = 1.
*              ELSE.
*                SCREEN-INPUT = 0.
*              ENDIF.
            ELSEIF wg_acao EQ c_add.
              screen-input = 1.
            ENDIF.
          ENDIF.
        ELSE.
*-IR065241 - 06.07.2021 - JT - inicio
          IF screen-name = 'WG_HEADER-RISCO_SACADO'.
            screen-input    = 1.
            screen-required = 1.
          ELSE.
            screen-input = 1.
          ENDIF.
*-IR065241 - 06.07.2021 - JT - fim
        ENDIF.
        MODIFY SCREEN.
        screen-required = 0.
      ENDIF.

      " 18.12.2023 - 128467 - Campo porto obrigatorio mas nao aqui, vai ser dinamico -->

*      IF screen-name = 'WG_HEADER-PORTO' AND wg_header-param_espec EQ c_z . "USER STORY 83960 - CAMPO PORTO
*        screen-input    = 1.
*        screen-required = 1.
*        MODIFY SCREEN.
*
*      ENDIF.

      " 18.12.2023 - 128467 - Campo porto obrigatorio mas nao aqui, vai ser dinamico --<

      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group2 EQ 'A4'.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'TAB_STRIP_TAB8'.
        IF wg_header-param_espec EQ c_f
        OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
        OR wg_header-param_espec EQ c_ax
           OR wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
          screen-active = 1.
          screen-invisible = 0.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
         wg_header-param_espec EQ c_ax .
        "  wg_header-param_espec EQ c_z ." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
        CASE screen-name.
          WHEN 'WG_HEADER-VKORG' OR
*         'WG_HEADER-VKGRP       ' OR
         'WG_HEADER-DATA_VENDA  ' OR
         'WG_HEADER-CORRETO     ' OR
         'WG_HEADER-KUNNR       ' OR
         'WG_HEADER-MATNR       ' OR
         'WG_HEADER-DTDE_LOGIST ' OR
         'WG_HEADER-DTATE_LOGIST'.
            screen-input = 0.
            MODIFY SCREEN.
        ENDCASE.
      ENDIF.

*   19.06.2018 - Pedido - U
      IF ( wg_header-param_espec EQ c_u ).
        CASE screen-name.
          WHEN  'TAB_STRIP_TAB2' OR
                'TAB_STRIP_TAB4' OR
                'TAB_STRIP_TAB5' OR
                'TAB_STRIP_TAB6' OR
                'TAB_STRIP_TAB7' OR
                'TAB_STRIP_TAB8'.
            screen-input  = 0.
            screen-active = 0.
            MODIFY SCREEN.
          WHEN 'TAB_STRIP_TAB9' OR 'TAB_STRIP_TAB3'.
            screen-input  = 1.
            screen-active = 1.
            MODIFY SCREEN.
        ENDCASE.
      ENDIF.

*-CS2021000615 - 17.06.2021 - JT - inicio
      IF screen-name EQ 'TAB_STRIP_TAB8'.
        IF wg_header-param_espec = c_ax.
          screen-input  = 0.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*-CS2021000615 - 17.06.2021 - JT - fim

*-CS2022000332-#78223 -02.06.2022-JT-inicio
      IF wg_header-param_espec = c_a OR
         wg_header-param_espec = c_x OR
        wg_header-param_espec = c_z ." CS2020000373 23/08/2022 -LP
        IF screen-name = 'WG_HEADER-BSTKD'.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'WG_HEADER-ID_CONTRATO'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-name = 'WG_HEADER-BSTKD'.
          screen-input = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'WG_HEADER-ID_CONTRATO'.
          CLEAR wg_header-id_contrato.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
*-CS2022000332-#78223 -02.06.2022-JT-fim

    ENDLOOP.

  ELSEIF wg_acao EQ c_modif_qtd.  " Modificar Quantidade

    LOOP AT  SCREEN.
      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group2 EQ 'A4'.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF wg_acao EQ c_modif_c_ov. " Modificar com OV

    LOOP AT  SCREEN.
      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 EQ 'CP'.
        IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
          screen-input = 1.
        ELSE.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'WG_COND_PGT-QTE_VENC'
      OR screen-name EQ 'WG_COND_PGT-ZTERM'.
        IF ( wg_header-param_espec EQ c_m ). "OR  wg_header-param_espec EQ c_z ). " 08.02.2024 - RAMON - COND PAG
          screen-input = 0.
          CLEAR: wg_cond_pgt-zterm.
        ELSE.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF  screen-name EQ 'WG_COND_PGT-ZTERM'.
        IF ( wg_header-param_espec EQ c_m ). "OR  wg_header-param_espec EQ c_z ). " 08.02.2024 - RAMON - COND PAG
          screen-input = 0.
          CLEAR: wg_cond_pgt-zterm.
        ELSE.
          screen-input = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'TAB_STRIP_TAB4'.
        IF wg_cond_pgt-pgto_ant IS NOT INITIAL.
          screen-active = 1.
        ELSE.
          REFRESH: tg_pgt_ant.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'TAB_STRIP_TAB6'.
        IF wg_header-param_espec EQ c_p OR
*-CS2022000332-#78223-02.06.2022-JT-inicio
           wg_header-param_espec EQ c_a OR
          wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2022000332-#78223-02.06.2022-JT-fim
          screen-active = 1.
        ELSE.
          REFRESH: tg_adto_ext.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'TAB_STRIP_TAB9'.
        IF wg_header-param_espec EQ c_u.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'TAB_STRIP_TAB5'.
        IF ( wg_header-inco1 EQ 'FOB'
        OR   wg_header-inco1 EQ 'FAS' )
        AND  wg_header-risco_sacado EQ 'N'.
          screen-active = 0.
          REFRESH: tg_logistica.
          IF g_tab_strip-pressed_tab EQ c_tab_strip-tab5.
*                G_TAB_STRIP-PRESSED_TAB = C_TAB_STRIP-TAB1.
            CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
              EXPORTING
                functioncode           = 'TAB_STRIP_FC1'
              EXCEPTIONS
                function_not_supported = 1.
          ENDIF.
        ELSE.

          screen-active = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'TAB_STRIP_TAB7'.
        IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
           wg_header-param_espec EQ c_ax OR
          wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
          screen-active = 1.
        ELSE.
*          REFRESH: TG_INSTRUCAO, TG_FORM_LOTE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name EQ 'WG_HEADER-NUM_FIXACAO'
         OR  screen-name EQ 'NUM_FIXACAO_TXT'.
        IF  wg_header-param_espec NE c_m  AND wg_header-param_espec NE c_z .
          CLEAR: wg_header-num_fixacao.
          REFRESH: tg_preco_frame.
          screen-active = 0.
          screen-invisible = 1.
        ELSE.
*          As linhas foram comentadas pq numero de Fixações não
*          pode ser aberto para Edição quando conter OV ou Linha com status 'F'
*          SCREEN-ACTIVE = 1.
*          SCREEN-INPUT = 1.
*          SCREEN-INVISIBLE = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'TAB_STRIP_TAB8'.
        IF wg_header-param_espec EQ c_f
        OR wg_header-param_espec EQ c_a
          OR wg_header-param_espec EQ c_z. " CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - inicio
*       OR wg_header-param_espec EQ c_ax.
*-CS2021000615 - 17.06.2021 - JT - fim
          screen-active = 1.
          screen-invisible = 0.
        ELSE.
          screen-active = 0.
          screen-invisible = 1.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF wg_acao NE c_add
     AND wg_acao NE c_modif.

    LOOP AT  SCREEN.
      IF screen-group2 EQ 'A4'.
        IF ( wg_header-param_espec EQ space
                OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
                OR wg_header-param_espec EQ c_ax
*-CS2021000615 - 17.06.2021 - JT - fim
                OR wg_header-param_espec EQ c_m
                OR wg_header-param_espec EQ c_z )" CS2020000373 23/08/2022 -LP
                AND ( wg_header-auart NE 'ZEXP'
                  AND wg_header-auart NE 'ZEXI' ).

*   "// PBI-59125 Melhoria de perfomace Inicio
          READ TABLE tl_0064 WITH KEY uname = sy-uname TRANSPORTING NO FIELDS.
*          SELECT *
*              FROM zsdt0064
*              INTO TABLE tl_0064
*               WHERE uname EQ sy-uname.
*   "// PBI-59125 Melhoria de perfomace Fim

          IF sy-subrc IS INITIAL.
            screen-invisible  = 0.

          ELSE.
            screen-invisible = 1.

          ENDIF.
        ELSE.
          screen-invisible = 1.

        ENDIF.
        MODIFY SCREEN.
      ELSEIF screen-name EQ 'PB_CHG_DTV'.
*        SELECT SINGLE *
*                FROM SETLEAF
*                INTO WL_SETLEAF
*                 WHERE SETNAME EQ 'MAGGI_DATA_SOLICIT_VENDA'
*                  AND VALFROM  EQ SY-UNAME.
*        IF SY-SUBRC IS INITIAL.
*          SCREEN-INVISIBLE = 0.
*          SCREEN-ACTIVE = 1.
*          SCREEN-INPUT = 1.
*        ELSE.
        screen-active = 0.
*          SCREEN-ACTIVE = 0.
*          SCREEN-INPUT = 0.
*        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wg_acao EQ c_chg_dtv.
    LOOP AT SCREEN.
      IF screen-name EQ 'WG_HEADER-DATA_VENDA'.
        screen-input = 1.
        MODIFY SCREEN.
      ELSEIF screen-name EQ 'WG_HEADER-NRO_SOL_OV'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF wg_colaps EQ '@K1@'.
    LOOP AT SCREEN.
      IF screen-group4 EQ 'B1'.

        screen-active    = 0.
        screen-invisible = 1.

        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'TITULO'.
        screen-invisible = 0.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSE.
    LOOP AT SCREEN.
      IF screen-group4 EQ 'B1'.
        IF screen-name EQ 'PB_CHG_DTV'.

          IF wl_setleaf-setname NE 'ZSDT0062_DT_VENDA'.   "#performance

            SELECT SINGLE *
                    FROM setleaf
                    INTO wl_setleaf
                     WHERE setname EQ 'ZSDT0062_DT_VENDA' "MAGGI_DATA_SOLICIT_VENDA
                      AND valfrom  EQ sy-uname.
          ELSE."#performance
            sy-subrc = 0 .
          ENDIF.   "#performance

          IF sy-subrc IS INITIAL.
            screen-invisible = 0.
            screen-active = 1.
            screen-input = 1.
          ELSE.
            screen-active = 0.
            screen-input = 0.
            screen-invisible = 1.
          ENDIF.
        ELSE.
          screen-active    = 1.
          screen-invisible = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'TITULO'.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group2 EQ 'A4'
       AND ( wg_acao EQ c_modif_qtd
         OR wg_acao EQ c_modif
         OR wg_acao EQ c_add ).
        screen-invisible = 0.
        screen-active    = 1.
        MODIFY SCREEN.
      ELSEIF screen-group2 EQ 'A4'.
        IF ( wg_header-param_espec EQ space
                OR wg_header-param_espec EQ c_a
*-CS2021000615 - 17.06.2021 - JT - inicio
                OR wg_header-param_espec EQ c_ax
*-CS2021000615 - 17.06.2021 - JT - fim
                OR wg_header-param_espec EQ c_m
                OR wg_header-param_espec EQ c_z )" CS2020000373 23/08/2022 -LP
                AND ( wg_header-auart NE 'ZEXP'
                  AND wg_header-auart NE 'ZEXI' ).

*   "// PBI-59125 Melhoria de perfomace Inicio
          READ TABLE tl_0064 WITH KEY uname = sy-uname TRANSPORTING NO FIELDS.
*          SELECT *
*              FROM zsdt0064
*              INTO TABLE tl_0064
*               WHERE uname EQ sy-uname.
*   "// PBI-59125 Melhoria de perfomace Fim
          IF sy-subrc IS INITIAL.
            screen-active    = 1.
            screen-invisible = 0.
          ELSE.
            screen-active    = 0.
            screen-invisible = 1.

          ENDIF.
        ELSE.
          screen-invisible = 1.
          screen-active    = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF tg_cond_esp[] IS NOT INITIAL.
    wg_cond_esp = icon_display_more.
  ELSE.
    wg_cond_esp = icon_enter_more.
  ENDIF.

  IF  wg_header-param_espec EQ c_z .
    LOOP AT SCREEN.
      IF screen-name = 'WG_COND_ESP'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DATA(tab) = |TAB_STRIP_TAB{ g_tab_strip-pressed_tab+12 }|.
  DATA vl_inativo TYPE char1.
  DATA vl_ativo TYPE char1.

  LOOP AT SCREEN.
    IF screen-name EQ tab.
      IF screen-active EQ 0.
        vl_inativo = abap_true.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF vl_inativo IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-name CS 'TAB_STRIP_TAB'.
        IF screen-active EQ 1 AND vl_ativo IS INITIAL.
          vl_ativo = abap_true.

          g_tab_strip-subscreen = |010{ screen-name+13 }|.
          g_tab_strip-pressed_tab+12 = screen-name+13.
          CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
            EXPORTING
              functioncode           = g_tab_strip-pressed_tab
            EXCEPTIONS
              function_not_supported = 1.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid4->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid5->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid6->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid7->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  IF grid8 IS NOT INITIAL.
    CALL METHOD grid8->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "PERFORM verifica_erros. "Comentando 25.05.2021


  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
*     I_SHOW        = 'X'
      i_repid       = sy-repid
      i_popup       = 0
      i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
      i_set_cell    = 'WG_CELL'
      i_set_obj     = 'WG_OBJ'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    CALL METHOD grid1->set_selected_cells
      EXPORTING
        it_cells = tg_cell[].

    APPEND wg_cell TO tg_cell.
    CALL METHOD grid1->set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  get_next_number
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_7995   text
*      -->P_7996   text
*      <--P_VL_NRONC  text
*----------------------------------------------------------------------*
FORM get_next_number  USING    p_object   "TYPE nrobj
                               p_nr_range "TYPE nrnr
                               p_commit
                      CHANGING p_number.

  IF p_commit IS NOT INITIAL.
    CLEAR p_number.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = p_nr_range
        object                  = p_object
      IMPORTING
        number                  = p_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    IF sy-subrc NE 0.
      CLEAR: p_number.
      MESSAGE e836(sd) WITH TEXT-m17.
    ELSE.
      IF p_object EQ 'ZNR_SOL_OV'.
        wg_flag = c_x.
      ENDIF.
    ENDIF.
  ELSE.
    p_number = '$00000001'.
    wg_flag = c_x.
  ENDIF.

ENDFORM.                    " get_next_number
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_doc.

  DATA: wl_0051     TYPE zsdt0051,
        wl_0052     TYPE zsdt0052,
*        wL_0057 TYPE ZSDT0057,
        tl_0053     TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_0053_aux TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_0054     TYPE TABLE OF zsdt0054 WITH HEADER LINE,
        tl_0055     TYPE TABLE OF zsdt0055 WITH HEADER LINE,
*        TL_0056 TYPE TABLE OF ZSDT0056 WITH HEADER LINE,
        tl_0059     TYPE TABLE OF zsdt0059 WITH HEADER LINE,
        tl_0059_2   TYPE TABLE OF zsdt0059_2 WITH HEADER LINE,
        tl_0063     TYPE TABLE OF zsdt0063 WITH HEADER LINE,
*        TL_0045     TYPE TABLE OF ZSDT0045 WITH HEADER LINE,
        tl_0066     TYPE TABLE OF zsdt0066 WITH HEADER LINE,
        tl_0069     TYPE TABLE OF zsdt0069 WITH HEADER LINE,
        tl_0162     TYPE TABLE OF zsdt0162 WITH HEADER LINE,
        tl_0073     TYPE TABLE OF zsdt0073 WITH HEADER LINE,
        tl_makt     TYPE TABLE OF makt     WITH HEADER LINE,
        tl_lfa1     TYPE TABLE OF lfa1     WITH HEADER LINE,
        tl_bsad     TYPE TABLE OF ty_bsad WITH HEADER LINE,
        tl_bsak     TYPE TABLE OF ty_bsad WITH HEADER LINE.

  CLEAR: wl_0051, wl_0052, tl_0053, tl_0054, tl_0055, tl_makt, tl_0059, tl_bsad,
         tl_0063, tl_lfa1, tl_bsak, tl_0069, tl_0073, tl_0053_aux.

  IF wl_0051-nro_sol_ov NE wg_header-nro_sol_ov."#performance

    SELECT SINGLE *
      FROM zsdt0051
      INTO wl_0051
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

  ELSE.
    sy-subrc = 0.
  ENDIF.

  IF sy-subrc IS INITIAL.

    SELECT *
      FROM zsdt0213
      INTO CORRESPONDING FIELDS OF TABLE it_0213
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov
          AND status EQ abap_false.

    SELECT SINGLE *
      FROM zsdt0052
      INTO wl_0052
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    SELECT *
      FROM zsdt0053
      INTO TABLE tl_0053
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov
       AND status NE 'C'.


    IF sy-subrc IS INITIAL.

      tl_makt[] = zcl_material=>get_instance( )->get_material_text_by_table( tl_0053[] ). "# performance

*      SELECT *
*        FROM makt
*        INTO TABLE tl_makt
*        FOR ALL ENTRIES IN tl_0053
*         WHERE matnr EQ tl_0053-matnr
*           AND spras EQ sy-langu.
    ENDIF.

    SELECT *
      FROM zsdt0054
      INTO TABLE tl_0054
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    IF sy-subrc IS INITIAL.
      SELECT belnr augdt augbl
        FROM bsad
        INTO TABLE tl_bsad
         FOR ALL ENTRIES IN tl_0054
         WHERE bukrs EQ wl_0051-vkorg
           AND belnr EQ tl_0054-adiant.

    ENDIF.

    SELECT *
      FROM zsdt0063
      INTO TABLE tl_0063
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    IF sy-subrc IS INITIAL.
      SELECT belnr augdt augbl
        FROM bsak
        INTO TABLE tl_bsak
         FOR ALL ENTRIES IN tl_0063
         WHERE bukrs EQ tl_0063-bukrs
           AND belnr EQ tl_0063-adiant.

      SELECT *
        FROM lfa1
        INTO TABLE tl_lfa1
          FOR ALL ENTRIES IN tl_0063
          WHERE lifnr EQ tl_0063-lifnr.

    ENDIF.

    SELECT *
      FROM zsdt0055
      INTO TABLE tl_0055
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov
        AND status       NE 'C'.

*    SELECT *
*      FROM ZSDT0055
*      INTO TABLE TL_0055
*        WHERE NRO_SOL_OV EQ WG_HEADER-NRO_SOL_OV.

    IF tl_0059[] IS INITIAL.  "#performance

      SELECT *
        FROM zsdt0059
        INTO TABLE tl_0059
          WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    ENDIF."#performance

    IF tl_0045[] IS INITIAL. "#performance

      SELECT *
        FROM zsdt0045
        INTO CORRESPONDING FIELDS OF TABLE tl_0045
          WHERE objek       EQ wg_header-nro_sol_ov
            AND objecttable EQ 'ZSDT0051'
            AND bukrs       EQ wl_0051-vkorg.

    ENDIF."#performance

    SELECT SINGLE redist redist_fix lote_obg" 23.09.2024 - 147331 - RAMON --
      FROM zsdt0057
        INTO (lv_redist,gv_redist_fix, gv_lib_lote) " 23.09.2024 - 147331 - RAMON --
          WHERE tp_venda EQ wl_0051-tp_venda.

    " 13.03.2025 - RAMON - 166561 - -->
    IF sy-subrc EQ 0.
      gv_redist = lv_redist.
    ENDIF.
    " 13.03.2025 - RAMON - 166561 --<

    IF tl_0059_2[] IS INITIAL.  "#performance
      " 15.02.2024 - 121095 - RBL -->
      SELECT * FROM zsdt0059_2
        INTO TABLE tl_0059_2
          WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
      " 15.02.2024 - 121095 - RBL --<
    ENDIF.  "#performance

*    IF it_zsdt0045[] IS INITIAL.  "#performance
*
*      SELECT * FROM zsdt0045
*        INTO CORRESPONDING FIELDS OF TABLE it_zsdt0045
*          WHERE bukrs       EQ wl_0051-vkorg
*            AND objecttable EQ 'ZSDT0051'.
*
*    ENDIF.

    FREE: tg_ins_frete.
    MOVE tl_0045[] TO tg_ins_frete.

    LOOP AT tl_0045 ASSIGNING FIELD-SYMBOL(<f0045>).
      <f0045>-lgort = <f0045>-charg.
    ENDLOOP.

    IF tl_0045[] IS NOT INITIAL.

      SELECT *
        FROM zppt0002
        INTO CORRESPONDING FIELDS OF TABLE it_002
        FOR ALL ENTRIES IN tl_0045
          WHERE werks    EQ tl_0045-werks
            AND cd_safra EQ tl_0045-safra
            AND lgort    EQ tl_0045-lgort.

      "Projeto Reestruturação Algodao 2024
      SELECT *
        FROM zppt0040
        INTO CORRESPONDING FIELDS OF TABLE it_zppt0040
        FOR ALL ENTRIES IN tl_0045
          WHERE werks    EQ tl_0045-werks
            AND lgort    EQ tl_0045-lgort
            AND safra    EQ tl_0045-safra.

      SORT it_002.
      DELETE ADJACENT DUPLICATES FROM it_002 COMPARING ALL FIELDS.

      IF it_002[] IS NOT INITIAL.

        SELECT *
          FROM zppt0004
          INTO CORRESPONDING FIELDS OF TABLE it_004
          FOR ALL ENTRIES IN it_002
            WHERE verid EQ it_002-verid
              AND werks EQ it_002-werks.

      ENDIF.

    ENDIF.

    SELECT *
          FROM zsdt0066
          INTO TABLE tl_0066
            WHERE nro_sol_ov EQ wg_header-nro_sol_ov
            AND status NE 'D'.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM makt
        APPENDING TABLE tl_makt
         FOR ALL ENTRIES IN tl_0066
         WHERE matnr EQ tl_0066-matnr
           AND spras EQ sy-langu.
    ENDIF.

    SELECT *
      FROM zsdt0069
      INTO TABLE tl_0069
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    SELECT *
      FROM zsdt0162
      INTO TABLE tg_0162
      WHERE vbeln EQ wg_header-nro_sol_ov.

    CLEAR: tg_estrat[].
    SELECT * FROM zsdt0161
    INTO CORRESPONDING FIELDS OF TABLE tg_estrat
    WHERE bukrs         LE wl_0051-vkorg
      AND bukrs_ate     GE wl_0051-vkorg
      AND vkbur         LE wl_0051-vkbur
      AND vkbur_ate     GE wl_0051-vkbur
      AND waers         EQ wl_0051-waerk
      AND tp_venda      LE wl_0051-tp_venda
      AND tp_venda_ate  GE wl_0051-tp_venda
      AND dt_val_de     LE sy-datum
      AND dt_val_ate    GE sy-datum.

    SORT tg_estrat BY nivel.

    SELECT *
      FROM zsdt0073
      INTO TABLE tl_0073
        WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    PERFORM monta_dados_doc TABLES tl_0053
                                   tl_0054
                                   tl_0055
                                   tl_0059
                                   tl_0059_2
                                   tl_0063
*                                   TL_0045
                                   tl_0066
                                   tl_0069
                                   tg_0162
                                   tl_0073
                                   tl_lfa1
                                   tl_makt
                                   tl_bsad
                                   tl_bsak
                            USING  wl_0051
                                   wl_0052.


    SELECT *
      FROM zsdt0235
       INTO TABLE it_0235
         WHERE tp_venda EQ wg_header-tp_venda.

    " 20.09.2024 - 147331 - RAMON -->

    READ TABLE it_0235 TRANSPORTING NO FIELDS
      WITH KEY tabela = 'ZSDT0053'
                campo = 'FIXACAO'.

    IF sy-subrc EQ 0.
      gv_sub_total_fix = abap_true.
    ELSE.
      gv_sub_total_fix = abap_false.
    ENDIF.

    " 20.09.2024 - 147331 - RAMON --<

    PERFORM busca_bezei_56.
*    BREAK-POINT.
    MOVE tl_0053[] TO tl_0053_aux[].
    SORT tl_0053_aux[] BY fixacao.
    DELETE ADJACENT DUPLICATES FROM tl_0053_aux[] COMPARING fixacao.

    LOOP AT tl_0053_aux.
      CLEAR: g_monat_c, g_monat_p.
      PERFORM busca_valores_59 USING tl_0053_aux-fixacao.
    ENDLOOP.

  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m18.
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0041  text
*      -->P_WL_0040  text
*----------------------------------------------------------------------*
FORM monta_dados_doc  TABLES tl_0053 STRUCTURE zsdt0053
                             tl_0054 STRUCTURE zsdt0054
                             tl_0055 STRUCTURE zsdt0055
                             tl_0059 STRUCTURE zsdt0059
                             tl_0059_2 STRUCTURE zsdt0059_2
                             tl_0063 STRUCTURE zsdt0063
*                             TL_0045 STRUCTURE ZSDT0045\
                             tl_0066 STRUCTURE zsdt0066
                             tl_0069 STRUCTURE zsdt0069
                             tg_0162 STRUCTURE zsdt0162
                             tl_0073 STRUCTURE zsdt0073
                             tl_lfa1 STRUCTURE lfa1
                             tl_makt STRUCTURE makt
                             tl_bsad STRUCTURE tg_bsad
                             tl_bsak STRUCTURE tg_bsad
                      USING  wl_0051 TYPE zsdt0051
                             wl_0052 TYPE zsdt0052.

  DATA: p_2(13)          TYPE p DECIMALS 2,
        p_5(13)          TYPE p DECIMALS 5,
        p_4(13)          TYPE p DECIMALS 4,
        wl_valor(30),
        wl_valor_aux(30),
        wl_tabix         TYPE sy-tabix.

  CLEAR: wg_header, wg_cond_pgt.
  REFRESH: tg_itens, tg_pgt_ant, tg_logistica.

* Header
  MOVE-CORRESPONDING: wl_0051 TO wg_header.

*-IR065241 - 06.07.2021 - JT - inicio
  IF wg_header-risco_sacado IS INITIAL.
    wg_header-risco_sacado = 'N'.
  ENDIF.
*-IR065241 - 06.07.2021 - JT - fim

  IF wl_0051-status EQ c_a.
    wg_status = icon_initial.
    wg_desc_status = TEXT-s01.
  ELSEIF wl_0051-status EQ c_l.
    wg_status = icon_release.
    wg_desc_status = TEXT-s02.
  ELSEIF wl_0051-status EQ c_d.
    wg_status = icon_delete.
    wg_desc_status = TEXT-s03.
  ELSEIF wl_0051-status EQ c_r.
    wg_status = icon_defect.
    wg_desc_status = TEXT-s04.
  ELSEIF wl_0051-status EQ c_p.
    wg_status = icon_led_yellow.
    wg_desc_status = TEXT-s05.
  ELSEIF wl_0051-status EQ c_z." CS2020000373 23/08/2022 -LP
    wg_status = icon_initial.
    wg_desc_status = TEXT-s01.
  ENDIF.

  IF wl_0051-observacao IS NOT INITIAL.
    CONCATENATE '@6X@' TEXT-b06 INTO pb_obs.
  ELSE.
    CONCATENATE '@6Y@' TEXT-b06 INTO pb_obs.
  ENDIF.

  IF wl_0051-coment_logistica IS NOT INITIAL.
    CONCATENATE '@6X@' TEXT-b07 INTO pb_coment_logistica.
  ELSE.
    CONCATENATE '@6Y@' TEXT-b07 INTO pb_coment_logistica.
  ENDIF.

* Condição de pagamento
  MOVE-CORRESPONDING: wl_0052 TO wg_cond_pgt.

* Itens
  LOOP AT tl_0053.

    READ TABLE tl_makt
      WITH KEY matnr = tl_0053-matnr.

    MOVE-CORRESPONDING: tl_0053 TO tg_itens.
    MOVE: tl_makt-maktx  TO tg_itens-maktx,
          tl_0053-dmbtr  TO tg_itens-dmbtr,
          tl_0053-vlrtot TO tg_itens-vlrtot,
          tl_0053-ck_troca_nota TO tg_itens-ck_troca_nota.

*    TG_ITENS-AUART = COND #( WHEN TL_0053-AUART IS NOT INITIAL THEN TL_0053-AUART ELSE WG_HEADER-AUART ).
    tg_itens-auart = tl_0053-auart.

    IF tl_0053-vbeln IS NOT INITIAL
    AND ( tl_0053-status IS INITIAL
      OR tl_0053-status EQ 'D' ).
      tg_itens-status_icon = icon_checked.

    ELSEIF tl_0053-vbeln IS INITIAL
      AND tg_itens-status IS INITIAL.
      tg_itens-status_icon = icon_message_warning.

    ELSEIF tl_0053-status EQ c_b.
      READ TABLE tl_0054 TRANSPORTING NO FIELDS
        WITH KEY nro_sol_ov = tl_0053-nro_sol_ov
                 posnr      = tl_0053-posnr.
      IF sy-subrc IS INITIAL.
        tg_itens-status_icon = icon_cost_components.
      ELSE.
        tg_itens-status_icon = icon_locked.
      ENDIF.
    ELSEIF tl_0053-status EQ c_e.
      tg_itens-status_icon = icon_complete.
    ELSEIF tl_0053-status EQ c_y.
      tg_itens-status_icon = icon_system_undo.
    ELSEIF tl_0053-status EQ c_w.
      tg_itens-status_icon = icon_positive.
    ENDIF.

    tg_itens-lotes_vol = '@1F@'.

    IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
       wg_header-param_espec EQ c_ax OR
      wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
      IF NOT line_exists( tg_itens-style[ fieldname = 'LOTES_VOL' ] ).

        " 01.12.2023 - 128467 - RBL -->
        INSERT VALUE #(
                          fieldname = 'LOTES_VOL'
                          style = cl_gui_alv_grid=>mc_style_button
                        )  INTO TABLE tg_itens-style.

*        APPEND VALUE #(
*                        fieldname = 'LOTES_VOL'
*                        style = cl_gui_alv_grid=>mc_style_button
*                      )  TO tg_itens-style.
        " 01.12.2023 - 128467 - RBL --<

      ENDIF.
    ENDIF.

    APPEND tg_itens.
    CLEAR: tg_itens, tl_0053, tl_makt.
  ENDLOOP.


* Pagamento Antecipado
  REFRESH: tg_pgt_ant-style.
  SORT: tl_bsad BY belnr.

  LOOP AT tl_0054.
    READ TABLE tl_bsad
      WITH KEY belnr = tl_0054-adiant
               BINARY SEARCH.

    MOVE-CORRESPONDING: tl_0054 TO tg_pgt_ant.

    MOVE: tl_bsad-audgt TO tg_pgt_ant-augdt,
          tl_bsad-augbl TO tg_pgt_ant-augbl.

    APPEND tg_pgt_ant.
    CLEAR: tg_pgt_ant, tl_0054, tl_bsad.
  ENDLOOP.

* Adiantamento Exterio
  SORT: tl_bsak BY belnr.
  LOOP AT tl_0063.
    READ TABLE tl_bsak
      WITH KEY belnr = tl_0063-adiant
               BINARY SEARCH.

    READ TABLE tl_lfa1
      WITH KEY lifnr = tl_0063-lifnr.

    MOVE-CORRESPONDING: tl_0063 TO tg_adto_ext.

    MOVE: tl_bsak-audgt TO tg_adto_ext-augdt,
          tl_bsak-augbl TO tg_adto_ext-augbl,
          tl_lfa1-name1 TO tg_adto_ext-name1.

    APPEND tg_adto_ext.
    CLEAR: tg_adto_ext, tl_0063, tl_bsak.
  ENDLOOP.

* Instrução

  DATA: material TYPE makt.
  LOOP AT tl_0045.
    CLEAR material.

    MOVE-CORRESPONDING: tl_0045 TO tg_instrucao.

    SELECT SINGLE *
      FROM makt
        INTO material
          WHERE matnr EQ tg_instrucao-matnr
             AND spras EQ sy-langu.

*-CS2021000615 - 17.06.2021 - JT - inicio
    IF wg_header-param_espec = c_ax OR
       wg_header-param_espec =  c_a OR
      wg_header-param_espec = c_z .
      tg_instrucao-tp_fardo = tl_0045-tamanho_fardo.
    ELSE.

      "Projeto Reestruturação Algodao 2024
      READ TABLE it_zppt0040 INTO DATA(lwa_zppt0040) WITH KEY werks    = tl_0045-werks
                                                              safra    = tl_0045-safra
                                                              lgort    = tl_0045-lgort.
      IF sy-subrc EQ 0.
        tg_instrucao-tp_fardo = lwa_zppt0040-tipo_fardo.
      ELSE.
        LOOP AT it_002 WHERE werks    EQ tl_0045-werks
                          AND cd_safra EQ tl_0045-safra
                          AND lgort    EQ tl_0045-lgort.

          LOOP AT it_004 WHERE verid    EQ it_002-verid.
            IF tg_instrucao-tp_fardo IS INITIAL.
              tg_instrucao-tp_fardo = it_004-tipo.
            ELSE.
              tg_instrucao-tp_fardo = |{ tg_instrucao-tp_fardo }, { it_004-tipo }|.
            ENDIF.
          ENDLOOP.
        ENDLOOP.
      ENDIF.




    ENDIF.
*-CS2021000615 - 17.06.2021 - JT - fim

    MOVE material-maktx TO tg_instrucao-desc_mat.

    tg_instrucao-icon =
    SWITCH #( tl_0045-status
                            WHEN ''  THEN '@Q3@'
                            WHEN 'L' THEN '@5Y@'
                            WHEN 'A' THEN '@K4@'
                            WHEN 'F' THEN '@DF@'
             ).

    MOVE: tl_0045-quantidade TO tg_instrucao-quantidade.
    APPEND tg_instrucao.
    CLEAR: tg_instrucao, tl_0045.
  ENDLOOP.

* Formação de Lote
  SORT: tl_makt BY matnr.
  CLEAR: tg_form_lote, tl_makt, tl_0066.
  LOOP AT tl_0066.
    READ TABLE tl_makt
      WITH KEY matnr = tl_0066-matnr
               BINARY SEARCH.

    MOVE-CORRESPONDING: tl_0066 TO tg_form_lote.

    MOVE: tl_makt-maktx TO tg_form_lote-maktx.

    IF tl_0066-status EQ c_a.
      tg_form_lote-status = icon_initial.
    ELSEIF tl_0066-status EQ c_l.
      tg_form_lote-status = icon_release.
    ELSEIF tl_0066-status EQ c_d.
      tg_form_lote-status = icon_defect.
    ELSEIF tl_0066-status EQ c_z.
      tg_form_lote-status = icon_initial.
    ENDIF.

    tg_form_lote-ztrocanota = tl_0066-ck_troca_nota.

    APPEND tg_form_lote.
    CLEAR: tg_form_lote, tl_makt, tl_0066.
  ENDLOOP.

* Logistica
  LOOP AT tl_0055.

    MOVE-CORRESPONDING: tl_0055 TO tg_logistica.

    APPEND tg_logistica.
    CLEAR: tg_logistica, tl_0055.
  ENDLOOP.

* Preco
  IF tree1 IS NOT INITIAL.
    CALL METHOD tree1->delete_all_nodes.
  ENDIF.
  REFRESH: t_fieldcatalog.
  CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
    EXPORTING
      i_tp_venda      = wg_header-tp_venda
    IMPORTING
      e_table         = t_new_table
    TABLES
      te_fieldcatalog = t_fieldcatalog
      te_fields       = tg_fields.


  CALL METHOD grid4->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = t_fieldcatalog.
* Cria uma field-symbol como tabela interna
  ASSIGN t_new_table->* TO <fs_table>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.
  CREATE DATA t_new_table LIKE <fs_table>.
* Cria uma field-symbol como tabela interna
  ASSIGN t_new_table->* TO <fs_table_frame>.

* Cria uma field-symbol como work area
  ASSIGN t_new_line->* TO <fs_line>.
  ASSIGN t_new_line->* TO <fs_line_aux>.

  SORT tl_0059 BY posnr ASCENDING nivel ASCENDING cod_fp ASCENDING .

  " 21.02.2024 - RAMON -->
  APPEND LINES OF tl_0059_2 TO tl_0059.
  " 21.02.2024 - RAMON --<

  LOOP AT tl_0059.

*    MOVE-CORRESPONDING: TL_0059 TO TG_PRECO_N.
    MOVE-CORRESPONDING: tl_0059 TO <fs_line>.

    PERFORM get_set_valores USING 'REDIST'
                                  'S'
                         CHANGING tl_0059-redist.


    PERFORM get_set_valores USING 'TIPO_CALC'
                                  'S'
                         CHANGING tl_0059-tipo_calc.

    PERFORM get_set_valores USING 'C_DECIMAIS'
                                  'S'
                         CHANGING tl_0059-c_decimais.

    PERFORM get_set_valores USING 'PRECO_ITEM'
                                  'S'
                         CHANGING tl_0059-preco.

    PERFORM get_set_valores USING 'WAERS'
                                  'S'
                         CHANGING tl_0059-waers.

    PERFORM get_set_valores USING 'OCBOT'
                                  'S'
                         CHANGING tl_0059-ocbot.

    CONDENSE tl_0059-formula2 NO-GAPS.

    IF tl_0059-c_decimais EQ 2.
      p_2 = tl_0059-formula2.
      WRITE p_2 TO tg_preco_n-formula.
    ELSEIF tl_0059-c_decimais EQ 4.
      p_4 = tl_0059-formula2.
      WRITE p_4 TO tg_preco_n-formula.
    ELSEIF tl_0059-c_decimais EQ 5.
      p_5 = tl_0059-formula2.
      WRITE p_5 TO tg_preco_n-formula.
    ENDIF.
    CONDENSE tg_preco_n-formula NO-GAPS.
*    MOVE: TL_0059-FORMULA2 TO TG_PRECO-FORMULA2.
*    MOVE: TL_0059-FORMULA  TO TG_PRECO_N-FORMULA.

    PERFORM get_set_valores USING tl_0059-field
                                  'S'
                         CHANGING tg_preco_n-formula.

*    APPEND TG_PRECO_N.
    CLEAR:wl_tabix.
    LOOP AT <fs_table> ASSIGNING <fs_line_aux>.

      CLEAR: wl_valor, wl_tabix.
      PERFORM get_set_valores USING  'COD_FP'
                                     'G'
                            CHANGING wl_valor.

      CLEAR wl_valor_aux.

      ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

      wl_valor_aux = <fs_campo>.

      IF wl_valor EQ wl_valor_aux.

        CLEAR wl_valor_aux.

        ASSIGN COMPONENT 'POSNR'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

        wl_valor_aux = <fs_campo>.

        IF tl_0059-posnr EQ wl_valor_aux AND tl_0059-redist = abap_false. "<-- ramon 21.02.2024

          ASSIGN COMPONENT tl_0059-field  OF STRUCTURE <fs_line_aux> TO <fs_campo>.

          <fs_campo> = tg_preco_n-formula.

          wl_tabix = sy-tabix.

        ENDIF.

      ENDIF.

      " 16.02.2024 - RAMON -->
      IF lv_redist = abap_true.

        IF tl_0059-redist = abap_true.

          ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line_aux> TO <fs_campo>.

          IF <fs_campo> = tl_0059-bezei.

            ASSIGN COMPONENT 'REDIST' OF STRUCTURE <fs_line_aux> TO <fs_campo>.
            <fs_campo> = tl_0059-redist.

          ENDIF.

        ENDIF.

      ENDIF.

      " 16.02.2024 - RAMON --<

      IF wg_header-param_espec EQ 'Z'. "RJF-Ini
        ASSIGN COMPONENT 'BEZEI'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
        ASSIGN COMPONENT 'PRECO'  OF STRUCTURE <fs_line_aux> TO <fs_campo2>.
        wl_valor     = <fs_campo>.
        wl_valor_aux = <fs_campo2>.
        IF wl_valor EQ 'QUANTIDADE À FIXAR' AND wl_valor_aux IS NOT INITIAL.
          <fs_campo2> = abap_false.
        ENDIF.
      ENDIF. "RJF-Fim

    ENDLOOP.
    IF wl_tabix IS INITIAL.

      " 22.02.2023 - RAMON -->
      PERFORM get_set_valores USING  'NIVEL' 'G' CHANGING wl_valor.
      gv_ultimo_nvl = wl_valor.
      " 22.02.2023 - RAMON --<


      APPEND <fs_line> TO <fs_table>.
    ELSEIF wl_tabix IS NOT INITIAL.
*        MODIFY <FS_TABLE> FROM <FS_LINE> INDEX WL_TABIX.
    ENDIF.
    CLEAR: tg_preco_n, tl_0059, <fs_line>.
  ENDLOOP.

  LOOP AT tl_0059.
    MOVE-CORRESPONDING: tl_0059 TO tg_preco_n.
    IF tl_0059-tipo_calc EQ c_c
    OR tl_0059-tipo_calc EQ c_r.
      MOVE: tl_0059-formula TO tg_preco_n-formula.
    ELSE.
      IF tl_0059-c_decimais EQ 2.
        p_2 = tl_0059-formula2.
        WRITE p_2 TO tg_preco_n-formula.
      ELSEIF tl_0059-c_decimais EQ 4.
        p_4 = tl_0059-formula2.
        WRITE p_4 TO tg_preco_n-formula.
      ELSEIF tl_0059-c_decimais EQ 5.
        p_5 = tl_0059-formula2.
        WRITE p_5 TO tg_preco_n-formula.
      ENDIF.
      CONDENSE tg_preco_n-formula NO-GAPS.
    ENDIF.

    " 21.02.2024 - RAMON -->
    IF tl_0059-redist = abap_true.
      tg_preco_n-invisible = abap_true.
    ENDIF.
    " 21.02.2024 - RAMON --<

    APPEND tg_preco_n.
  ENDLOOP.

  READ TABLE tg_preco_n  INDEX 1.
  DELETE tg_preco_n WHERE posnr NE tg_preco_n-posnr.

  IF tree1 IS NOT INITIAL
      AND grid4 IS NOT INITIAL.
    PERFORM redefine_objetos.
  ENDIF.
  IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.

    PERFORM create_hierarchy.
  ENDIF.
  tg_0059_old[] = tl_0059[].

* Motivo
  LOOP AT tl_0069.

    MOVE-CORRESPONDING: tl_0069 TO tg_motivo.

    APPEND tg_motivo.
    CLEAR: tg_motivo, tl_0069.
  ENDLOOP.

  LOOP AT tg_0162.

    tg_motivo-id_historico  = tg_0162-id_log.
    tg_motivo-nro_sol_ov    = tg_0162-vbeln.
    tg_motivo-status        = tg_0162-status.
    tg_motivo-motivo        = tg_0162-motivo.
    tg_motivo-usnam         = tg_0162-usuario.
    tg_motivo-data_atual    = tg_0162-data_atual.
    tg_motivo-hora_atual    = tg_0162-hora_atual.

    APPEND tg_motivo.

  ENDLOOP.

* Cond. especial
  LOOP AT tl_0073.
    MOVE-CORRESPONDING: tl_0073 TO tg_cond_esp.

    APPEND tg_cond_esp.
    CLEAR: tl_0073, tg_cond_esp.
  ENDLOOP.


ENDFORM.                    " MONTA_DADOS_DOC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .

  DATA: obj_frete TYPE REF TO zcl_solicitacao_ov.
  CREATE OBJECT obj_frete.


  DATA: wl_input_0051     TYPE zsdt0051,
        wl_0051           TYPE zsdt0051,
        tl_0053           TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_0066           TYPE TABLE OF zsdt0066 WITH HEADER LINE,
        tl_0061           TYPE TABLE OF zsdt0061 WITH HEADER LINE,
        wl_input_0052     TYPE zsdt0052,
        tl_input_0053     TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tl_input_0053_aux TYPE TABLE OF zsdt0053 WITH HEADER LINE, " novos itens redistribuidos
        tl_input_0054     TYPE TABLE OF zsdt0054 WITH HEADER LINE,
        tl_input_0055     TYPE TABLE OF zsdt0055 WITH HEADER LINE,
        tl_input_0059     TYPE TABLE OF zsdt0059 WITH HEADER LINE,
        tl_input_0061     TYPE TABLE OF zsdt0061 WITH HEADER LINE,
        tl_input_0063     TYPE TABLE OF zsdt0063 WITH HEADER LINE,
        tl_input_0045     TYPE TABLE OF zsdt0045 WITH HEADER LINE,
        tl_input_0066     TYPE TABLE OF zsdt0066 WITH HEADER LINE,
        tl_input_0073     TYPE TABLE OF zsdt0073 WITH HEADER LINE,
        tl_input_0184     TYPE TABLE OF zsdt0184 WITH HEADER LINE,
        wl_input_0084     TYPE zsdt0084,
        it_zsdt0059       TYPE TABLE OF zsdt0059,
        wa_zsdt0059       TYPE zsdt0059,
        p_2(13)           TYPE p DECIMALS 2,
        p_5(13)           TYPE p DECIMALS 5,
        p_4(13)           TYPE p DECIMALS 4,
        f_g               TYPE zsded038,
        wl_valor(30),
        wl_valor_aux(30).
  DATA: var_tabix59 TYPE sy-tabix,
        var_len59   TYPE sy-tabix,
        qtd_fixa(9) TYPE c VALUE 'QTDFIXADA'.

  DATA lt_preco_fixacao TYPE TABLE OF ty_preco_n.

  DATA: var_msg                       TYPE string.
  DATA: cx_exception                  TYPE REF TO zcx_webservice.
  DATA: obj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.

  CLEAR: wl_input_0051, wl_input_0052, tl_input_0053, tl_input_0054, tl_input_0055, wl_0051,
         tl_input_0059, tl_input_0053_aux, tl_input_0063, tl_input_0045, tl_input_0066, tl_0066,
         tl_input_0073, wl_input_0084, tl_input_0184.

  REFRESH: tl_input_0053, tl_input_0054, tl_input_0055, tl_input_0059, tl_input_0061,
           tl_0061, tl_input_0053_aux, tl_input_0063, tl_input_0045, tl_input_0066, tl_0066,
           tl_input_0073, tl_input_0184.

  " 08.02.2024 - 128467 -	RBL -->

  IF wg_header-param_espec EQ c_z.

    IF wg_cond_pgt IS NOT INITIAL.

      DO wg_header-num_fixacao TIMES.

        DATA(lv_index) = sy-index.

        APPEND INITIAL LINE TO tg_cond_esp ASSIGNING FIELD-SYMBOL(<fs_cond_esp>).

        <fs_cond_esp>-fixacao = lv_index.
        <fs_cond_esp>-zterm = wg_cond_pgt-zterm.

        IF wg_cond_pgt-qte_venc IS INITIAL OR wg_cond_pgt-qte_venc = '00'.
          <fs_cond_esp>-qte_venc = '01'.
        ELSE.
          <fs_cond_esp>-qte_venc = wg_cond_pgt-qte_venc.
        ENDIF.

      ENDDO.

      SORT tg_cond_esp BY fixacao.

      DELETE tg_cond_esp WHERE qte_venc = 0.

    ENDIF.

    " 08.02.2024 - ramon -->
*    IF lines( tg_cond_esp ) = 1 AND wg_cond_pgt IS NOT INITIAL.
*
*      CLEAR tg_cond_esp[].
*
*      APPEND INITIAL LINE TO tg_cond_esp ASSIGNING <fs_cond_esp>.
*
*      "<fs_cond_esp>-fixacao
*      <fs_cond_esp>-zterm = wg_cond_pgt-zterm.
*      <fs_cond_esp>-qte_venc = wg_cond_pgt-qte_venc.
*
*    ENDIF.
    " 08.02.2024 - ramon --<

  ENDIF.

  " 08.02.2024 - 128467 -	RBL --<

  SELECT SINGLE *
    FROM zsdt0051
    INTO wl_0051
     WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

  IF sy-subrc IS INITIAL.
    MOVE: sy-uname           TO wl_input_0051-usnam,
          sy-datum           TO wl_input_0051-data_atual,
          sy-uzeit           TO wl_input_0051-hora_atual.
    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
*    OR WG_HEADER-PARAM_ESPEC EQ C_P.
      MOVE: wl_0051-status TO wl_input_0051-status.
    ENDIF.
  ELSE.
    MOVE: sy-uname     TO wl_input_0051-usnam,
          sy-datum     TO wl_input_0051-data_atual,
          sy-uzeit     TO wl_input_0051-hora_atual.
  ENDIF.

* // Ligação de Lote e Instrução da ZSDT0053
  PERFORM add_0213.

  IF wg_acao NE c_modif_qtd AND wg_acao NE c_modif_c_ov.
* Header
    MOVE: wg_header-nro_sol_ov       TO wl_input_0051-nro_sol_ov,
          wg_header-tp_venda         TO wl_input_0051-tp_venda,
          wg_header-auart            TO wl_input_0051-auart,
          wg_header-vkorg            TO wl_input_0051-vkorg,
          wg_header-vtweg            TO wl_input_0051-vtweg,
          wg_header-spart            TO wl_input_0051-spart,
          wg_header-vkgrp            TO wl_input_0051-vkgrp,
          wg_header-vkbur            TO wl_input_0051-vkbur,
          wg_header-kunnr            TO wl_input_0051-kunnr,
          wg_header-bstkd            TO wl_input_0051-bstkd,
          wg_header-id_contrato      TO wl_input_0051-id_contrato, "*-CS2022000332-#78223-02.06.2022-JT-inicio
          wg_header-inco1            TO wl_input_0051-inco1,
          wg_header-inco2            TO wl_input_0051-inco2,
          wg_header-vkaus            TO wl_input_0051-vkaus,
          wg_header-waerk            TO wl_input_0051-waerk,
          wg_header-observacao       TO wl_input_0051-observacao,
          wg_header-coment_logistica TO wl_input_0051-coment_logistica,
          wg_header-dtde_logist      TO wl_input_0051-dtde_logist,
          wg_header-dtate_logist     TO wl_input_0051-dtate_logist,
          wg_header-matnr            TO wl_input_0051-matnr,
          wg_header-tx_multa         TO wl_input_0051-tx_multa,
          wg_header-tx_juros         TO wl_input_0051-tx_juros,
          wg_header-correto          TO wl_input_0051-correto,
          wg_header-param_espec      TO wl_input_0051-param_espec,
          wg_header-data_venda       TO wl_input_0051-data_venda,
          wg_header-num_fixacao      TO wl_input_0051-num_fixacao,
          wg_header-risco_sacado     TO wl_input_0051-risco_sacado,
          wg_header-kvgr5            TO wl_input_0051-kvgr5,
          wg_header-v_email          TO wl_input_0051-v_email, " RJF
          wg_header-porto            TO wl_input_0051-porto.


    wl_input_0051-job = abap_false.

    IF wl_input_0051-status IS INITIAL.
      wl_input_0051-status =
      SWITCH #( wg_header-param_espec
*                WHEN 'A' THEN COND #( WHEN WG_HEADER-VKORG EQ '0001' THEN COND #( WHEN WL_0051-STATUS EQ C_L  THEN C_L ELSE C_A ) ELSE C_L )
*                WHEN 'P' THEN C_L
*-CS2021000615 - 17.06.2021 - JT - inicio
                WHEN 'P' OR 'A'OR 'Z'  THEN c_l  "OR 'X' THEN c_l
*-CS2021000615 - 17.06.2021 - JT - fim
                ELSE c_a ).
    ENDIF.

* Condição de pagamento
    MOVE-CORRESPONDING: wg_cond_pgt TO wl_input_0052.
    MOVE: wl_input_0051-nro_sol_ov  TO wl_input_0052-nro_sol_ov,
          wl_input_0051-usnam       TO wl_input_0052-usnam,
          wl_input_0051-data_atual  TO wl_input_0052-data_atual,
          wl_input_0051-hora_atual  TO wl_input_0052-hora_atual.
* Itens
    LOOP AT tg_itens.
      IF tg_itens-item_edit EQ c_r.
        CLEAR: tg_itens-item_edit.
      ENDIF.
      MOVE-CORRESPONDING: tg_itens TO tl_input_0053.
      MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0053-nro_sol_ov,
            wl_input_0051-usnam       TO tl_input_0053-usnam,
            wl_input_0051-data_atual  TO tl_input_0053-data_atual,
            wl_input_0051-hora_atual  TO tl_input_0053-hora_atual,
            tg_itens-dmbtr            TO tl_input_0053-dmbtr,
            tg_itens-vlrtot           TO tl_input_0053-vlrtot.

      APPEND tl_input_0053.
      CLEAR:tl_input_0053.
    ENDLOOP.

* Condições especiais
    LOOP AT tg_cond_esp.
      MOVE-CORRESPONDING: tg_cond_esp TO tl_input_0073.
      MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0073-nro_sol_ov,
            wl_input_0051-usnam       TO tl_input_0073-usnam,
            wl_input_0051-data_atual  TO tl_input_0073-data_atual,
            wl_input_0051-hora_atual  TO tl_input_0073-hora_atual.

      APPEND tl_input_0073.
      CLEAR:tl_input_0073.
    ENDLOOP.

* Pagamento Antecipado
    LOOP AT tg_pgt_ant.
      MOVE-CORRESPONDING: tg_pgt_ant TO tl_input_0054.
      MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0054-nro_sol_ov,
            wl_input_0051-usnam       TO tl_input_0054-usnam,
            wl_input_0051-data_atual  TO tl_input_0054-data_atual,
            wl_input_0051-hora_atual  TO tl_input_0054-hora_atual.

      APPEND tl_input_0054.
      CLEAR:tl_input_0054.
    ENDLOOP.

* Adiantamento Externo
    LOOP AT tg_adto_ext.
      MOVE-CORRESPONDING: tg_adto_ext TO tl_input_0063.
      MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0063-nro_sol_ov,
            wl_input_0051-usnam       TO tl_input_0063-usnam,
            wl_input_0051-data_atual  TO tl_input_0063-data_atual,
            wl_input_0051-hora_atual  TO tl_input_0063-hora_atual.

      APPEND tl_input_0063.
      CLEAR:tl_input_0063.
    ENDLOOP.

    IF ( wg_header-param_espec NE c_a AND
*-CS2021000615 - 17.06.2021 - JT - inicio
         wg_header-param_espec NE c_ax ).
*-CS2021000615 - 17.06.2021 - JT - fim
* Instrução
****************************************************************************
      LOOP AT tg_instrucao.
        MOVE-CORRESPONDING: tg_instrucao TO tl_input_0045.
        MOVE: tg_instrucao-quantidade  TO tl_input_0045-quantidade.
        IF tl_input_0045-zseq_inst IS INITIAL.
          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr = '01'
              object      = 'ZSEQ_INST'
            IMPORTING
              number      = tl_input_0045-zseq_inst.
        ENDIF.

        MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0045-objek,
              'ZSDT0051'                TO tl_input_0045-objecttable,
              wl_input_0051-usnam       TO tl_input_0045-usuario,
              wl_input_0051-data_atual  TO tl_input_0045-data_criacao.
*            WL_INPUT_0051-HORA_ATUAL  TO TL_INPUT_0063-HORA_ATUAL.

        APPEND tl_input_0045.
        CLEAR:tl_input_0045.
      ENDLOOP.

***************************************************************************

* Formação de Lote
      LOOP AT tg_form_lote.
        MOVE-CORRESPONDING: tg_form_lote TO tl_input_0066.
        MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0066-nro_sol_ov,
              wl_input_0051-usnam       TO tl_input_0066-usnam,
              wl_input_0051-data_atual  TO tl_input_0066-data_atual,
              wl_input_0051-hora_atual  TO tl_input_0066-hora_atual.

        IF tg_form_lote-status IS INITIAL
        OR tg_form_lote-status EQ icon_initial.
          MOVE:  c_a                       TO tl_input_0066-status.
          MOVE:  c_z                       TO tl_input_0066-status." CS2020000373 23/08/2022 -LP
        ELSEIF tg_form_lote-status EQ icon_release.
          MOVE:  c_l                       TO tl_input_0066-status.
        ELSEIF tg_form_lote-status EQ icon_defect.
          MOVE:  c_d                       TO tl_input_0066-status.
        ENDIF.

        APPEND tl_input_0066.
        CLEAR:tl_input_0066.
      ENDLOOP.

    ENDIF.

* Logistica
    LOOP AT tg_logistica.
      MOVE-CORRESPONDING: tg_logistica TO tl_input_0055.
      IF ( tl_input_0055-id IS INITIAL ).
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZSEQ_LOG'
          IMPORTING
            number      = tl_input_0055-id.

      ENDIF.

      MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0055-nro_sol_ov,
            wl_input_0051-usnam       TO tl_input_0055-usnam,
            wl_input_0051-data_atual  TO tl_input_0055-data_atual,
            wl_input_0051-hora_atual  TO tl_input_0055-hora_atual.

      IF ( wg_header-risco_sacado EQ 'N' ).
        CLEAR: tl_input_0055-valdt_hedge.
      ENDIF.

      APPEND tl_input_0055.
      CLEAR:tl_input_0055.
    ENDLOOP.

* Pedido
    LOOP AT tg_pedido.
      MOVE-CORRESPONDING: tg_pedido TO tl_input_0184.

      MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0184-nro_sol_ov,
            wl_input_0051-usnam       TO tl_input_0184-usnam,
            wl_input_0051-data_atual  TO tl_input_0184-data_atual,
            wl_input_0051-hora_atual  TO tl_input_0184-hora_atual.

      APPEND tl_input_0184.
      CLEAR: tl_input_0184.
    ENDLOOP.

* Preço
    UNASSIGN <fs_line>.

    DATA lv_posnr TYPE posnr.
    DATA lv_value TYPE p DECIMALS 2.

    LOOP AT <fs_table> ASSIGNING <fs_line>.

      CLEAR: wg_valor.

      PERFORM get_set_valores USING 'COD_FP' 'G' CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.

      " 09.05.2024 - RAMON -->
      PERFORM get_set_valores USING 'POSNR' 'G' CHANGING lv_posnr.
      CONDENSE lv_posnr NO-GAPS.
      " 09.05.2024 - RAMON --<

      LOOP AT tg_preco_n
        WHERE cod_fp EQ wg_valor.

        MOVE-CORRESPONDING: tg_preco_n TO tl_input_0059.

*      MOVE: TG_PRECO-FORMULA2     TO TL_INPUT_0059-FORMULA2.
        MOVE: tg_preco_n-formula        TO tl_input_0059-formula.
        MOVE: tg_preco_n-ocbot          TO tl_input_0059-ocbot.
        MOVE: tg_preco_n-preco          TO tl_input_0059-preco.
        MOVE: tg_preco_n-c_decimais     TO tl_input_0059-c_decimais.
        MOVE: wl_input_0051-nro_sol_ov  TO tl_input_0059-nro_sol_ov,
              wl_input_0051-usnam       TO tl_input_0059-usnam,
              wl_input_0051-data_atual  TO tl_input_0059-data_atual,
              wl_input_0051-hora_atual  TO tl_input_0059-hora_atual.


        CLEAR: wl_valor.
        PERFORM get_set_valores USING tg_preco_n-field
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.

        tl_input_0059-formula2 = wl_valor.

        TRANSLATE tl_input_0059-formula2 USING '. '.
        TRANSLATE tl_input_0059-formula2 USING ',.'.
        CONDENSE tl_input_0059-formula2  NO-GAPS.

        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'WAERS'
                                      'G'
                             CHANGING  wl_valor.
        CONDENSE wl_valor NO-GAPS.
        tl_input_0059-waers = wl_valor.

        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'INVISIBLE'
                                      'G'
                             CHANGING  wl_valor.
        CONDENSE wl_valor NO-GAPS.
        tl_input_0059-invisible = wl_valor.

        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'CBOT'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.

        tl_input_0059-cbot = wl_valor.

* RJF
        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'SAFRA'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.

        tl_input_0059-safra = wl_valor.

        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'MONAT'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.

        tl_input_0059-monat = wl_valor.

        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'VALDT'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.

        tl_input_0059-valdt = wl_valor.

        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'POSNR'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.
        tl_input_0059-posnr = wl_valor.


        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'POSNR1'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.
        tl_input_0059-posnr1 = wl_valor.


        CLEAR: wl_valor.
        PERFORM get_set_valores USING 'VALDT_HEDGE'
                                      'G'
                             CHANGING wl_valor.
        CONDENSE wl_valor NO-GAPS.
        tl_input_0059-valdt_hedge = wl_valor.


        " 10.05.2024 - RAMON --->>
        IF lv_posnr IS NOT INITIAL.

          IF tl_input_0059-tipo_calc = 'V'.
            lv_value = tl_input_0059-formula2.
            WRITE lv_value TO tl_input_0059-formula NO-GAP LEFT-JUSTIFIED.
          ENDIF.

        ENDIF.
        " 10.05.2024 - RAMON --<<

        APPEND tl_input_0059.
        CLEAR:tl_input_0059.
      ENDLOOP.
    ENDLOOP.



** Posição Financeira
    IF wg_acao EQ c_add.

      MOVE: wg_header-nro_sol_ov       TO wl_input_0084-nro_sol_ov,
            wg_posicao-vlr_f_vencida   TO wl_input_0084-vlr_vencido_brl,
            wg_posicao-vlr_f_avencer   TO wl_input_0084-vlr_avencer_brl,
            wg_posicao-vlr_f_adiant    TO wl_input_0084-vlr_adiant_brl,
            wg_posicao-vlr_limite      TO wl_input_0084-limite_credito,
            wg_posicao-vlr_total       TO wl_input_0084-total_brl,
            wg_posicao-vlr_total_mov   TO wl_input_0084-total_mov_brl,
            wg_posicao-vlr_saldo       TO wl_input_0084-saldo_disp_brl,
            wg_posicao-utilizado       TO wl_input_0084-util_limit_brl,
            wg_posicao-sdo_ov_emit     TO wl_input_0084-sdo_ov_emit,
            sy-uname                   TO wl_input_0084-usnam,
            sy-datum                   TO wl_input_0084-data_atual,
            sy-uzeit                   TO wl_input_0084-hora_atual.

*      MODIFY ZSDT0084 FROM WL_INPUT_0084.
    ENDIF.
    tg_0059[] = tl_input_0059[].

    DELETE FROM zsdt0051 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0052 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0053 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0054 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0055 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0059 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0063 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0073 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0184 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.

    MODIFY zsdt0051 FROM wl_input_0051.
    MODIFY zsdt0052 FROM wl_input_0052.
    MODIFY zsdt0053 FROM TABLE tl_input_0053.
    MODIFY zsdt0054 FROM TABLE tl_input_0054.
    MODIFY zsdt0055 FROM TABLE tl_input_0055.

    " 29.04.2024 - 121095 - RBL -->
    PERFORM f_preco_redist TABLES tl_input_0059[].
    PERFORM f_zsdt0059_redist TABLES tl_input_0059[].
    " 29.04.2024 - 121095 - RBL --<

    MODIFY zsdt0059 FROM TABLE tl_input_0059.
    MODIFY zsdt0063 FROM TABLE tl_input_0063.
    MODIFY zsdt0073 FROM TABLE tl_input_0073.
    MODIFY zsdt0184 FROM TABLE tl_input_0184.

    "SD - Persistencia Inst.Form Lote ZSDT0062 IR199276 - WPP - Ini
*    IF wg_header-param_espec NE c_a AND
**-CS2021000615 - 17.06.2021 - JT - inicio
*       wg_header-param_espec NE c_ax AND
*       wg_header-param_espec NE c_z.  ""Não alterar instrução ao salvar solicitação
**-CS2021000615 - 17.06.2021 - JT - fim
*      DELETE FROM zsdt0066 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov AND status NE 'D'.
*      DELETE FROM zsdt0045 WHERE objecttable EQ 'ZSDT0051'
*                             AND objek       EQ wl_input_0051-nro_sol_ov.
*      MODIFY zsdt0066 FROM TABLE tl_input_0066.
*      MODIFY zsdt0045 FROM TABLE tl_input_0045.
*    ENDIF.
    "SD - Persistencia Inst.Form Lote ZSDT0062 IR199276 - fim

  ELSEIF wg_acao EQ c_modif_c_ov.

*****************    INICIO   *****************
***************** C_MODIF_C_OV *****************

    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
      UPDATE zsdt0051 SET observacao = wg_header-observacao
                          job        = abap_false
                          usnam      = sy-uname
                          data_atual = sy-datum
                          hora_atual = sy-uzeit
                      WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
    ENDIF.
* Condição de pagamento
    MOVE-CORRESPONDING: wg_cond_pgt TO wl_input_0052.
    MOVE: wl_0051-nro_sol_ov  TO wl_input_0052-nro_sol_ov,
          sy-uname      TO wl_input_0052-usnam,
          sy-datum      TO wl_input_0052-data_atual,
          sy-uzeit      TO wl_input_0052-hora_atual.


*** Stefanini - IR238549 - 24/06/2025 - FINC - Início de Alteração
    IF wl_0051-nro_sol_ov IS NOT INITIAL.
      SELECT nro_sol_ov, posnr, docnum_rt
        INTO TABLE @DATA(lt_zsdt0053_rt)
        FROM zsdt0053
       WHERE nro_sol_ov EQ @wl_0051-nro_sol_ov.
    ENDIF.
*** Stefanini - IR238549 - 24/06/2025 - FINC - Fim de Alteração


* Itens
    LOOP AT tg_itens WHERE vbeln IS INITIAL.
      IF tg_itens-item_edit EQ c_r.
        CLEAR: tg_itens-item_edit.
      ENDIF.
      MOVE-CORRESPONDING: tg_itens TO tl_input_0053.
      MOVE: wl_0051-nro_sol_ov         TO tl_input_0053-nro_sol_ov,
            sy-uname                   TO tl_input_0053-usnam,
            sy-datum                   TO tl_input_0053-data_atual,
            sy-uzeit                   TO tl_input_0053-hora_atual,
            tg_itens-dmbtr             TO tl_input_0053-dmbtr,
            tg_itens-vlrtot            TO tl_input_0053-vlrtot,
            space                      TO tl_input_0053-status.

*** Stefanini - IR238549 - 24/06/2025 - FINC - Início de Alteração
      READ TABLE lt_zsdt0053_rt INTO DATA(ls_zsdt0053_rt) WITH KEY nro_sol_ov = wl_0051-nro_sol_ov
                                                                   posnr      = tg_itens-posnr.
      IF sy-subrc IS INITIAL.
        MOVE: ls_zsdt0053_rt-docnum_rt TO tl_input_0053-docnum_rt.
      ENDIF.
*** Stefanini - IR238549 - 24/06/2025 - FINC - Fim de Alteração

      APPEND tl_input_0053.
      CLEAR:tl_input_0053.
    ENDLOOP.

    IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
       wg_header-param_espec EQ c_ax OR
      wg_header-param_espec EQ c_z." CS2020000373 23/08/2022 -LP
*-CS2021000615 - 17.06.2021 - JT - fim
      LOOP AT tg_itens .

        IF tg_itens-item_edit EQ c_r.
          CLEAR: tg_itens-item_edit.
        ENDIF.

        UPDATE   zsdt0053
          SET volum = tg_itens-volum
        WHERE nro_sol_ov = wl_0051-nro_sol_ov
        AND   posnr = tg_itens-posnr.


      ENDLOOP.


    ENDIF.


* Pagamento Antecipado
    IF wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
       wg_header-param_espec EQ c_ax OR
      wg_header-param_espec EQ c_z.
*-CS2021000615 - 17.06.2021 - JT - fim
      LOOP AT tg_pgt_ant.
        MOVE-CORRESPONDING: tg_pgt_ant TO tl_input_0054.
        MOVE: wg_header-nro_sol_ov      TO tl_input_0054-nro_sol_ov,
              sy-uname                  TO tl_input_0054-usnam,
              sy-datum                  TO tl_input_0054-data_atual,
              sy-uzeit                  TO tl_input_0054-hora_atual.

        APPEND tl_input_0054.
        CLEAR:tl_input_0054.
      ENDLOOP.

      DELETE FROM zsdt0054 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
      MODIFY zsdt0054 FROM TABLE tl_input_0054.

    ENDIF.

*-CS2022000332-#78223-02.06.2022-JT-inicio
    IF ( wg_header-param_espec EQ c_a OR  wg_header-param_espec  EQ c_z ).
      LOOP AT tg_adto_ext.
        MOVE-CORRESPONDING: tg_adto_ext TO tl_input_0063.
        MOVE: wg_header-nro_sol_ov  TO tl_input_0063-nro_sol_ov,
              sy-uname              TO tl_input_0063-usnam,
              sy-datum              TO tl_input_0063-data_atual,
              sy-uzeit              TO tl_input_0063-hora_atual.

        APPEND tl_input_0063.
        CLEAR:tl_input_0063.
      ENDLOOP.
      DELETE FROM zsdt0063 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
      MODIFY zsdt0063 FROM TABLE tl_input_0063.
    ENDIF.
*-CS2022000332-#78223-02.06.2022-JT-fim

* Condições especiais
    LOOP AT tg_cond_esp.
      MOVE-CORRESPONDING: tg_cond_esp TO tl_input_0073.
      MOVE: wg_header-nro_sol_ov      TO tl_input_0073-nro_sol_ov,
            sy-uname                  TO tl_input_0073-usnam,
            sy-datum                  TO tl_input_0073-data_atual,
            sy-uzeit                  TO tl_input_0073-hora_atual.

      APPEND tl_input_0073.
      CLEAR:tl_input_0073.
    ENDLOOP.

    "SD - Persistencia Inst.Form Lote ZSDT0062 IR199276 - WPP  - Ini
*    IF wg_header-param_espec NE c_a AND
**-CS2021000615 - 17.06.2021 - JT - inicio
*       wg_header-param_espec NE c_ax.
**-CS2021000615 - 17.06.2021 - JT - fim
** Formação de Lote
*      LOOP AT tg_form_lote." WHERE VBELN EQ SPACE.
*        MOVE-CORRESPONDING: tg_form_lote TO tl_input_0066.
*        MOVE: wl_0051-nro_sol_ov         TO tl_input_0066-nro_sol_ov,
*              sy-uname                   TO tl_input_0066-usnam,
*              sy-datum                   TO tl_input_0066-data_atual,
*              sy-uzeit                   TO tl_input_0066-hora_atual.
*
*        IF tg_form_lote-status IS INITIAL
*        OR tg_form_lote-status EQ icon_initial.
*          MOVE:  c_a                       TO tl_input_0066-status.
*          MOVE:  c_z                       TO tl_input_0066-status.
*        ELSEIF tg_form_lote-status EQ icon_release.
*          MOVE:  c_l                       TO tl_input_0066-status.
*        ELSEIF tg_form_lote-status EQ icon_defect.
*          MOVE:  c_d                       TO tl_input_0066-status.
*        ENDIF.
*
*        APPEND tl_input_0066.
*        CLEAR:tl_input_0066.
*      ENDLOOP.
*
*      DELETE FROM zsdt0066 WHERE nro_sol_ov EQ wl_0051-nro_sol_ov
*                                 AND status NE 'D'.
*      MODIFY zsdt0066 FROM TABLE tl_input_0066.
*    ENDIF.
    "SD - Persistencia Inst.Form Lote ZSDT0062 IR199276 - WPP  - Fim

* Pedido
    IF ( wg_header-param_espec EQ c_u ).
      LOOP AT tg_pedido.
        MOVE-CORRESPONDING tg_pedido TO tl_input_0184.
        MOVE: sy-uname               TO tl_input_0184-usnam,
              sy-datum               TO tl_input_0184-data_atual,
              sy-uzeit               TO tl_input_0184-hora_atual.

        APPEND tl_input_0184.
        CLEAR: tl_input_0184.
      ENDLOOP.

      DELETE FROM zsdt0184 WHERE nro_sol_ov EQ wl_0051-nro_sol_ov
                                 AND status NE 'D'.
      MODIFY zsdt0184 FROM TABLE tl_input_0184.

    ENDIF.

* Preço
    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.

      UNASSIGN <fs_line>.
      LOOP AT <fs_table> ASSIGNING <fs_line>.
        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'COD_FP'
                                      'G'
                             CHANGING wg_valor.
        CONDENSE wg_valor NO-GAPS.

        " 28.05.2024 - ramon -->
        CLEAR: wg_valor_aux.
        PERFORM get_set_valores USING 'BEZEI'
                                      'G'
                             CHANGING wg_valor_aux.

        " 28.05.2024 - ramon --<

        LOOP AT tg_preco_n
          WHERE cod_fp EQ wg_valor.

          " 28.05.2024 - ramon -->
          CHECK wg_valor_aux = tg_preco_n-bezei.
          " 28.05.2024 - ramon --<


          MOVE-CORRESPONDING: tg_preco_n TO tl_input_0059.

*      MOVE: TG_PRECO-FORMULA2     TO TL_INPUT_0059-FORMULA2.
          MOVE: tg_preco_n-formula        TO tl_input_0059-formula.
          MOVE: tg_preco_n-ocbot          TO tl_input_0059-ocbot.
          MOVE: tg_preco_n-preco          TO tl_input_0059-preco.
          MOVE: tg_preco_n-c_decimais     TO tl_input_0059-c_decimais.
          MOVE: wl_0051-nro_sol_ov        TO tl_input_0059-nro_sol_ov,
                sy-uname                  TO tl_input_0059-usnam,
                sy-datum                  TO tl_input_0059-data_atual,
                sy-uzeit                  TO tl_input_0059-hora_atual.


          CLEAR: wl_valor.
          PERFORM get_set_valores USING tg_preco_n-field
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-formula2 = wl_valor.

          IF tl_input_0059-tipo_calc EQ 'V'.
            tl_input_0059-formula = tl_input_0059-formula2.
          ENDIF.

          TRANSLATE tl_input_0059-formula2 USING '. '.
          TRANSLATE tl_input_0059-formula2 USING ',.'.
          CONDENSE tl_input_0059-formula2  NO-GAPS.


          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'WAERS'
                                        'G'
                               CHANGING  wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-waers = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'CBOT'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.

          tl_input_0059-cbot = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'MONAT'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.

          tl_input_0059-monat = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'VALDT'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.

          tl_input_0059-valdt = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'POSNR'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-posnr = wl_valor.


          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'POSNR1'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-posnr1 = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'VALDT_HEDGE'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-valdt_hedge = wl_valor.

          " 04.12.2023 - 128467 - RBL -->
          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'SAFRA'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-safra = wl_valor.

          " 04.12.2023 - 128467 - RBL --<

          " 28.05.2024 - 121095 - RBL -->
          " codigo adicionado, pq o campo refist precisa estar
          " preenchido quando é registro vindo da redistribuição
          "IF lv_redist = abap_true.

          CLEAR wl_valor.

          PERFORM get_set_valores
            USING 'REDIST' 'G'
         CHANGING wl_valor.

          IF wl_valor = abap_true.

            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-redist = wl_valor.

            PERFORM get_set_valores
              USING 'BEZEI' 'G'
           CHANGING wl_valor.

            tl_input_0059-bezei = wl_valor.

          ENDIF.

          "ENDIF.

          " 28.05.2024 - 121095 - RBL --<


          APPEND tl_input_0059.

          CLEAR:tl_input_0059.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    DELETE FROM zsdt0052 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0053 WHERE nro_sol_ov EQ wl_0051-nro_sol_ov
                           AND vbeln      EQ space.
    DELETE FROM zsdt0059 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.
    DELETE FROM zsdt0073 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.

    IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
      MODIFY zsdt0051 FROM wl_input_0051.
    ENDIF.

* Logistica
    IF wg_header-inco1 = 'CIF' OR wg_header-inco1 = 'CPT'. "ALRS
      LOOP AT tg_logistica.
        MOVE-CORRESPONDING: tg_logistica TO tl_input_0055.
        MOVE: wg_header-nro_sol_ov  TO tl_input_0055-nro_sol_ov,
              sy-uname              TO tl_input_0055-usnam,
              sy-datum              TO tl_input_0055-data_atual,
              sy-uzeit              TO tl_input_0055-hora_atual.

        APPEND tl_input_0055.
        CLEAR:tl_input_0055.
      ENDLOOP.
      DELETE FROM zsdt0055 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
      MODIFY zsdt0055 FROM TABLE tl_input_0055.
    ENDIF.

    "SD - Persistencia Inst.Form Lote ZSDT0062 IR199276 - WPP - Ini
*    IF wg_header-param_espec NE c_a AND
**-CS2021000615 - 17.06.2021 - JT - inicio
*       wg_header-param_espec NE c_ax.
**-CS2021000615 - 17.06.2021 - JT - fim
** Instrução
*      LOOP AT tg_instrucao.
*        MOVE-CORRESPONDING: tg_instrucao TO tl_input_0045.
*        MOVE: tg_instrucao-quantidade  TO tl_input_0045-quantidade.
*        IF tl_input_0045-zseq_inst IS INITIAL.
*          CALL FUNCTION 'NUMBER_GET_NEXT'
*            EXPORTING
*              nr_range_nr = '01'
*              object      = 'ZSEQ_INST'
*            IMPORTING
*              number      = tl_input_0045-zseq_inst.
*        ENDIF.
*
*        MOVE: wg_header-nro_sol_ov      TO tl_input_0045-objek,
*              'ZSDT0051'                TO tl_input_0045-objecttable,
*              wl_input_0051-usnam       TO tl_input_0045-usuario,
*              wl_input_0051-data_atual  TO tl_input_0045-data_criacao.
**            WL_INPUT_0051-HORA_ATUAL  TO TL_INPUT_0063-HORA_ATUAL.
*        MOVE: tg_instrucao-tp_fardo TO tl_input_0045-tamanho_fardo.
*        APPEND tl_input_0045.
*        CLEAR:tl_input_0045.
*      ENDLOOP.
*
*      DELETE FROM zsdt0045 WHERE objecttable EQ 'ZSDT0051'
*                             AND objek       EQ wg_header-nro_sol_ov.
*      MODIFY zsdt0045 FROM TABLE tl_input_0045.
*
*    ENDIF.
    "SD - Persistencia Inst.Form Lote ZSDT0062 IR199276 - WPP - Fim

    MODIFY zsdt0052 FROM wl_input_0052.
    MODIFY zsdt0053 FROM TABLE tl_input_0053.


    " 14.02.2024 - 121095 - RBL -->
    "IF lv_redist = abap_true.
    PERFORM f_preco_redist TABLES tl_input_0059[].
    PERFORM f_zsdt0059_redist TABLES tl_input_0059[].
    "ENDIF.

    " 14.02.2024 - 121095 - RBL --<


    MODIFY zsdt0059 FROM TABLE tl_input_0059.
    MODIFY zsdt0073 FROM TABLE tl_input_0073.

*****************      FIM     *****************
***************** C_MODIF_C_OV *****************

  ELSE.

*****************    INICIO ELSE    *****************

    IF wg_redistribuir IS INITIAL.
      UPDATE zsdt0051 SET observacao = wg_header-observacao
                          usnam      = sy-uname
                          data_atual = sy-datum
                          hora_atual = sy-uzeit
                      WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
    ENDIF.

*    IF WG_REDISTRIBUIR IS INITIAL.
* Pagamento Antecipado
    LOOP AT tg_pgt_ant.
      MOVE-CORRESPONDING: tg_pgt_ant TO tl_input_0054.
      MOVE: wg_header-nro_sol_ov      TO tl_input_0054-nro_sol_ov,
            sy-uname                  TO tl_input_0054-usnam,
            sy-datum                  TO tl_input_0054-data_atual,
            sy-uzeit                  TO tl_input_0054-hora_atual.

      APPEND tl_input_0054.
      CLEAR:tl_input_0054.
    ENDLOOP.
    DELETE FROM zsdt0054 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
    MODIFY zsdt0054 FROM TABLE tl_input_0054.
*    ENDIF.

    SELECT *
      FROM zsdt0053
      INTO TABLE tl_0053
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

    IF sy-subrc IS INITIAL.
* Itens
      LOOP AT tg_itens.
*        WHERE VBELN IS NOT INITIAL.
*      if tg_itens-vbeln is not initial.
        MOVE-CORRESPONDING: tg_itens TO tl_input_0053.
        MOVE: wg_header-nro_sol_ov      TO tl_input_0053-nro_sol_ov,
              sy-uname                  TO tl_input_0053-usnam,
              sy-datum                  TO tl_input_0053-data_atual,
              sy-uzeit                  TO tl_input_0053-hora_atual,
              tg_itens-dmbtr            TO tl_input_0053-dmbtr,
              tg_itens-vlrtot           TO tl_input_0053-vlrtot,
              tg_itens-vbeln            TO tl_input_0053-vbeln,
              space                     TO tl_input_0053-status.

        READ TABLE tl_0053
          WITH KEY posnr = tg_itens-posnr
                   vbeln = tg_itens-vbeln.

        IF sy-subrc IS INITIAL
        AND ( tl_0053-zmeng NE tl_input_0053-zmeng
           OR tl_0053-valdt NE tl_input_0053-valdt ).

          APPEND tl_input_0053.
        ELSEIF tg_itens-item_edit EQ c_r.
          IF wg_redistribuir IS NOT INITIAL.
            CLEAR: tl_input_0053-item_edit.
            APPEND tl_input_0053 TO tl_input_0053_aux.
          ENDIF.
        ENDIF.
        CLEAR:tl_input_0053.
      ENDLOOP.
    ENDIF.

    IF wg_header-param_espec NE c_a AND
*-CS2021000615 - 17.06.2021 - JT - inicio
       wg_header-param_espec NE c_ax.
*-CS2021000615 - 17.06.2021 - JT - fim
      SELECT *
      FROM zsdt0066
      INTO TABLE tl_0066
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov
        AND status NE 'D'.
      IF sy-subrc IS INITIAL.
* Itens formacao de lote
        LOOP AT tg_form_lote.
*      if tg_itens-vbeln is not initial.
          MOVE-CORRESPONDING: tg_form_lote TO tl_input_0066.
          MOVE: wg_header-nro_sol_ov      TO tl_input_0066-nro_sol_ov,
                sy-uname                  TO tl_input_0066-usnam,
                sy-datum                  TO tl_input_0066-data_atual,
                sy-uzeit                  TO tl_input_0066-hora_atual,
                tg_form_lote-dmbtr        TO tl_input_0066-dmbtr,
                tg_form_lote-vlrtot       TO tl_input_0066-vlrtot,
                tg_form_lote-vbeln        TO tl_input_0066-vbeln,
                space                     TO tl_input_0066-status.

          READ TABLE tl_0066
            WITH KEY posnr = tg_form_lote-posnr
                     vbeln = tg_form_lote-vbeln.

          IF sy-subrc IS INITIAL
          AND ( tl_0066-zmeng NE tl_input_0066-zmeng
           OR   tl_0066-volum NE tl_input_0066-volum  ).
            APPEND tl_input_0066.
          ENDIF.
          CLEAR:tl_input_0066.
        ENDLOOP.
      ENDIF.
    ENDIF.

    PERFORM atualiza_ov TABLES tl_input_0053
                               tl_input_0066.

    IF wg_redistribuir IS INITIAL.
      UPDATE zsdt0051 SET job        = abap_false
                          usnam      = sy-uname
                          data_atual = sy-datum
                          hora_atual = sy-uzeit
                      WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
    ENDIF.

    LOOP AT tl_input_0053.
      IF wg_redistribuir IS INITIAL.
        UPDATE zsdt0053 SET zmeng     = tl_input_0053-zmeng
                           valdt      = tl_input_0053-valdt
                           vlrtot     = tl_input_0053-vlrtot
                           usnam      = sy-uname
                           data_atual = sy-datum
                           hora_atual = sy-uzeit
         WHERE nro_sol_ov EQ tl_input_0053-nro_sol_ov
           AND posnr      EQ tl_input_0053-posnr.
      ELSE.
        UPDATE zsdt0053 SET zmeng     = tl_input_0053-zmeng
                       vlrtot     = tl_input_0053-vlrtot
                       kunnr      = tl_input_0053-kunnr
                       usnam      = sy-uname
                       data_atual = sy-datum
                       hora_atual = sy-uzeit
     WHERE nro_sol_ov EQ tl_input_0053-nro_sol_ov
       AND posnr      EQ tl_input_0053-posnr.
      ENDIF.
    ENDLOOP.

    IF wg_header-param_espec NE c_a AND
*-CS2021000615 - 17.06.2021 - JT - inicio
       wg_header-param_espec NE c_ax.
*-CS2021000615 - 17.06.2021 - JT - fim
      LOOP AT tl_input_0066.
        IF wg_redistribuir IS INITIAL.
          UPDATE zsdt0066 SET zmeng     = tl_input_0066-zmeng
                             volum      = tl_input_0066-volum
                             vlrtot     = tl_input_0066-vlrtot
                             usnam      = sy-uname
                             data_atual = sy-datum
                             hora_atual = sy-uzeit
           WHERE nro_sol_ov EQ tl_input_0066-nro_sol_ov
             AND posnr      EQ tl_input_0066-posnr.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF wg_redistribuir IS NOT INITIAL
    AND tl_input_0053[] IS NOT INITIAL
   AND  wg_header-param_espec EQ c_m." OR  wg_header-param_espec EQ c_z.
* Condições especiais
      LOOP AT tg_cond_esp.
        MOVE-CORRESPONDING: tg_cond_esp TO tl_input_0073.
        MOVE:  wg_header-nro_sol_ov     TO tl_input_0073-nro_sol_ov,
              sy-uname                  TO tl_input_0073-usnam,
              sy-datum                  TO tl_input_0073-data_atual,
              sy-uzeit                  TO tl_input_0073-hora_atual.

        APPEND tl_input_0073.
        CLEAR:tl_input_0073.
      ENDLOOP.
      DELETE FROM zsdt0073 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
      MODIFY zsdt0073 FROM TABLE tl_input_0073.

      UPDATE zsdt0051 SET num_fixacao = wg_header-num_fixacao
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

      UNASSIGN <fs_line>.
      LOOP AT <fs_table> ASSIGNING <fs_line>.

        " 22.02.2024 - RAMON -->
        PERFORM get_set_valores
          USING 'REDIST' 'G'
       CHANGING wg_valor.

        CHECK wg_valor = space.
        " 22.02.2024 - RAMON --<

        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'COD_FP'
                                      'G'
                             CHANGING wg_valor.
        CONDENSE wg_valor NO-GAPS.

        " 21.02.2024 - ramon -->
        CLEAR: wg_valor_aux.
        PERFORM get_set_valores USING 'BEZEI'
                                      'G'
                             CHANGING wg_valor_aux.

        " 21.02.2024 - ramon -->

        LOOP AT tg_preco_n
          WHERE cod_fp EQ wg_valor.

          " 21.02.2024 - ramon -->
          CHECK wg_valor_aux = tg_preco_n-bezei.
          " 21.02.2024 - ramon --<

          MOVE-CORRESPONDING: tg_preco_n TO tl_input_0059.

          MOVE: tg_preco_n-formula        TO tl_input_0059-formula.
          MOVE: tg_preco_n-ocbot          TO tl_input_0059-ocbot.
          MOVE: tg_preco_n-preco          TO tl_input_0059-preco.
          MOVE: tg_preco_n-c_decimais     TO tl_input_0059-c_decimais.
          MOVE: wl_0051-nro_sol_ov        TO tl_input_0059-nro_sol_ov,
                sy-uname                  TO tl_input_0059-usnam,
                sy-datum                  TO tl_input_0059-data_atual,
                sy-uzeit                  TO tl_input_0059-hora_atual.


          CLEAR: wl_valor.
          PERFORM get_set_valores USING tg_preco_n-field
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-formula2 = wl_valor.

          IF tl_input_0059-tipo_calc EQ 'V'.
            tl_input_0059-formula = tl_input_0059-formula2.
          ENDIF.

          TRANSLATE tl_input_0059-formula2 USING '. '.
          TRANSLATE tl_input_0059-formula2 USING ',.'.
          CONDENSE tl_input_0059-formula2  NO-GAPS.


          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'WAERS'
                                        'G'
                               CHANGING  wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-waers = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'CBOT'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.

          tl_input_0059-cbot = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'MONAT'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.

          tl_input_0059-monat = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'VALDT'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.

          tl_input_0059-valdt = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'POSNR'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-posnr = wl_valor.

          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'POSNR1'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-posnr1 = wl_valor.


          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'VALDT_HEDGE'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-valdt_hedge = wl_valor.

          " 04.12.2023 - 128467 - RBL -->
          CLEAR: wl_valor.
          PERFORM get_set_valores USING 'SAFRA'
                                        'G'
                               CHANGING wl_valor.
          CONDENSE wl_valor NO-GAPS.
          tl_input_0059-safra = wl_valor.

          " 04.12.2023 - 128467 - RBL --<

          " 14.02.2024 - 121095 - RBL -->
          "IF lv_redist = abap_true.

          CLEAR wl_valor.

          PERFORM get_set_valores
            USING 'REDIST' 'G'
         CHANGING wl_valor.

          IF wl_valor = abap_true.

            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-redist = wl_valor.

            PERFORM get_set_valores
              USING 'BEZEI' 'G'
           CHANGING wl_valor.

            tl_input_0059-bezei = wl_valor.

          ENDIF.

          "ENDIF.

          " 14.02.2024 - 121095 - RBL --<

          APPEND tl_input_0059.
          CLEAR:tl_input_0059.

        ENDLOOP.
      ENDLOOP.

      DELETE FROM zsdt0059 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.

      " 14.02.2024 - 121095 - RBL -->
      "IF lv_redist = abap_true.
      PERFORM f_preco_redist TABLES tl_input_0059[].
      PERFORM f_zsdt0059_redist TABLES tl_input_0059[].
      "ENDIF.

      " 14.02.2024 - 121095 - RBL --<

      MODIFY zsdt0059 FROM TABLE tl_input_0059.

      PERFORM input_monat. " INSERE OS MONAT NA TABELA 59 PARA C AND P

*   COPIA A DATA DO VALDT PARA VALDT_HEDGE QUANDO FOR REDISTRIBUIÇÃO E PARAMETRO ESPECIAL 'M' SOMENTE QUANDO SALVAR.
      IF ( wg_header-param_espec EQ 'M' OR wg_header-param_espec EQ 'Z'  ) AND ( wg_redistribuir IS NOT INITIAL ).

        UNASSIGN <fs_line>. " 21.05.2024

        LOOP AT <fs_table> ASSIGNING <fs_line>.

          " 21.02.2024 - RAMON -->
          CLEAR wg_valor.
          PERFORM get_set_valores USING 'REDIST' 'G' CHANGING wg_valor.
          CHECK wg_valor = abap_false.
          " 21.02.2024 - RAMON --<

          var_tabix59 = sy-tabix.
          CLEAR wg_valor.PERFORM get_set_valores USING 'VALDT' 'G' CHANGING wg_valor.
          IF ( wg_valor NE '00000000' ).
*            CLEAR WG_VALOR.PERFORM GET_SET_VALORES USING 'FIELD' 'G' CHANGING WG_VALOR. " Realizar o processo para todos os FIELDs
*            IF WG_VALOR EQ QTD_FIXA.
            CLEAR wg_valor.PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
            var_len59 = strlen( wg_valor ).
            IF ( var_len59 EQ 2 ) OR ( var_len59 EQ 3 ).
              IF (  wg_valor(1) EQ 'T' ).
                CLEAR wg_valor.PERFORM get_set_valores USING 'VALDT_HEDGE' 'G' CHANGING wg_valor.
                IF ( wg_valor EQ '00000000' ).
                  CLEAR wg_valor.PERFORM get_set_valores USING 'VALDT' 'G' CHANGING wg_valor.
                  CONDENSE wg_valor NO-GAPS.
                  PERFORM get_set_valores USING   'VALDT_HEDGE' 'S' CHANGING wg_valor.
                  MODIFY <fs_table> FROM <fs_line> INDEX var_tabix59.
                ENDIF.
              ENDIF.
            ENDIF.
*            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.


    "Caso seja modificação.
    IF NOT ( var_modred IS INITIAL ).

*     Quando PERFORM ATUALIZA_OV Retorna vazio a tabela da 53,
*     significa que ocorreu algum erro na atualização da Ordem.
*     e não deve gravar os dados na zsdt0059
      IF NOT tl_input_0053[] IS INITIAL.

        PERFORM input_monat. " INSERE OS MONAT NA TABELA 59 PARA C AND P

        UNASSIGN <fs_line>.

        LOOP AT <fs_table> ASSIGNING <fs_line>.

          PERFORM get_set_valores USING 'QTDFIXADA'
                                  'G' CHANGING var_valor_char.

          TRANSLATE var_valor_char USING '. '.
          TRANSLATE var_valor_char USING ',.'.
          CONDENSE var_valor_char NO-GAPS.

          p_2 = var_valor_char.
          WRITE p_2 TO f_g.
          CONDENSE f_g NO-GAPS.

          PERFORM get_set_valores USING   'QTDFIXADA'
                                          'S'
                                 CHANGING f_g.

        ENDLOOP.

        LOOP AT <fs_table> ASSIGNING <fs_line>.

          " 22.02.2024 - RAMON -->
          PERFORM get_set_valores
            USING 'REDIST' 'G'
         CHANGING wg_valor.

          CHECK wg_valor = space.
          " 22.02.2024 - RAMON --<

          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'COD_FP'
                                        'G'
                               CHANGING wg_valor.

          CONDENSE wg_valor NO-GAPS.

          " 21.02.2024 - ramon -->
          CLEAR: wg_valor_aux.
          PERFORM get_set_valores USING 'BEZEI'
                                        'G'
                               CHANGING wg_valor_aux.

          " 21.02.2024 - ramon -->

          LOOP AT tg_preco_n
            WHERE cod_fp EQ wg_valor.

            " 21.02.2024 - ramon -->
            IF wg_valor_aux NE tg_preco_n-bezei.
              EXIT.
            ENDIF.
            " 21.02.2024 - ramon --<

            MOVE-CORRESPONDING: tg_preco_n TO tl_input_0059.

            MOVE: tg_preco_n-formula        TO tl_input_0059-formula.
            MOVE: tg_preco_n-ocbot          TO tl_input_0059-ocbot.
            MOVE: tg_preco_n-preco          TO tl_input_0059-preco.
            MOVE: tg_preco_n-c_decimais     TO tl_input_0059-c_decimais.
            MOVE: wl_0051-nro_sol_ov        TO tl_input_0059-nro_sol_ov,
                  sy-uname                  TO tl_input_0059-usnam,
                  sy-datum                  TO tl_input_0059-data_atual,
                  sy-uzeit                  TO tl_input_0059-hora_atual.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING tg_preco_n-field
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-formula2 = wl_valor.

            IF tl_input_0059-tipo_calc EQ 'V'.
              tl_input_0059-formula = tl_input_0059-formula2.
            ENDIF.

            TRANSLATE tl_input_0059-formula2 USING '. '.
            TRANSLATE tl_input_0059-formula2 USING ',.'.
            CONDENSE tl_input_0059-formula2  NO-GAPS.


            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'WAERS'
                                          'G'
                                 CHANGING  wl_valor.
            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-waers = wl_valor.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'CBOT'
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.

            tl_input_0059-cbot = wl_valor.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'MONAT'
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.

            tl_input_0059-monat = wl_valor.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'VALDT'
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.

            tl_input_0059-valdt = wl_valor.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'POSNR'
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-posnr = wl_valor.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'POSNR1'
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-posnr1 = wl_valor.


            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'VALDT_HEDGE'
                                          'G'
                                 CHANGING wl_valor.
            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-valdt_hedge = wl_valor.

            CLEAR: wl_valor.
            PERFORM get_set_valores USING 'MONAT'
                                          'G'
                                 CHANGING  wl_valor.
            CONDENSE wl_valor NO-GAPS.
            tl_input_0059-monat = wl_valor.

            " 14.02.2024 - 121095 - RBL -->
            "IF lv_redist = abap_true.

            CLEAR wl_valor.

            PERFORM get_set_valores
              USING 'REDIST' 'G'
           CHANGING wl_valor.

            IF wl_valor = abap_true.

              CONDENSE wl_valor NO-GAPS.
              tl_input_0059-redist = wl_valor.

              PERFORM get_set_valores
                USING 'BEZEI' 'G'
             CHANGING wl_valor.

              tl_input_0059-bezei = wl_valor.

            ENDIF.

            " 21.02.2024 - ramon -->
            IF wg_valor_aux NE tg_preco_n-bezei.
              EXIT.
            ENDIF.
            " 21.02.2024 - ramon --<


            "ENDIF.

            " 14.02.2024 - 121095 - RBL --<


            APPEND tl_input_0059.
            CLEAR:tl_input_0059.
          ENDLOOP.
        ENDLOOP.

        DELETE FROM zsdt0059 WHERE nro_sol_ov EQ wl_input_0051-nro_sol_ov.

        " 14.02.2024 - 121095 - RBL -->
        "IF lv_redist = abap_true.
        PERFORM f_preco_redist TABLES tl_input_0059[].
        PERFORM f_zsdt0059_redist TABLES tl_input_0059[].
        "ENDIF.

        MODIFY zsdt0059 FROM TABLE tl_input_0059.

      ENDIF.
    ENDIF.

    IF tl_input_0053[] IS NOT INITIAL.
      INSERT zsdt0053 FROM TABLE tl_input_0053_aux.

      IF wg_redistribuir IS INITIAL.
* Historico
        SELECT *
          FROM zsdt0061
          INTO TABLE tl_0061
           FOR ALL ENTRIES IN tl_input_0053
           WHERE nro_sol_ov EQ tl_input_0053-nro_sol_ov
             AND usnam      EQ sy-uname.

        SORT: tl_0061 BY item DESCENDING.
        READ TABLE tl_0061 INDEX 1.

        ADD 1 TO tl_0061-item.

        LOOP AT tg_historico.
          READ TABLE tl_input_0053
            WITH KEY posnr = tg_historico-posnr
                     matnr = tg_historico-matnr.
          IF sy-subrc IS INITIAL.
            MOVE-CORRESPONDING: tg_historico TO tl_input_0061.
            MOVE: tg_historico-zmeng_old TO tl_input_0061-zmeng_old,
                  tl_input_0053-zmeng    TO tl_input_0061-zmeng_new,
                  tl_0061-item           TO tl_input_0061-item,
                  wg_header-nro_sol_ov   TO tl_input_0061-nro_sol_ov,
                  sy-uname               TO tl_input_0061-usnam,
                  sy-datum               TO tl_input_0061-data_atual,
                  sy-uzeit               TO tl_input_0061-hora_atual.

            APPEND tl_input_0061.
            CLEAR: tl_input_0061, tl_input_0053.
          ENDIF.
        ENDLOOP.
        MODIFY zsdt0061 FROM TABLE tl_input_0061.

      ENDIF.


* Logistica
      LOOP AT tg_logistica.
        MOVE-CORRESPONDING: tg_logistica TO tl_input_0055.
        MOVE: wg_header-nro_sol_ov  TO tl_input_0055-nro_sol_ov,
              sy-uname              TO tl_input_0055-usnam,
              sy-datum              TO tl_input_0055-data_atual,
              sy-uzeit              TO tl_input_0055-hora_atual.

        APPEND tl_input_0055.
        CLEAR:tl_input_0055.
      ENDLOOP.
      DELETE FROM zsdt0055 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
      MODIFY zsdt0055 FROM TABLE tl_input_0055.
    ENDIF.
*    ENDIF.

*****************   FIM ELSE    *****************

  ENDIF.


  wg_zmeng  = tg_itens-zmeng.
  wg_dmbtr  = tg_itens-dmbtr.
  wg_charg  = tg_itens-charg.
  wg_matnr  = tg_itens-matnr.

  PERFORM save_log.

  SELECT SINGLE * INTO wa_zparametros
    FROM zparametros
   WHERE nome_parametro = 'EMPRESAS_EVIA_SV_SIGAM'
     AND valor = wg_header-vkorg.

  IF sy-subrc IS INITIAL.
    CLEAR wa_vbpa .
    REFRESH tg_vbpa.
    wa_vbpa-vbeln = wg_header-nro_sol_ov.
    wa_vbpa-parvw = 'LR'.
    IF tg_itens-kunnr IS INITIAL.
      wa_vbpa-kunnr = wg_header-kunnr.
    ELSE.
      wa_vbpa-kunnr = tg_itens-kunnr.
    ENDIF.

    APPEND wa_vbpa TO tg_vbpa.

*--> 24.08.2023 18:33:19 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_SD_OUTBOUND_ORD_VENDA' IN BACKGROUND TASK DESTINATION 'XI_ORDEM_VENDA'
*      EXPORTING
*        nu_ordem_venda = wg_header-nro_sol_ov
*        tp_ordem_venda = 'SOV' "WG_HEADER-TP_VENDA
*        dt_ordem_venda = wg_header-data_atual            "nu_item =
*        tp_frete       = wg_header-inco1
*        id_cliente     = wg_header-kunnr
*        qt_ordem_venda = wg_zmeng
*        cd_material    = wg_matnr
*        vr_unitario    = wg_dmbtr
*        cd_safra       = wg_charg
*        cd_empresa     = wg_header-vkorg
*        cd_centro      = wg_header-vkbur
*        cd_moeda       = wg_header-waerk
*        st_atualizacao = wg_st_change "vl_atua
*        status         = wg_header-vkaus "im_ekko-statu
*        dt_atualizacao = sy-datum
*        hr_atualizacao = sy-uzeit
*        rg_atualizado  = '0'
*        id_interface   = '14'
*      TABLES
*        it_vbpa        = tg_vbpa.
*    COMMIT WORK.

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_SD_OUTBOUND_ORD_VENDA'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        EXPORTING
          nu_ordem_venda = wg_header-nro_sol_ov
          tp_ordem_venda = 'SOV' "WG_HEADER-TP_VENDA
          dt_ordem_venda = wg_header-data_atual            "nu_item =
          tp_frete       = wg_header-inco1
          id_cliente     = wg_header-kunnr
          qt_ordem_venda = wg_zmeng
          cd_material    = wg_matnr
          vr_unitario    = wg_dmbtr
          cd_safra       = wg_charg
          cd_empresa     = wg_header-vkorg
          cd_centro      = wg_header-vkbur
          cd_moeda       = wg_header-waerk
          st_atualizacao = wg_st_change "vl_atua
          status         = wg_header-vkaus "im_ekko-statu
          dt_atualizacao = sy-datum
          hr_atualizacao = sy-uzeit
          rg_atualizado  = '0'
          id_interface   = '14'
        TABLES
          it_vbpa        = tg_vbpa.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        EXPORTING
          nu_ordem_venda = wg_header-nro_sol_ov
          tp_ordem_venda = 'SOV' "WG_HEADER-TP_VENDA
          dt_ordem_venda = wg_header-data_atual            "nu_item =
          tp_frete       = wg_header-inco1
          id_cliente     = wg_header-kunnr
          qt_ordem_venda = wg_zmeng
          cd_material    = wg_matnr
          vr_unitario    = wg_dmbtr
          cd_safra       = wg_charg
          cd_empresa     = wg_header-vkorg
          cd_centro      = wg_header-vkbur
          cd_moeda       = wg_header-waerk
          st_atualizacao = wg_st_change "vl_atua
          status         = wg_header-vkaus "im_ekko-statu
          dt_atualizacao = sy-datum
          hr_atualizacao = sy-uzeit
          rg_atualizado  = '0'
          id_interface   = '14'
        TABLES
          it_vbpa        = tg_vbpa.
    ENDIF.

    COMMIT WORK.
*<-- 24.08.2023 18:33:19 - Migração S4 – ML – Fim
  ENDIF.


*  IF NOT TG_INSTRUCAO[] IS INITIAL..
*
*    OBJ_FRETE->ENVIO_AUTOMATICO( P_NRO_SOL_OV = WG_HEADER-NRO_SOL_OV
*                                 I_ZSDT0045 = TL_INPUT_0045[]
*                                 I_TCODE    = SY-TCODE
*                                ).
*  ENDIF.

* Gera a OV automatico quando salva a Ordem, somente para o tipo de venda 12.
  CASE wg_header-tp_venda.
    WHEN '12'.
      PERFORM libera_ov.
  ENDCASE.

*  Chamar o WebService para fazer o processo da TAXA CURVA para o FRAME.
*     Quando PERFORM ATUALIZA_OV Retorna vazio a tabela da 53,
*     significa que ocorreu algum erro na atualização da Ordem e não dispara o HEDGE.

*  Redistribuição não Dispara o Hedge
  IF ( ok_code EQ 'SAVE' ) AND NOT ( tg_preco_n[] IS INITIAL ) AND ( wg_redistribuir IS INITIAL ).
*IF NOT TL_INPUT_0053[] IS INITIAL.
    CASE wg_header-waerk.
      WHEN: 'BRL'.
        CLEAR: wl_setleaf.
        SELECT SINGLE * FROM setleaf INTO wl_setleaf WHERE setname EQ 'MAGGI_ZSDT0062_HEDGE'
                                                       AND valfrom EQ wl_0051-tp_venda.

        IF ( sy-subrc EQ 0 ).

          IF NOT tl_input_0053[] IS INITIAL.

            IF (  wl_0051-param_espec EQ 'M' OR wl_0051-param_espec EQ 'Z' ).

              "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
              FREE: obj_zcl_webservice_taxa_curva.
              CREATE OBJECT obj_zcl_webservice_taxa_curva.

              TRY.
****              Rotina para alimentar os disparos dos itens no Botão salvar.

                  PERFORM tratar_disparo_frete.

                  IF NOT tl_itens_frete IS INITIAL.
                    LOOP AT tl_itens_frete INTO wl_itens_frete.
                      obj_zcl_webservice_taxa_curva->executar( i_numero  = wg_header-nro_sol_ov
                                                               i_tipo    = 'FRE'
                                                               i_fixacao = wl_itens_frete-fixacao
                                                               i_status  = wl_itens_frete-status_itm
                                                               i_tcode   = sy-tcode
                                                               ).
                    ENDLOOP.
                  ENDIF.
                  FREE tl_itens_frete.

                  IF ( modred_hedge IS INITIAL ).
                    obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                             i_tipo   = 'FRA'
                                                             i_tcode  = sy-tcode
                                                             ).
                  ELSE.

                    obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                             i_tipo   = 'FRA'
                                                             i_ucomm  = 'MODRED'
                                                             i_tcode  = sy-tcode
                                                             ).

                  ENDIF.


                CATCH zcx_webservice INTO cx_exception.
                  var_msg = cx_exception->get_text( ).
                  MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
              ENDTRY.


              CLEAR: var_edit, var_modred.
            ENDIF.

            IF ( var_edit EQ 'X' ) AND ( ok_code = 'SAVE') .

              "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
              FREE: obj_zcl_webservice_taxa_curva.
              CREATE OBJECT obj_zcl_webservice_taxa_curva.
              TRY.
                  obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                           i_tipo   = 'EDI'
                                                           i_tcode  = sy-tcode
                                                           ).
                CATCH zcx_webservice INTO cx_exception.
                  var_msg = cx_exception->get_text( ).
                  MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
              ENDTRY.

              CLEAR: var_edit, var_modred.
            ENDIF.


            IF NOT ( var_modred IS INITIAL  ) AND ( ok_code = 'SAVE').
*                   Empresa 0032 disparar somente diferente de ZCIC "Intercompany", e com o Grupo de Mercadoria 700170 700110 "Soja e Milho"
              IF wg_header-vkorg EQ '0032'.
                v_valido = abap_true.

                SELECT COUNT(*)
                  FROM kna1
                  WHERE lifnr EQ @wg_header-kunnr
                  AND ktokd EQ 'ZCIC'.

                IF sy-subrc IS NOT INITIAL.

                  SELECT COUNT(*)
                    FROM mara
                    WHERE matnr EQ wg_header-matnr
                    AND matkl IN ('700110', '700170').

                  IF sy-subrc IS INITIAL.
                    CLEAR v_valido.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF v_valido IS INITIAL.
                "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
                FREE: obj_zcl_webservice_taxa_curva.
                CREATE OBJECT obj_zcl_webservice_taxa_curva.
                TRY.
                    obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                             i_tipo   = 'EDI'
                                                             i_ucomm  = 'LIBERAR'
                                                             i_tcode  = sy-tcode
                                                             ).
                  CATCH zcx_webservice INTO cx_exception.
                    var_msg = cx_exception->get_text( ).
                    MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
                ENDTRY.

                CLEAR: var_edit, var_modred.

              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m26.
          ENDIF.

          MESSAGE s836(sd) WITH TEXT-m14
                       wl_input_0051-nro_sol_ov
                       TEXT-m19.
        ENDIF.
*** BUG 63143 - Inicio - CSB
      WHEN: 'USD'.
        IF ( var_edit EQ 'X' ) AND ( ok_code EQ 'SAVE') AND ( wl_0051-param_espec NE 'M' ) .
          "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
          FREE: obj_zcl_webservice_taxa_curva.
          CREATE OBJECT obj_zcl_webservice_taxa_curva.
          TRY.
              obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                       i_tipo   = 'EDI'
                                                       i_tcode  = sy-tcode
                                                       ).
            CATCH zcx_webservice INTO cx_exception.
              var_msg = cx_exception->get_text( ).
              MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
          ENDTRY.

          CLEAR: var_edit, var_modred.
        ENDIF.
*** BUG 63143 - Fim - CSB
    ENDCASE.
  ELSE.

    MESSAGE s836(sd) WITH TEXT-m14
                    wl_input_0051-nro_sol_ov
                    TEXT-m19.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_EZSDT0051'
    EXPORTING
      nro_sol_ov = wg_header-nro_sol_ov.

  PERFORM limpa_variavel USING c_atual.
  wg_acao = c_atual.
  LEAVE TO SCREEN 050.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  CLEAR:     wg_desc_kunnr,
             wg_desc_correto,
             wg_desc_auart,
             wg_desc_vkorg,
             wg_desc_vtweg,
             wg_desc_spart,
             wg_desc_vkgrp,
             wg_desc_vkbur,
             wg_desc_zlsch,
             wg_desc_zterm,
             wg_desc_hbkid,
             wg_desc_tp_venda,
             wg_desc_matnr.

  DATA: l_matnr  TYPE matnr,
        lr_matnr TYPE RANGE OF matnr.

  IF strlen( wg_header-matnr ) EQ 40.
    l_matnr = wg_header-matnr+22(18).
  ELSE.
    l_matnr = wg_header-matnr.
  ENDIF.

  APPEND INITIAL LINE TO lr_matnr ASSIGNING FIELD-SYMBOL(<mat>).
  <mat>-sign = 'I'.
  <mat>-option = 'EQ'.
  <mat>-low = l_matnr.

  APPEND INITIAL LINE TO lr_matnr ASSIGNING <mat>.
  <mat>-sign = 'I'.
  <mat>-option = 'EQ'.
  <mat>-low = wg_header-matnr.

  DATA(ls_makt) = zcl_material=>get_instance( )->get_material_text_by_matnr( l_matnr ). "# performance

*  SELECT SINGLE matnr, maktx
*     FROM makt
*     INTO @DATA(l_maktx)
*      WHERE matnr IN @lr_matnr
*          AND spras EQ @sy-langu.

  IF ls_makt IS NOT INITIAL.
    IF ls_makt-matnr = l_matnr.
      wg_header-matnr = l_matnr.
    ENDIF.
  ENDIF.

  wg_desc_matnr = ls_makt-maktx.

  SELECT SINGLE name1
    FROM kna1
    INTO wg_desc_kunnr
     WHERE kunnr EQ wg_header-kunnr.

  SELECT SINGLE name1
    FROM lfa1
    INTO wg_desc_correto
     WHERE lifnr EQ wg_header-correto.

  SELECT SINGLE bezei
    FROM tvakt
    INTO wg_desc_auart
     WHERE spras EQ sy-langu
       AND auart EQ wg_header-auart.

  SELECT SINGLE vtext
    FROM tvkot
    INTO wg_desc_vkorg
     WHERE spras EQ sy-langu
       AND vkorg EQ wg_header-vkorg.

  SELECT SINGLE vtext
    FROM tvtwt
    INTO wg_desc_vtweg
     WHERE spras EQ sy-langu
       AND vtweg EQ wg_header-vtweg.

  SELECT SINGLE vtext
    FROM tspat
    INTO wg_desc_spart
     WHERE spras EQ sy-langu
       AND spart EQ wg_header-spart.

  SELECT SINGLE vtext
    FROM tspat
    INTO wg_desc_spart
     WHERE spras EQ sy-langu
       AND spart EQ wg_header-spart.

  SELECT SINGLE bezei
    FROM tvgrt
    INTO wg_desc_vkgrp
     WHERE spras EQ sy-langu
       AND vkgrp EQ wg_header-vkgrp.

  SELECT SINGLE bezei
    FROM tvkbt
    INTO wg_desc_vkbur
     WHERE spras EQ sy-langu
       AND vkbur EQ wg_header-vkbur.


  CASE sy-langu.
    WHEN: 'S'.
      SELECT SINGLE text1
       FROM t042z
       INTO wg_desc_zlsch
        WHERE land1 EQ 'AR'
         AND zlsch EQ wg_cond_pgt-zlsch.
    WHEN: 'P'.
      SELECT SINGLE text1
        FROM t042z
        INTO wg_desc_zlsch
         WHERE land1 EQ 'BR'
           AND zlsch EQ wg_cond_pgt-zlsch.
    WHEN OTHERS.
      SELECT SINGLE text1
        FROM t042z
        INTO wg_desc_zlsch
         WHERE land1 EQ sy-langu
           AND zlsch EQ wg_cond_pgt-zlsch.
  ENDCASE.

  SELECT SINGLE vtext
   FROM tvzbt
   INTO wg_desc_zterm
    WHERE spras EQ sy-langu
      AND zterm EQ wg_cond_pgt-zterm.


  SELECT SINGLE text1
    FROM t012t
    INTO wg_desc_hbkid
     WHERE spras EQ sy-langu
       AND bukrs EQ wg_header-vkorg
       AND hbkid EQ wg_cond_pgt-hbkid.

  SELECT SINGLE bezei
      FROM zsdt0057
      INTO wg_desc_tp_venda
       WHERE tp_venda EQ wg_header-tp_venda.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM valida_layout  TABLES   tl_fieldcatalog STRUCTURE lvc_s_fcat
                     USING   uname.

  DATA: tl_parametros        TYPE ustyp_t_parameters,
        wl_parametros        TYPE ustyp_parameters,
        wl_fieldcatalog      TYPE lvc_s_fcat,
        wl_variante01        TYPE zvariante01,
        tl_variante02_alv    TYPE TABLE OF zvariante02 WITH HEADER LINE,
        tl_variante02_screen TYPE TABLE OF zvariante02 WITH HEADER LINE,
        wl_tabix             TYPE sy-tabix,
        wl_atributo(30).

  REFRESH: tl_parametros, tl_variante02_alv, tl_variante02_screen.
  FIELD-SYMBOLS: <fs_atributos> TYPE any.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = uname
*     WITH_TEXT           =
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  READ TABLE tl_parametros INTO wl_parametros
    WITH KEY parid = 'ZVARIANTE'.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE grpva EQ wl_parametros-parva
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      CONDENSE wl_variante01-grpva NO-GAPS.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_screen
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip NE 'ALV'
           AND dynnr   EQ sy-dynnr.

    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
    AND ( sy-tcode NE 'SE38'
       AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
          AND tl_variante02_screen-acao EQ wg_acao )
            OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      wl_tabix = sy-tabix.
      READ TABLE tl_variante02_alv
        WITH KEY field = wl_fieldcatalog-fieldname.
      IF sy-subrc IS NOT  INITIAL.
        IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
          DELETE tl_fieldcatalog INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE default_var EQ c_x
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
         FROM zvariante02
         INTO TABLE tl_variante02_screen
          WHERE grpva   EQ wl_variante01-grpva
            AND tcode   EQ sy-tcode
            AND atr_tip NE 'ALV'
            AND dynnr   EQ sy-dynnr.
    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
        AND ( sy-tcode NE 'SE38'
           AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      wl_tabix = sy-tabix.
      READ TABLE tl_variante02_alv
        WITH KEY field = wl_fieldcatalog-fieldname.
      IF sy-subrc IS NOT  INITIAL.
        IF ( tl_variante02_alv-acao IS NOT INITIAL
            AND tl_variante02_alv-acao EQ wg_acao )
              OR tl_variante02_alv-acao IS INITIAL.
          DELETE tl_fieldcatalog INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TEXTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1071   text
*      -->P_WG_ACAO  text
*      <--P_WG_HEADER_OBSERVACAO  text
*----------------------------------------------------------------------*
FORM busca_textos_obs USING VALUE(p_texto)
                            p_acao
                   CHANGING p_campo_texto TYPE zsdt0051-observacao
                            p_icon.

  DATA: tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab,
        wl_cont  TYPE sy-tabix,
        wl_mod   TYPE sy-tabix,
        wl_pos   TYPE sy-tabix,
        wl_line  TYPE sy-tabix.

  CLEAR: wl_texto, tl_texto[].


  wl_cont = strlen( p_campo_texto ).

  WHILE wl_pos < wl_cont.
    wl_line = wl_cont - wl_pos.

    IF wl_line >= 72.
      wl_line = 72.
    ENDIF.

    wl_texto = p_campo_texto+wl_pos(wl_line).
    ADD 72 TO wl_pos.
    IF wl_texto IS NOT INITIAL.
      APPEND wl_texto TO tl_texto.
    ENDIF.
    CLEAR: wl_texto.
  ENDWHILE.

  IF p_acao EQ c_add
  OR p_acao EQ c_modif.
    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title = p_texto
*       IM_DISPLAY_MODE  =
      CHANGING
        ch_text  = tl_texto.

    LOOP AT tl_texto INTO wl_texto.
      IF sy-tabix EQ 1.
        p_campo_texto = wl_texto.
      ELSEIF sy-tabix GT 1.
        CONCATENATE p_campo_texto wl_texto INTO p_campo_texto SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    IF tl_texto[] IS INITIAL.
      CLEAR: p_campo_texto.
    ENDIF.

    IF p_campo_texto IS NOT INITIAL.
      p_icon = '@6X@'.
    ELSE.
      p_icon = '@6Y@'.
    ENDIF.

  ELSEIF p_acao EQ c_modif_qtd.
    IF wg_redistribuir IS NOT INITIAL.
      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = p_texto
          im_display_mode = 'X'
        CHANGING
          ch_text         = tl_texto.
    ELSE.
      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = p_texto
*         IM_DISPLAY_MODE  =
        CHANGING
          ch_text  = tl_texto.

      LOOP AT tl_texto INTO wl_texto.
        IF sy-tabix EQ 1.
          p_campo_texto = wl_texto.
        ELSEIF sy-tabix GT 1.
          CONCATENATE p_campo_texto wl_texto INTO p_campo_texto SEPARATED BY space.
        ENDIF.
      ENDLOOP.
      IF tl_texto[] IS INITIAL.
        CLEAR: p_campo_texto.
      ENDIF.

      IF p_campo_texto IS NOT INITIAL.
        p_icon = '@6X@'.
      ELSE.
        p_icon = '@6Y@'.
      ENDIF.

    ENDIF.
  ELSE.
    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = p_texto
        im_display_mode = 'X'
      CHANGING
        ch_text         = tl_texto.

  ENDIF.


ENDFORM.                    " BUSCA_TEXTOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_TEXTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1071   text
*      -->P_WG_ACAO  text
*      <--P_WG_HEADER_OBSERVACAO  text
*----------------------------------------------------------------------*
FORM busca_textos  USING    VALUE(p_texto)
                            p_acao
                   CHANGING p_campo_texto TYPE zsdt0051-observacao
                            p_icon.

  DATA: tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab,
        wl_cont  TYPE sy-tabix,
        wl_line  TYPE sy-tabix,
        wl_pos   TYPE sy-tabix.


  wl_cont = strlen( p_campo_texto ).

  WHILE wl_pos < wl_cont.
    wl_line = wl_cont - wl_pos.

    IF wl_line >= 72.
      wl_line = 72.
    ENDIF.

    wl_texto = p_campo_texto+wl_pos(wl_line).
    ADD 72 TO wl_pos.
    IF wl_texto IS NOT INITIAL.
      APPEND wl_texto TO tl_texto.
    ENDIF.
    CLEAR: wl_texto.
  ENDWHILE.

  IF p_acao EQ c_add
  OR p_acao EQ c_modif.
    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title = p_texto
*       IM_DISPLAY_MODE  =
      CHANGING
        ch_text  = tl_texto.

    LOOP AT tl_texto INTO wl_texto.
      IF sy-tabix EQ 1.
        p_campo_texto = wl_texto.
      ELSEIF sy-tabix GT 1.
        CONCATENATE p_campo_texto wl_texto INTO p_campo_texto SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    IF tl_texto[] IS INITIAL.
      CLEAR: p_campo_texto.
    ENDIF.

    IF p_campo_texto IS NOT INITIAL.
      p_icon = '@6X@'.
    ELSE.
      p_icon = '@6Y@'.
    ENDIF.
  ELSE.
    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = p_texto
        im_display_mode = 'X'
      CHANGING
        ch_text         = tl_texto.

  ENDIF.


ENDFORM.                    " BUSCA_TEXTOS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_VKGRP  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_vkgrp INPUT.
  DATA: tl_tvbvk      TYPE TABLE OF tvbvk WITH HEADER LINE,
        tl_return_tab TYPE TABLE OF ddshretval,
        wl_return_tab TYPE ddshretval,
        tl_dselc      TYPE TABLE OF dselc.
*        tl_tvgrt type table of tvgrt with header line.

  DATA: BEGIN OF tl_tvgrt OCCURS 0,
          vkgrp TYPE tvgrt-vkgrp,
          bezei TYPE tvgrt-bezei,
        END OF tl_tvgrt.

  CLEAR: tl_tvbvk, tl_tvgrt, tl_return_tab, tl_dselc.
  REFRESH: tl_tvbvk, tl_tvgrt, tl_return_tab, tl_dselc.

  SELECT *
    FROM tvbvk
    INTO TABLE tl_tvbvk
     WHERE vkbur EQ wg_header-vkbur.

  IF sy-subrc IS INITIAL.
    SELECT vkgrp bezei
      FROM tvgrt
      INTO TABLE tl_tvgrt
      FOR ALL ENTRIES IN tl_tvbvk
       WHERE spras EQ sy-langu
         AND vkgrp EQ tl_tvbvk-vkgrp.

  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'VKGRP'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-VKGRP'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tvgrt
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_VKGRP  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_VKBUR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_vkbur INPUT.
  CLEAR: wg_header-vkgrp, wg_desc_vkgrp.
ENDMODULE.                 " REFRESH_VKBUR  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_TELA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_tela INPUT.
  enter.

ENDMODULE.                 " REFRESH_TELA  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_POSICAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_posicao INPUT.

  PERFORM busca_posicao.

ENDMODULE.                 " REFRESH_POSICAO  INPUT
*&---------------------------------------------------------------------*
*&      Module  GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_cursor INPUT.
  DATA: l_cursor TYPE t_cursor.
  IF sy-ucomm EQ c_dp_click.
    GET CURSOR FIELD l_cursor-fname LINE l_cursor-pos AREA l_cursor-tc
               VALUE l_cursor-value.

    IF l_cursor-fname EQ 'WG_POSICAO-VLR_F_VENCIDA'.
      sy-ucomm = c_pf_vencidas.
    ELSEIF l_cursor-fname EQ 'WG_POSICAO-VLR_F_AVENCER'.
      sy-ucomm = c_pf_avencer.
    ELSEIF l_cursor-fname EQ 'WG_POSICAO-VLR_F_ADIANT'.
      sy-ucomm = c_pf_adiant.
    ELSEIF l_cursor-fname EQ 'WG_POSICAO-VLR_TOTAL_MOV'.
      sy-ucomm = c_pf_totmov.
    ENDIF.

    CALL FUNCTION 'ZSDMF002_POSICAO_FINANCEIRA'
      EXPORTING        "BUKRS = WG_HEADER-VKORG
        kunnr = wg_header-kunnr
        waerk = wg_header-waerk
        ucomm = sy-ucomm.
  ENDIF.
ENDMODULE.                 " GET_CURSOR  INPUT
*&---------------------------------------------------------------------*
*&      Module  REFRESH_PRECO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_preco INPUT.
  DATA: rc TYPE sy-subrc.
  PERFORM verifica_enqueue CHANGING rc.
  PERFORM refresh_preco.

ENDMODULE.                 " REFRESH_PRECO  INPUT
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STATUS   text
*----------------------------------------------------------------------*
FORM modifica_status  USING  p_status
                    CHANGING rc.

  DATA: wl_0051  TYPE zsdt0051,
        wl_0053  TYPE zsdt0053,
        tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab.

  DATA: wl_setleaf TYPE setleaf.

**********************************************************************
* Variaveis
**********************************************************************
  DATA: var_msg   TYPE string.

**********************************************************************
* Classe Exception
**********************************************************************
  DATA: cx_exception TYPE REF TO zcx_webservice.

**********************************************************************
* Classes que serão utilizadas nesse processo.
**********************************************************************
  DATA: obj_zcl_webservice_taxa_curva TYPE REF TO zcl_webservice_tx_curva.


  CLEAR: wl_0051, wl_0053, wl_texto.


  SELECT SINGLE *
    FROM zsdt0051
    INTO wl_0051
     WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

  IF sy-subrc IS INITIAL.

    IF p_status EQ c_p.

      TRY.
          DATA(tg_parc)  = CAST zcl_clientes( zcl_clientes=>zif_parceiros~get_instance(
              )->set_parceiro( i_parceiro = wl_0051-kunnr
              )->ck_ativo( i_ck_sd = abap_true
              )->ck_ativo_empresa( i_empresa = wl_0051-vkorg
          ) )->at_kna1.

        CATCH zcx_parceiros INTO DATA(ex_parceiros_k).
          DATA(texto) = ex_parceiros_k->get_text( ).
          MESSAGE texto TYPE ex_parceiros_k->msgty.
      ENDTRY.

      TRY.
          zcl_fornecedores=>zif_parceiros~get_instance(
          )->set_parceiro_cnpj_cpf_ie(  i_cnpj  = CONV #( tg_parc-stcd1 )
                                        i_cpf   = CONV #( tg_parc-stcd2 )
                                        i_insc_estatual = CONV #( tg_parc-stcd3 )
                                        i_agnorar_bloqueio = abap_true

          ).
        CATCH zcx_parceiros INTO ex_parceiros_k.
          DATA(vl_exit) = abap_true.
      ENDTRY.
      IF vl_exit IS INITIAL.
        TRY.
            zcl_fornecedores=>zif_parceiros~get_instance(
              )->ck_ativo(
            ).
          CATCH zcx_parceiros INTO ex_parceiros_k.
            texto = ex_parceiros_k->get_text( ).
            MESSAGE texto TYPE ex_parceiros_k->msgty.
        ENDTRY.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0053
      INTO wl_0053
       WHERE nro_sol_ov EQ wg_header-nro_sol_ov
         AND vbeln NE space.

    IF sy-subrc IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_EZSDT0051'
        EXPORTING
          nro_sol_ov     = wg_header-nro_sol_ov
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      MOVE: p_status TO wl_0051-status.

      IF wl_0051-status IS INITIAL.
        wg_status = icon_initial.

      ELSEIF wl_0051-status EQ c_l.
        wg_status = icon_release.
        wg_desc_status = TEXT-s02.

        CLEAR: tg_motivo.
        PERFORM get_next_number
                    USING
                       'ZHISTORIC'
                       '01'
                       c_x
                    CHANGING
                       tg_motivo-id_historico.

        REFRESH: tl_texto.
        CLEAR: convert_texto.
        MOVE TEXT-t01 TO convert_texto.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title = convert_texto "'Texto Motivo Liberação' "text-T01
          CHANGING
            ch_text  = tl_texto.

        LOOP AT tl_texto INTO wl_texto.
          IF sy-tabix EQ 1.
            tg_motivo-motivo = wl_texto.
          ELSE.
            CONCATENATE tg_motivo-motivo wl_texto INTO tg_motivo-motivo.
          ENDIF.
        ENDLOOP.

        tg_motivo-status     = wl_0051-status.
        tg_motivo-nro_sol_ov = wl_0051-nro_sol_ov.
        tg_motivo-usnam      = sy-uname.
        tg_motivo-data_atual = sy-datum.
        tg_motivo-hora_atual = sy-uzeit.
        APPEND tg_motivo.

        MODIFY zsdt0069 FROM TABLE tg_motivo.

        IF wg_header-param_espec NE c_m OR wg_header-param_espec NE c_z.
          LOOP AT tg_itens.
            tg_itens-status_itm = c_f.
            MODIFY tg_itens.
          ENDLOOP.
        ENDIF.

      ELSEIF wl_0051-status EQ c_d.
        wg_status = icon_delete.
        wg_desc_status = TEXT-s03.
      ELSEIF wl_0051-status EQ c_r.
        wg_status = icon_defect.
        wg_desc_status = TEXT-s04.
        CLEAR: tg_motivo.
        PERFORM get_next_number
                    USING
                       'ZHISTORIC'
                       '01'
                       c_x
                    CHANGING
                       tg_motivo-id_historico.

        REFRESH: tl_texto.
        CLEAR: convert_texto.
        MOVE TEXT-t02 TO convert_texto.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title = convert_texto "'Texto Movito Reprovação'"text-T02
          CHANGING
            ch_text  = tl_texto.

        LOOP AT tl_texto INTO wl_texto.
          IF sy-tabix EQ 1.
            tg_motivo-motivo = wl_texto.
          ELSE.
            CONCATENATE tg_motivo-motivo wl_texto INTO tg_motivo-motivo.
          ENDIF.
        ENDLOOP.

        tg_motivo-status     = wl_0051-status.
        tg_motivo-nro_sol_ov = wl_0051-nro_sol_ov.
        tg_motivo-usnam      = sy-uname.
        tg_motivo-data_atual = sy-datum.
        tg_motivo-hora_atual = sy-uzeit.
        MODIFY zsdt0069 FROM tg_motivo.

        IF wg_header-param_espec NE c_m OR wg_header-param_espec NE c_z .
          LOOP AT tg_itens.
            tg_itens-status_itm = space.

            MODIFY tg_itens.
          ENDLOOP.
        ENDIF.
      ELSEIF wl_0051-status EQ c_p.
        wg_status = icon_led_yellow.
        wg_desc_status = TEXT-s05.
      ENDIF.

      MODIFY zsdt0051 FROM wl_0051.

      rc = sy-subrc.
      IF sy-subrc IS INITIAL
      AND p_status EQ c_l.
        IF wg_header-param_espec EQ c_p.
          PERFORM liberacao_performance.
        ENDIF.
*        PERFORM ENVIA_EMAIL_LIBERACAO.
        PERFORM gera_html_liberacao.
      ENDIF.

      PERFORM refresh_log_0162.
      PERFORM sol_aprov_email.

      CALL FUNCTION 'DEQUEUE_EZSDT0051'
        EXPORTING
          nro_sol_ov = wg_header-nro_sol_ov.
    ENDIF.

    IF ( wl_0051-param_espec NE 'M' ) AND ( p_status EQ 'L' ).
      CASE wg_header-waerk.
        WHEN: 'BRL'.

          SELECT SINGLE * FROM setleaf INTO wl_setleaf WHERE setname EQ 'MAGGI_ZSDT0062_HEDGE'
                                                         AND valfrom EQ wl_0051-tp_venda.
          IF ( sy-subrc EQ 0 ).
            "WebService para Capturar o Valor da Taxa e gravar no campo TAXA_CURVA
            FREE: obj_zcl_webservice_taxa_curva.
            CREATE OBJECT obj_zcl_webservice_taxa_curva.
            TRY.
                obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                         i_tipo   = 'EDI'
                                                         i_tcode  = sy-tcode
                                                         ).

                obj_zcl_webservice_taxa_curva->executar( i_numero = wg_header-nro_sol_ov
                                                         i_tipo   = 'LIB'
                                                         i_tcode  = sy-tcode
                                                         ).

              CATCH zcx_webservice INTO cx_exception.
                var_msg = cx_exception->get_text( ).
                MESSAGE e007(zwebservice) DISPLAY LIKE 'W' WITH var_msg.
            ENDTRY.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDIF.

ENDFORM.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  REFRESH_PRECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_preco .
  DATA: wl_0057          TYPE zsdt0057,
        tl_0056          TYPE TABLE OF zsdt0056 WITH HEADER LINE,
        tl_0058          TYPE TABLE OF zsdt0058 WITH HEADER LINE,
        tl_0070          TYPE TABLE OF zsdt0070 WITH HEADER LINE,
        tl_preco_in      TYPE TABLE OF zsds006 WITH HEADER LINE,
        tl_preco_out     TYPE TABLE OF zsds006 WITH HEADER LINE,
        wl_valor(30),
        wl_valor_aux(30),
        wl_tabix         TYPE sy-tabix.

  DATA: p_2(13) TYPE p DECIMALS 2,
        p_5(13) TYPE p DECIMALS 5,
        p_4(13) TYPE p DECIMALS 4.

  REFRESH: tl_0056, tl_0058, tl_0070, tl_preco_in, tl_preco_out, tg_preco_n.
  CLEAR: wl_0057, tl_0056, tl_0058, tl_0070, tg_preco.

  SELECT SINGLE *
    FROM zsdt0057
     INTO wl_0057
     WHERE tp_venda EQ wg_header-tp_venda.

  IF sy-subrc IS INITIAL.
    MOVE: wl_0057-auart       TO wg_header-auart.
    MOVE: wl_0057-param_espec TO wg_header-param_espec.
    SELECT *
      FROM zsdt0058
      INTO TABLE tl_0058
       WHERE esq_calc EQ wl_0057-esq_calc.

    " 15.04.2024 - RAMON -->
    SORT tl_0058 BY nivel ASCENDING.
    " 15.04.2024 - RAMON --<

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zsdt0056
        INTO TABLE tl_0056
         FOR ALL ENTRIES IN tl_0058
         WHERE cod_fp EQ tl_0058-cod_fp.

      IF sy-subrc IS INITIAL .
        SELECT *
          FROM zsdt0070
          INTO TABLE tl_0070
           FOR ALL ENTRIES IN tl_0056
           WHERE cod_fp EQ tl_0056-cod_fp.

      ENDIF.
    ENDIF.

    REFRESH: t_fieldcatalog.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = wg_header-tp_venda
      IMPORTING
        e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog
        te_fields       = tg_fields.


    CALL METHOD grid4->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog.
* Cria uma field-symbol como tabela interna
    ASSIGN t_new_table->* TO <fs_table>.
    CREATE DATA t_new_line LIKE LINE OF <fs_table>.
    CREATE DATA t_new_table LIKE <fs_table>.
* Cria uma field-symbol como tabela interna
    ASSIGN t_new_table->* TO <fs_table_frame>.

* Cria uma field-symbol como work area
    ASSIGN t_new_line->* TO <fs_line>.
    ASSIGN t_new_line->* TO <fs_line_aux>.

    IF tree1 IS NOT INITIAL
    AND grid4 IS NOT INITIAL.
      PERFORM redefine_objetos.
    ENDIF.

    LOOP  AT tl_0058.
      READ TABLE tl_0056
        WITH KEY cod_fp = tl_0058-cod_fp.
*      CLEAR: TL_0056-PRECO.
      MOVE-CORRESPONDING: tl_0058 TO tg_preco_n.
      MOVE-CORRESPONDING: tl_0058 TO <fs_line>.
      MOVE-CORRESPONDING: tl_0056 TO tg_preco_n.
      MOVE-CORRESPONDING: tl_0056 TO <fs_line>.

      PERFORM get_set_valores USING   'WAERS'
                                      'S'
                             CHANGING tl_0056-waers.

      PERFORM get_set_valores USING    'OCBOT'
                                       'S'
                              CHANGING tl_0056-ocbot.

      PERFORM get_set_valores USING    'INVISIBLE'
                                       'S'
                              CHANGING tl_0056-invisible.

      MOVE: tl_0056-waers     TO tg_preco_n-waers,
            tl_0056-ocbot     TO tg_preco_n-ocbot,
            tl_0056-invisible TO tg_preco_n-invisible.
*      MOVE-CORRESPONDING: TL_PRECO_IN TO TG_PRECO.
*      IF TL_0056-TP_COD_FP EQ C_C.
      READ TABLE tl_0070
        WITH KEY cod_fp = tl_0056-cod_fp
                 field  = tl_0058-field.

      IF sy-subrc IS INITIAL.
        PERFORM get_set_valores USING 'TIPO_CALC'
                                      'S'
                             CHANGING tl_0070-tipo_calc.

        PERFORM get_set_valores USING 'C_DECIMAIS'
                                      'S'
                             CHANGING tl_0070-c_decimais.

        PERFORM get_set_valores USING 'PRECO_ITEM'
                                      'S'
                             CHANGING tl_0070-preco.

        MOVE: tl_0070-field      TO tg_preco_n-field,
              tl_0070-tipo_calc  TO tg_preco_n-tipo_calc,
              tl_0070-c_decimais TO tg_preco_n-c_decimais,
*              tl_0070-VLR_FIXO   to TG_PRECO-VLR_FIXO,
              tl_0070-preco      TO tg_preco_n-preco.


      ENDIF.
*      ENDIF.
      MOVE: tl_0058-formula TO tg_preco_n-formula.
      PERFORM get_set_valores USING  'FORMULA'
                                             'S'
                                    CHANGING tl_0058-formula.
      IF tg_preco_n-tipo_calc EQ 'C'
      OR tg_preco_n-tipo_calc EQ c_r.
*        MOVE: 'C305' TO TG_PRECO-LINE_COLOR.
        wl_valor = 'C305'.
        PERFORM get_set_valores USING  'LINE_COLOR'
                                       'S'
                              CHANGING wl_valor.
      ELSE.
*        CLEAR TG_PRECO-LINE_COLOR.
        CLEAR wl_valor.
        PERFORM get_set_valores USING  'LINE_COLOR'
                                       'S'
                              CHANGING wl_valor.
      ENDIF.
      IF tg_preco_n-tipo_calc EQ c_f.
        CONDENSE tl_0058-formula NO-GAPS.
        IF tg_preco_n-c_decimais EQ 2.
          p_2 = tl_0058-formula.
          WRITE p_2 TO tg_preco_n-formula.
        ELSEIF tg_preco_n-c_decimais EQ 4.
          p_4 = tl_0058-formula.
          WRITE p_4 TO tg_preco_n-formula.
        ELSEIF tg_preco_n-c_decimais EQ 5.
          p_5 = tl_0058-formula.
          WRITE p_5 TO tg_preco_n-formula.

        ENDIF.
        CONDENSE tg_preco_n-formula NO-GAPS.
        PERFORM get_set_valores USING tl_0058-field
                                       'S'
                              CHANGING tg_preco_n-formula.
      ELSE.
        wg_valor_aux = space.
        PERFORM get_set_valores USING tl_0058-field
                                       'S'
                              CHANGING wg_valor_aux.
*        CLEAR: TG_PRECO-FORMULA2.
*        CLEAR: TG_PRECO_N-FORMULA.
*        PERFORM GET_SET_VALORES USING TL_0058-FIELD
*                                       'S'
*                              CHANGING TG_PRECO_N-FORMULA.
      ENDIF.
*      APPEND TL_PRECO_IN.
      APPEND tg_preco_n.

      LOOP AT <fs_table> ASSIGNING <fs_line_aux>.
        CLEAR: wl_valor, wl_tabix.
        PERFORM get_set_valores USING  'COD_FP'
                                       'G'
                              CHANGING wl_valor.

        CLEAR wl_valor_aux.
        ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
        wl_valor_aux = <fs_campo>.
        IF wl_valor EQ wl_valor_aux.
          wl_tabix = sy-tabix.
        ENDIF.
      ENDLOOP.
      IF wl_tabix IS INITIAL.
        APPEND <fs_line> TO <fs_table>.
      ELSEIF wl_tabix IS NOT INITIAL.
        MODIFY <fs_table> FROM <fs_line> INDEX wl_tabix.
      ENDIF.
      CLEAR: tl_preco_in, tl_0056, tg_preco_n, tl_0070, <fs_line>.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " REFRESH_PRECO
*&---------------------------------------------------------------------*
*&      Module  REFRESH_LOGISTICA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_logistica INPUT.
  PERFORM refresh_logistica.

ENDMODULE.                 " REFRESH_LOGISTICA  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_LOGISTICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_logistica .
  DATA: wl_dias TYPE i,
        wl_qtd  TYPE zsdt0053-zmeng,
        wl_data TYPE sy-datum.

  IF wg_header-inco1 EQ 'CIF' AND var_modred IS INITIAL.
    IF wg_header-dtde_logist NE wg_header-dtate_logist.
*      CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*        EXPORTING
*          I_DATUM_BIS = WG_HEADER-DTATE_LOGIST  " Data Maior
*          I_DATUM_VON = WG_HEADER-DTDE_LOGIST  " Data Menor
*        IMPORTING
*          E_TAGE      = WL_DIAS.

      ADD 1 TO wl_dias.
      IF wl_dias GE 1.
        REFRESH: tg_logistica.

        CLEAR: wl_qtd.
        LOOP AT tg_itens.
          ADD tg_itens-zmeng TO wl_qtd.

        ENDLOOP.

        TRY.
            DIVIDE wl_qtd BY wl_dias.
*      catch cx_zero
        ENDTRY.
        CLEAR: wl_data.
        MOVE: wg_header-dtde_logist TO wl_data.
        DO wl_dias TIMES.
          MOVE: wl_data        TO tg_logistica-data_progr,
                tg_itens-zieme TO tg_logistica-zieme,
                wl_qtd         TO tg_logistica-cadencia_qte.

          APPEND tg_logistica.
          ADD 1 TO wl_data.
        ENDDO.
      ENDIF.
    ELSEIF wg_header-inco1 EQ 'FOB'.
      REFRESH: tg_logistica.
    ENDIF.
  ENDIF.

  CALL METHOD grid4->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " REFRESH_LOGISTICA
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_ov TABLES tl_input_0053 STRUCTURE zsdt0053
                        tl_input_0066 STRUCTURE zsdt0066.

  CALL FUNCTION 'ZSDMF002_ATUALI_OV_SOLICITACAO'
    TABLES
      ti_itens_ov       = tl_input_0053
      ti_form_lote      = tl_input_0066
    EXCEPTIONS
      ov_nao_encontrada = 1
      OTHERS            = 2.

  CALL FUNCTION 'ENQUEUE_EZSDT0051'
    EXPORTING
      nro_sol_ov     = wg_header-nro_sol_ov
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.                    " ATUALIZA_OV
*&---------------------------------------------------------------------*
*&      Form  BUSCA_POSICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_posicao .


  CALL FUNCTION 'ZSDMF002_POSICAO_FINANCEIRA'
    EXPORTING
      "BUKRS                      = WG_HEADER-VKORG
      kunnr                = wg_header-kunnr
      waerk                = wg_header-waerk
    IMPORTING
      vlr_vencido_brl      = wg_posicao-vlr_f_vencida
*     VLR_VENCIDO_USD      =
      vlr_avencer_brl      = wg_posicao-vlr_f_avencer
*     VLR_AVENCER_USD      =
      vlr_adiantado_brl    = wg_posicao-vlr_f_adiant
*     VLR_ADIANTADO_USD    =
      limite_credito       = wg_posicao-vlr_limite
      total_brl            = wg_posicao-vlr_total
*     TOTAL_USD            =
      total_movimento_brl  = wg_posicao-vlr_total_mov
*     TOTAL_MOVIMENTO_USD  =
      saldo_disponivel_brl = wg_posicao-vlr_saldo
*     SALDO_DISPONIVEL_USD =
      utilizado_limite_brl = wg_posicao-utilizado
      sdo_ov_emit          = wg_posicao-sdo_ov_emit.
*   UTILIZADO_LIMITE_USD       =
*  .

  wg_hora = sy-uzeit.
  wg_data = sy-datum.

ENDFORM.                    " BUSCA_POSICAO
*&---------------------------------------------------------------------*
*&      Form  ENVIAR_DOCUMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM enviar_documento .
  CLEAR: p_email.
  CLEAR: rb_imprimir.
  rb_email = c_x.
  CLEAR: c_eletronica.
  CALL SCREEN 200 ENDING AT 51 8 STARTING AT 3 3.

ENDFORM.                    " ENVIAR_DOCUMENTO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  SET PF-STATUS 'Z02'.

  LOOP AT SCREEN.
    IF rb_email IS NOT INITIAL.
      IF screen-name EQ 'P_EMAIL'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.

    ELSE.
      IF screen-name EQ 'P_EMAIL'.
        screen-input = 0.
        MODIFY SCREEN.
        CLEAR: p_email.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: vl_form TYPE tdsfname,
        vl_name TYPE rs38l_fnam.

  CASE sy-ucomm.
    WHEN 'CANCEL'
      OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.

*      PERFORM REFRESH_LOG_0162.
* PBI  - 66602 - Inicio - CBRAND
*vl_form = 'ZSDS0003'. P
      vl_form = 'ZSDS0008'.
* PBI  - 66602 - Fim - CBRAND

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = vl_form
        IMPORTING
          fm_name            = vl_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF rb_email IS NOT INITIAL.
        PERFORM gera_pdf.
      ELSE.
        CALL FUNCTION vl_name
          EXPORTING
            i_nro_sol_ov     = wg_header-nro_sol_ov
            i_digital        = c_eletronica "PBI - 66602 - CBRAND
          EXCEPTIONS
            formatting_error = 1
            internal_error   = 2
            send_error       = 3
            user_canceled    = 4
            OTHERS           = 5.

        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  GERA_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_pdf .
  DATA: ls_control        TYPE ssfctrlop,
        ls_options        TYPE ssfcompop,
        job_output_info   TYPE ssfcrescl,
        ls_xsfparam_line  TYPE ssfxsfp,
        v_bin_filesize    TYPE i,
        it_docs           TYPE STANDARD TABLE OF docs,
        it_lines          TYPE STANDARD TABLE OF tline,
        lv_fname          TYPE rs38l_fnam,
        lv_mail_recipient TYPE swotobjid,
        lv_mail_sender    TYPE swotobjid,
        lv_control        TYPE ssfctrlop,
        lv_name           TYPE so_name,
        lv_output         TYPE ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20).

  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
        zva_txt     TYPE char255,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

*  Impresora
  ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = 'X'.

  CLEAR:job_output_info.
  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      i_nro_sol_ov       = wg_header-nro_sol_ov
      i_digital          = c_eletronica "PBI - 66602 - CBRAND
    IMPORTING
      job_output_info    = job_output_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.

  ELSE.

    i_otf[] = job_output_info-otfdata[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = v_bin_filesize
      TABLES
        otf                   = i_otf
        lines                 = i_tline
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.
    IF sy-subrc EQ 0.
    ENDIF.
    LOOP AT i_tline.
      TRANSLATE i_tline USING '~'.
      CONCATENATE wa_buffer i_tline INTO wa_buffer.
    ENDLOOP.
    TRANSLATE wa_buffer USING '~'.
    DO.
      i_record = wa_buffer.
      APPEND i_record.
      SHIFT wa_buffer LEFT BY 255 PLACES.
      IF wa_buffer IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
* Attachment
    REFRESH: i_reclist,
    i_objtxt,
    i_objbin,
    i_objpack.
    CLEAR wa_objhead.
    i_objbin[] = i_record[].
* Create Message Body Title and Description
    zva_txt = |{ TEXT-w33 } { TEXT-w34 }|. "USER STORY 170715 / AOENNING
    i_objtxt = zva_txt. "USER STORY 170715 / AOENNING
    APPEND i_objtxt.

    DESCRIBE TABLE i_objtxt LINES v_lines_txt.
    READ TABLE i_objtxt INDEX v_lines_txt.
    wa_doc_chng-obj_name = 'smartform'.
    wa_doc_chng-expiry_dat = sy-datum + 10.
    CONCATENATE TEXT-w02 wg_header-nro_sol_ov INTO wa_doc_chng-obj_descr
      SEPARATED BY space.
*     = 'smartform'.
    wa_doc_chng-sensitivty = 'F'.
    wa_doc_chng-doc_size = v_lines_txt * 255.
* Main Text
    CLEAR i_objpack-transf_bin.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    i_objpack-body_num = v_lines_txt.
    i_objpack-doc_type = 'RAW'.
    APPEND i_objpack.
* Attachment (pdf-Attachment)
    i_objpack-transf_bin = 'X'.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    DESCRIBE TABLE i_objbin LINES v_lines_bin.
    READ TABLE i_objbin INDEX v_lines_bin.
    i_objpack-doc_size = v_lines_bin * 255 .
    i_objpack-body_num = v_lines_bin.
    i_objpack-doc_type = 'PDF'.
    i_objpack-obj_name = 'smart'.
    i_objpack-obj_descr = wg_header-nro_sol_ov.
    APPEND i_objpack.
    CLEAR i_reclist.
    i_reclist-receiver = p_email.
    i_reclist-rec_type = 'U'.
    APPEND i_reclist.
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wa_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = i_objpack
        object_header              = wa_objhead
        contents_bin               = i_objbin
        contents_txt               = i_objtxt
        receivers                  = i_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-w03.
    ELSE.
      MESSAGE s836(sd) WITH TEXT-w04.
*WRITE:/ ‘Mail sent’.
    ENDIF.

  ENDIF.
ENDFORM.                    " GERA_PDF
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL_LIBERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email_liberacao .
  DATA: ls_control        TYPE ssfctrlop,
        ls_options        TYPE ssfcompop,
        xsfparam_line     TYPE ssfxsfp,
        job_output_info   TYPE ssfcrescl,
        v_bin_filesize    TYPE i,
        it_docs           TYPE STANDARD TABLE OF docs,
        it_lines          TYPE STANDARD TABLE OF tline,
        lv_fname          TYPE rs38l_fnam,
        lv_mail_recipient TYPE swotobjid,
        lv_mail_sender    TYPE swotobjid,
        lv_control        TYPE ssfctrlop,
        lv_name           TYPE so_name,
        lv_output         TYPE ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20),
        tl_html           TYPE trfresult,
        tl_graphics       TYPE tsf_xsf_gr.

  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        wl_t001w    TYPE t001w,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i,
        tl_zmail    TYPE TABLE OF zmail WITH HEADER LINE.

  SELECT *
    FROM zmail
     INTO  TABLE tl_zmail
     WHERE bukrs EQ wg_header-vkorg
       AND tcode EQ sy-tcode.

  IF sy-subrc IS NOT INITIAL.
    vl_form = 'ZSDS0003'.

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = vl_form
      IMPORTING
        fm_name            = vl_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
*  Impresora
    ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
*    LS_OPTIONS-TDDEST   = 'LOCL'.
    ls_options-tdimmed  = space.
    ls_options-tdnewid  = space.
*    LS_OPTIONS-TDNOARCH = C_X.

    ls_control-preview = space.
    ls_control-device  = 'PRINTER'.
*    LS_CONTROL-GETOTF  = 'X'.
*    LS_OPTIONS-XSF  = 'X'.
*    LS_OPTIONS-XSFOUTMODE  = 'A'.

* activate XSF Output Mode
    ls_options-xsf        = 'X'.      " XSF Output active
    ls_options-xsfcmode   = 'X'.      " Get XSF params from program
    ls_options-xsfoutmode = 'A'.      " Application
    CLEAR ls_options-xsfoutdev.
    ls_options-xsfformat  = 'X'.      " Formatting ON

    xsfparam_line-name  = 'GRAPHICS'.
    xsfparam_line-value = 'EXTRACT'.
    APPEND xsfparam_line TO ls_options-xsfpars.
    xsfparam_line-name  = 'GRAPHICS-DIRECTORY'.             "#EC NOTEXT
    xsfparam_line-value = 'mygraphics/'.
    APPEND xsfparam_line TO ls_options-xsfpars.
    xsfparam_line-name  = 'CONTENT-ID'.                     "#EC NOTEXT
    xsfparam_line-value = 'ENABLE'.                         "#EC NOTEXT
    APPEND xsfparam_line TO ls_options-xsfpars.

    CLEAR:job_output_info.
    CALL FUNCTION vl_name
      EXPORTING
        user_settings      = ' '
        control_parameters = ls_control
        output_options     = ls_options
        i_nro_sol_ov       = wg_header-nro_sol_ov
      IMPORTING
        job_output_info    = job_output_info
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
    ELSE.
      tl_html       = job_output_info-xmloutput-trfresult.
      tl_graphics[] = job_output_info-xmloutput-xsfgr[].

      IF sy-uname EQ 'ABAP'.
* III. Put HTML and images into mail object

      ENDIF.


      IF sy-uname NE 'ABAP'.
        i_otf[] = job_output_info-otfdata[].
        CALL FUNCTION 'CONVERT_OTF'
          EXPORTING
            format                = 'PDF'
            max_linewidth         = 132
          IMPORTING
            bin_filesize          = v_bin_filesize
          TABLES
            otf                   = i_otf
            lines                 = i_tline
          EXCEPTIONS
            err_max_linewidth     = 1
            err_format            = 2
            err_conv_not_possible = 3
            OTHERS                = 4.
        IF sy-subrc EQ 0.
        ENDIF.
        LOOP AT i_tline.
          TRANSLATE i_tline USING '~'.
          CONCATENATE wa_buffer i_tline INTO wa_buffer.
        ENDLOOP.
        TRANSLATE wa_buffer USING '~'.
        DO.
          i_record = wa_buffer.
          APPEND i_record.
          SHIFT wa_buffer LEFT BY 255 PLACES.
          IF wa_buffer IS INITIAL.
            EXIT.
          ENDIF.
        ENDDO.
* Attachment
        REFRESH: i_reclist,
        i_objtxt,
        i_objbin,
        i_objpack.
        CLEAR wa_objhead.
        i_objbin[] = i_record[].
* Create Message Body Title and Description
        i_objtxt = TEXT-w05.
        APPEND i_objtxt.
        READ TABLE tg_itens INDEX 1.
        CONCATENATE TEXT-w06 wg_header-vkorg '-'
        wg_desc_vkorg INTO i_objtxt SEPARATED BY space.
        APPEND i_objtxt.
        READ TABLE tg_itens INDEX 1.
        SELECT SINGLE *
          FROM t001w
          INTO wl_t001w
            WHERE werks EQ tg_itens-werks.

        CONCATENATE TEXT-w07 tg_itens-werks '-'
        wl_t001w-name1 INTO i_objtxt SEPARATED BY space.
        APPEND i_objtxt.
        CONCATENATE TEXT-w08 wg_header-vkgrp '-'
        wg_desc_vkgrp INTO i_objtxt SEPARATED BY space.
        APPEND i_objtxt.
        DESCRIBE TABLE i_objtxt LINES v_lines_txt.
        READ TABLE i_objtxt INDEX v_lines_txt.
        wa_doc_chng-obj_name = 'smartform'.
        wa_doc_chng-expiry_dat = sy-datum + 10.
        CONCATENATE TEXT-w09 wg_header-nro_sol_ov INTO wa_doc_chng-obj_descr
          SEPARATED BY space.
*     = 'smartform'.
        wa_doc_chng-sensitivty = 'F'.
        wa_doc_chng-doc_size = v_lines_txt * 255.
* Main Text
        CLEAR i_objpack-transf_bin.
        i_objpack-head_start = 1.
        i_objpack-head_num = 0.
        i_objpack-body_start = 1.
        i_objpack-body_num = v_lines_txt.
        i_objpack-doc_type = 'RAW'.
        APPEND i_objpack.
* Attachment (pdf-Attachment)
        i_objpack-transf_bin = 'X'.
        i_objpack-head_start = 1.
        i_objpack-head_num = 0.
        i_objpack-body_start = 1.
        DESCRIBE TABLE i_objbin LINES v_lines_bin.
        READ TABLE i_objbin INDEX v_lines_bin.
        i_objpack-doc_size = v_lines_bin * 255 .
        i_objpack-body_num = v_lines_bin.
        i_objpack-doc_type = 'PDF'.
        i_objpack-obj_name = 'smart'.
        i_objpack-obj_descr = wg_header-nro_sol_ov.
        APPEND i_objpack.
        LOOP AT tl_zmail.
          CLEAR i_reclist.
          i_reclist-receiver = tl_zmail-email.
          i_reclist-rec_type = 'U'.
          APPEND i_reclist.

        ENDLOOP.
        CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
          EXPORTING
            document_data              = wa_doc_chng
            put_in_outbox              = 'X'
            commit_work                = 'X'
          TABLES
            packing_list               = i_objpack
            object_header              = wa_objhead
            contents_bin               = i_objbin
            contents_txt               = i_objtxt
            receivers                  = i_reclist
          EXCEPTIONS
            too_many_receivers         = 1
            document_not_sent          = 2
            document_type_not_exist    = 3
            operation_no_authorization = 4
            parameter_error            = 5
            x_error                    = 6
            enqueue_error              = 7
            OTHERS                     = 8.
        IF sy-subrc NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-w03.
        ELSE.
          MESSAGE s836(sd) WITH TEXT-w04.
*WRITE:/ ‘Mail sent’.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " ENVIA_EMAIL_LIBERACAO
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_saldo.
  DATA: tl_itens    LIKE TABLE OF tg_itens WITH HEADER LINE,
        tl_vbfa     TYPE TABLE OF vbfa WITH HEADER LINE,
        tl_vbfa_aux TYPE TABLE OF vbfa WITH HEADER LINE,
        wl_matnr    TYPE mara-matnr,
        wl_zieme    TYPE mara-meins,
        wl_pmein    TYPE mara-meins,
        wl_menge    TYPE ekpo-menge,
        wl_tabix    TYPE sy-tabix.

  REFRESH: tg_saldo.
  tl_itens[] = tg_itens[].

  DELETE tl_itens WHERE vbeln IS INITIAL.

  IF tl_itens[] IS NOT INITIAL.
    SELECT *
      FROM vbfa
      INTO TABLE tl_vbfa
       FOR ALL ENTRIES IN tl_itens
       WHERE vbelv EQ tl_itens-vbeln
         AND vbtyp_n EQ 'J'
         AND vbtyp_v EQ 'C'.

    IF tl_vbfa[] IS NOT INITIAL.
      SELECT *
        FROM vbfa
        INTO TABLE tl_vbfa_aux
         FOR ALL ENTRIES IN tl_vbfa
         WHERE vbeln EQ tl_vbfa-vbeln
           AND vbtyp_n EQ 'J'
           AND vbtyp_v EQ 'J'.

      LOOP AT tl_vbfa_aux.
        DELETE tl_vbfa WHERE vbeln EQ tl_vbfa_aux-vbeln.
      ENDLOOP.
    ENDIF.

    SORT tl_vbfa BY vbelv.
    REFRESH: tg_saldo.
    LOOP AT tg_itens.
      CLEAR: tg_saldo.
      LOOP AT tl_vbfa WHERE vbelv EQ tg_itens-vbeln.

        tg_saldo-vbeln = tl_vbfa-vbelv.
        tg_saldo-werks = tg_itens-werks.
        tg_saldo-total = tl_vbfa-rfmng.

        COLLECT tg_saldo.
        wl_tabix = sy-tabix.
*        CLEAR TG_SALDO.
      ENDLOOP.
      IF sy-subrc IS INITIAL.
        READ TABLE tg_saldo INDEX wl_tabix.
      ENDIF.

      wl_matnr = tg_itens-matnr.
      wl_zieme = tg_itens-zieme.
      wl_pmein = tl_vbfa-meins.
      wl_menge = tg_itens-zmeng.

      IF tl_vbfa IS NOT INITIAL.
        CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
          EXPORTING
            i_matnr              = wl_matnr
            i_in_me              = wl_zieme
            i_out_me             = wl_pmein
            i_menge              = wl_menge
          IMPORTING
            e_menge              = wl_menge
          EXCEPTIONS
            error_in_application = 1
            error                = 2
            OTHERS               = 3.
      ENDIF.

      tg_saldo-zmeng = wl_menge.
      tg_saldo-saldo = tg_saldo-zmeng - tg_saldo-total.

      IF wl_tabix IS NOT INITIAL.
        MODIFY tg_saldo INDEX wl_tabix.
      ELSE.
        tg_saldo-vbeln = tg_itens-vbeln.
        tg_saldo-werks = tg_itens-werks.
        APPEND tg_saldo.
      ENDIF.
      CLEAR: wl_tabix.
*      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " BUSCA_SALDO
*&---------------------------------------------------------------------*
*&      Form  EXIBE_SALDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibe_saldo .
  DATA: wl_layout TYPE  slis_layout_alv.
  wl_layout-zebra = c_x.
  wl_layout-window_titlebar = TEXT-t03."'Solicitação Ordem de Venda - Saldo'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      is_layout             = wl_layout
      it_fieldcat           = estrutura[]
*     IT_SORT               = T_SORT[]
      i_default             = ' '
      i_save                = ' '
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 60
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_saldo.
ENDFORM.                    " EXIBE_SALDO
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  DATA: x_contador TYPE string.
  CLEAR: wa_estrutura, x_contador.

  x_contador = strlen( p_scrtext_l ).

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-edit          = p_edit.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-reptext_ddic  = p_scrtext_l.
  wa_estrutura-outputlen     = x_contador.

  IF p_field EQ 'SALDO'.
    wa_estrutura-do_sum = c_x.
  ENDIF.

  IF ( p_field EQ 'NIVEL' ) OR ( p_field EQ 'APROVADOR' ).
    wa_estrutura-no_zero = abap_true.
    wa_estrutura-just    = 'C'.
  ENDIF.


  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  REDISTRIBUIR_QTD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM redistribuir_qtd .
  PERFORM busca_saldo.
  CLEAR: wg_redistribuir_titem.

  LOOP AT tg_itens.
    ADD tg_itens-zmeng TO wg_redistribuir_titem .
  ENDLOOP.
  PERFORM calcula_redistribuicao.
  wg_redistribuir = c_x.

ENDFORM.                    " REDISTRIBUIR_QTD
*&---------------------------------------------------------------------*
*&      Form  CALCULA_REDISTRIBUICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM calcula_redistribuicao .
  CLEAR: wg_redistribuir_total, wg_redistribuir_saldo.
  LOOP AT tg_saldo.
    ADD tg_saldo-saldo TO wg_redistribuir_total.
  ENDLOOP.

  LOOP AT tg_itens.
    ADD tg_itens-zmeng TO wg_redistribuir_saldo.
    IF tg_itens-item_edit EQ c_r.
      SUBTRACT tg_itens-zmeng FROM wg_redistribuir_total.
    ENDIF.
  ENDLOOP.

  wg_redistribuir_saldo = wg_redistribuir_saldo - wg_redistribuir_titem.
  wg_redistribuir_total = ( wg_redistribuir_total + ( wg_redistribuir_saldo * -1 ) ).

  " 21.03.2025 - RAMON -->
  "PERFORM f_reg_aba_preco_redist.
  " 21.03.2025 - RAMON --<

ENDFORM.                    " CALCULA_REDISTRIBUICAO

*&---------------------------------------------------------------------*
*&      Form  liberacao adiantamento
*&---------------------------------------------------------------------*
FORM liberacao_performance_adiant.

  DATA : tl_nro_sol_ov   TYPE TABLE OF zsds007 WITH HEADER LINE,
         tl_saida_exec   TYPE TABLE OF zsds008 WITH HEADER LINE,
         tg_selectedcell TYPE lvc_t_cell,
         wg_selectedcell TYPE lvc_s_cell,
         tl_input_0063   TYPE TABLE OF zsdt0063 WITH HEADER LINE.

*-#82210-jaime
  REFRESH: tl_nro_sol_ov, tl_saida_exec.

  CALL METHOD grid5->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid5->get_selected_cells
    IMPORTING
      et_cell = tg_selectedcell.

*-CS2022000332-#78223-02.06.2022-JT-inicio
  IF wg_header-param_espec EQ c_a OR wg_header-param_espec EQ  c_z AND wg_header-nro_sol_ov IS NOT INITIAL.
    LOOP AT tg_adto_ext.
      MOVE-CORRESPONDING: tg_adto_ext TO tl_input_0063.
      MOVE: wg_header-nro_sol_ov  TO tl_input_0063-nro_sol_ov,
            sy-uname              TO tl_input_0063-usnam,
            sy-datum              TO tl_input_0063-data_atual,
            sy-uzeit              TO tl_input_0063-hora_atual.

      APPEND tl_input_0063.
      CLEAR:tl_input_0063.
    ENDLOOP.
    DELETE FROM zsdt0063 WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
    MODIFY zsdt0063 FROM TABLE tl_input_0063.
  ENDIF.
*-CS2022000332-#78223-02.06.2022-JT-fim

  READ TABLE tg_selectedcell INTO wg_selectedcell INDEX 1.

  IF sy-subrc <> 0 OR wg_selectedcell-row_id-index IS INITIAL OR
                      wg_selectedcell-col_id-fieldname <> 'BUKRS'.
    MOVE wg_header-nro_sol_ov   TO tl_nro_sol_ov-nro_sol_ov.
    APPEND tl_nro_sol_ov.
  ELSE.
    LOOP AT tg_selectedcell   INTO wg_selectedcell.
      READ TABLE tg_adto_ext  INTO wl_adto_ext INDEX wg_selectedcell-row_id-index.
      MOVE wg_header-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov.
      MOVE wl_adto_ext-posnr    TO tl_nro_sol_ov-posnr.
      APPEND tl_nro_sol_ov.
    ENDLOOP.
  ENDIF.

  IF tl_nro_sol_ov[] IS NOT INITIAL.
    CALL FUNCTION 'ZSDMF004_GERA_ADIANTAMENTO'
      TABLES
        ti_nro_sol_ov = tl_nro_sol_ov
        te_return     = tl_saida_exec.
  ENDIF.

  LOOP AT tg_adto_ext  INTO wl_adto_ext.
    DATA(l_tabix) = sy-tabix.

    IF wl_adto_ext-adiant IS INITIAL.
      SELECT SINGLE adiant
        FROM zsdt0063
        INTO @DATA(l_adiant)
        WHERE nro_sol_ov = @wg_header-nro_sol_ov
        AND   posnr      = @wl_adto_ext-posnr
        AND   valdt      = @wl_adto_ext-valdt.
      IF sy-subrc IS INITIAL.
        wl_adto_ext-adiant    = l_adiant.
        MODIFY tg_adto_ext FROM wl_adto_ext INDEX l_tabix.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF tl_saida_exec[] IS NOT INITIAL.
    PERFORM montar_layout USING 'SAIDA_EXEC'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       i_callback_program    = v_report
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        it_fieldcat           = estrutura[]
*       IT_SORT               = T_SORT[]
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      TABLES
        t_outtab              = tl_saida_exec.
  ENDIF.

ENDFORM.                    " LIBERACAO_PERFORMANCE

*&---------------------------------------------------------------------*
*&      Form  EXECUTA_ADTO_EXT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM liberacao_performance .
  DATA : tl_nro_sol_ov TYPE TABLE OF zsds007 WITH HEADER LINE,
         tl_saida_exec TYPE TABLE OF zsds008 WITH HEADER LINE.
  REFRESH: tl_nro_sol_ov.

  MOVE wg_header-nro_sol_ov TO tl_nro_sol_ov-nro_sol_ov.
  APPEND tl_nro_sol_ov.

  IF tl_nro_sol_ov[] IS NOT INITIAL.
    CALL FUNCTION 'ZSDMF004_GERA_ADIANTAMENTO'
      TABLES
        ti_nro_sol_ov = tl_nro_sol_ov
        te_return     = tl_saida_exec.

  ENDIF.

  IF tg_adto_ext-adiant IS INITIAL.
    SELECT SINGLE adiant
      FROM zsdt0063
      INTO tg_adto_ext-adiant
      WHERE nro_sol_ov = wg_header-nro_sol_ov
      AND   posnr      = tg_adto_ext-posnr
      AND   valdt      = tg_adto_ext-valdt.
    IF sy-subrc IS INITIAL.
      LOOP AT tg_adto_ext INTO DATA(ls_adto_ext).
        IF ls_adto_ext-posnr EQ tg_adto_ext-posnr AND ls_adto_ext-valdt EQ tg_adto_ext-valdt.
          MODIFY tg_adto_ext FROM tg_adto_ext INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF tl_saida_exec[] IS NOT INITIAL.
    PERFORM montar_layout USING 'SAIDA_EXEC'.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
*       i_callback_program    = v_report
*       I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
        it_fieldcat           = estrutura[]
*       IT_SORT               = T_SORT[]
        i_save                = 'A'
        i_screen_start_column = 3
        i_screen_start_line   = 3
        i_screen_end_column   = 100
        i_screen_end_line     = 13
      TABLES
        t_outtab              = tl_saida_exec.
  ENDIF.
ENDFORM.                    " LIBERACAO_PERFORMANCE
*&---------------------------------------------------------------------*
*&      Form  SOL_APROV_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sol_aprov_email .
  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        wl_t001w    TYPE t001w,
        tl_0064     TYPE TABLE OF zsdt0064 WITH HEADER LINE,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i.

*  CLEAR: wl_0064.
* Create Message Body Title and Description
  CONCATENATE TEXT-w10 wg_header-nro_sol_ov
   INTO  i_objtxt SEPARATED BY space.
  APPEND i_objtxt.
  READ TABLE tg_itens INDEX 1.
  CONCATENATE TEXT-w06 wg_header-vkorg '-'
  wg_desc_vkorg INTO i_objtxt SEPARATED BY space.
  APPEND i_objtxt.
  READ TABLE tg_itens INDEX 1.
  SELECT SINGLE *
    FROM t001w
    INTO wl_t001w
      WHERE werks EQ tg_itens-werks.

  CONCATENATE TEXT-w07 tg_itens-werks '-'
  wl_t001w-name1 INTO i_objtxt SEPARATED BY space.
  APPEND i_objtxt.
  CONCATENATE TEXT-w08 wg_header-vkgrp '-'
  wg_desc_vkgrp INTO i_objtxt SEPARATED BY space.
  APPEND i_objtxt.
  DESCRIBE TABLE i_objtxt LINES v_lines_txt.
  READ TABLE i_objtxt INDEX v_lines_txt.
  wa_doc_chng-obj_name = TEXT-w11.
  wa_doc_chng-expiry_dat = sy-datum + 10.
  CONCATENATE TEXT-w12 wg_header-nro_sol_ov
 INTO  wa_doc_chng-obj_descr  SEPARATED BY space.
*     = 'smartform'.
  wa_doc_chng-sensitivty = 'F'.
  wa_doc_chng-doc_size = v_lines_txt * 255.
* Main Text
  CLEAR i_objpack-transf_bin.
  i_objpack-head_start = 1.
  i_objpack-head_num = 0.
  i_objpack-body_start = 1.
  i_objpack-body_num = v_lines_txt.
  i_objpack-doc_type = 'RAW'.
  APPEND i_objpack.
* Attachment (pdf-Attachment)
  i_objpack-transf_bin = 'X'.
  i_objpack-head_start = 1.
  i_objpack-head_num = 0.
  i_objpack-body_start = 1.

  SELECT *
    FROM zsdt0064
    INTO TABLE tl_0064
     WHERE werks EQ wg_header-vkbur.

  LOOP AT tl_0064.
    REFRESH i_reclist.
    CLEAR i_reclist.
    i_reclist-receiver = tl_0064-email.
    i_reclist-rec_type = 'U'.
    APPEND i_reclist.

*      ENDLOOP.
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wa_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = i_objpack
        object_header              = wa_objhead
        contents_bin               = i_objbin
        contents_txt               = i_objtxt
        receivers                  = i_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-w03.
    ELSE.
      MESSAGE s836(sd) WITH TEXT-w04.
*WRITE:/ ‘Mail sent’.
    ENDIF.
  ENDLOOP.

*    ENDIF.
ENDFORM.                    " SOL_APROV_EMAIL
*&---------------------------------------------------------------------*
*&      Form  FORME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email_html TABLES lt_obj_cont STRUCTURE solisti1
                      USING p_mail p_assunto.
*--------------------------------------------------------------------*
*Mailing Related Data Decleration
*--------------------------------------------------------------------*
  DATA : ls_type     TYPE sood-objtp,

         lt_obj_head TYPE TABLE OF solisti1,
         ls_obj_head TYPE solisti1,

*         LT_OBJ_CONT TYPE TABLE OF SOLISTI1,
         ls_obj_cont TYPE solisti1,

         lt_recever  TYPE TABLE OF somlreci1,
         ls_recever  TYPE somlreci1,

         lv_date     TYPE char10,
*         LV_STR TYPE STRING,
         wl_email    TYPE adr6-smtp_addr.
*Refresh Tables
  REFRESH : lt_obj_head,  lt_recever. "lt_member.

* Type
  MOVE: 'HTML' TO ls_type,
         p_mail TO wl_email.

*--------------------------------------------------------------------*
*Send Email Via Class
*--------------------------------------------------------------------*
  DATA: lo_document    TYPE REF TO cl_document_bcs,
        lo_bcs         TYPE REF TO cl_bcs,
        lo_sapuser_bcs TYPE REF TO cl_sapuser_bcs,
        lo_recipient   TYPE REF TO if_recipient_bcs,
        lo_ex_bcs      TYPE REF TO cx_bcs,
        lv_message     TYPE string.

  CLEAR: lo_document.

  DATA : lv_sub TYPE so_obj_des.
  lv_sub = p_assunto.

  lo_document = cl_document_bcs=>create_document(
    i_type    = 'HTM'
    i_subject = lv_sub
    i_text    = lt_obj_cont[] ). "lt_txt_cont

  lo_bcs = cl_bcs=>create_persistent( ).
  lo_bcs->set_document( lo_document ).

  lo_recipient = cl_cam_address_bcs=>create_internet_address( wl_email ).

  lo_bcs->set_message_subject( ip_subject = p_assunto ).   "Subject

*--------------------------------------------------------------------*
*Image from MIME
*--------------------------------------------------------------------*

  DATA: o_mr_api         TYPE REF TO if_mr_api.

  DATA is_folder TYPE boole_d.
  DATA l_img1 TYPE xstring.
  DATA l_img2 TYPE xstring.
  DATA l_loio TYPE skwf_io.

  IF o_mr_api IS INITIAL.

    o_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

  ENDIF.
  CALL METHOD o_mr_api->get
    EXPORTING
      i_url              = '/SAP/PUBLIC/zmime/amaggi.gif'
    IMPORTING
      e_is_folder        = is_folder
      e_content          = l_img1
      e_loio             = l_loio
    EXCEPTIONS
      parameter_missing  = 1
      error_occured      = 2
      not_found          = 3
      permission_failure = 4
      OTHERS             = 5.

  DATA :lt_hex1      TYPE solix_tab,
        lt_hex2      TYPE solix_tab,
        ls_hex       LIKE LINE OF lt_hex1,
        lv_img1_size TYPE sood-objlen,
        lv_img2_size TYPE sood-objlen.

  CLEAR : lt_hex1, lt_hex2, ls_hex, lv_img1_size, lv_img2_size.

  WHILE l_img1 IS NOT INITIAL.
    ls_hex-line = l_img1.
    APPEND ls_hex TO lt_hex1.
    SHIFT l_img1 LEFT BY 255 PLACES IN BYTE MODE.
  ENDWHILE.

*Findthe Size of the image
  DESCRIBE TABLE lt_hex1 LINES lv_img1_size.
  lv_img1_size = lv_img1_size * 255.

*--------------------------------------------------------------------*
*Attach Images
*--------------------------------------------------------------------*

  TRY . " 22.10.2024 - 154003 - RAMON -->

      lo_document->add_attachment(
        EXPORTING
          i_attachment_type    = 'gif'                  " Document Class for Attachment
          i_attachment_subject = 'img1'                " Attachment Title
          i_attachment_size    = lv_img1_size           " Size of Document Content
          i_att_content_hex    = lt_hex1  " Content (Binary)
      ).

    CATCH cx_document_bcs." 22.10.2024 - 154003 - RAMON --

  ENDTRY." 22.10.2024 - 154003 - RAMON --

*--------------------------------------------------------------------*
*Add the recipient
*--------------------------------------------------------------------*
  TRY.
      CALL METHOD lo_bcs->add_recipient
        EXPORTING
          i_recipient = lo_recipient
          i_express   = 'X'.
    CATCH cx_send_req_bcs.
  ENDTRY.

  lo_sapuser_bcs = cl_sapuser_bcs=>create( sy-uname ).
  lo_bcs->set_sender( i_sender = lo_sapuser_bcs ).
  lo_bcs->set_send_immediately( 'X' ).

*--------------------------------------------------------------------*
*Send Mail
*--------------------------------------------------------------------*
  TRY.
      CALL METHOD lo_bcs->send( ).

      COMMIT WORK.
      MESSAGE 'Send Successfully' TYPE 'S'.
    CATCH cx_bcs INTO lo_ex_bcs.
      lv_message = lo_ex_bcs->get_text( ).
  ENDTRY.
ENDFORM.                    " FORME
*&---------------------------------------------------------------------*
*&      Form  GERA_HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_html_liberacao.

  DATA: lt_obj_cont TYPE TABLE OF solisti1,
        ls_obj_cont TYPE solisti1,
        tl_zmail    TYPE TABLE OF zmail WITH HEADER LINE.

  DEFINE add_html.

    ls_obj_cont-line = &1.
    APPEND ls_obj_cont TO lt_obj_cont.
  END-OF-DEFINITION.
*--------------------------------------------------------------------*
* Heading and Style Patterns
*--------------------------------------------------------------------*
  DATA: wl_field(300),
        wl_qtd(20),
        wl_data(10),
        wl_t001w      TYPE t001w,
        wl_matnr(18),
        wl_kna1       TYPE kna1,
        wl_assunto    TYPE string.

*Refresh Tables
  REFRESH : lt_obj_cont, tl_zmail.

  SELECT SINGLE *
    FROM kna1
    INTO wl_kna1
     WHERE kunnr EQ wg_header-kunnr.

  add_html:
  '<!DOCTYPE html>',
  '<html>',
  '<head>',
  '<title>',
  TEXT-w13,'</title>',
  '<style type="text/css">',
  'body,td,th {',
  '  font-family: "Arial", Times, serif; }',
  '</style>',
  '</head>',
'<p><img src= "cid:img1.gif"></p>',
******Header do HTML
  '<table width="700" height="361" border="1">',
  '  <tr>',
  '    <td width="801" valign="top"><table width="800" border="0">',
  '      <tr>',
  '       <td width="173">',TEXT-w14,'</td>',
  '        <td width="166">',TEXT-w15,'</td>',
  '        <td width="135">',TEXT-w16,'</td>',
  '        <td width="160">',TEXT-w17,'</td>',
  '      </tr>',
  '      <tr>'.
  CONCATENATE '          <td>' wg_header-nro_sol_ov '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field.
*  '       <td>#NRO_SOL_OV#</td>',
  CONCATENATE '          <td>' wg_header-auart '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field.
*  '        <td>#AUART#</td>',
  WRITE wg_header-data_atual TO wl_data.
  CONCATENATE '          <td>' wl_data '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field.
*  '        <td>#DATA_ATUAL#</td>',
  CONCATENATE '          <td>' wg_header-inco1 '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field,
*  '        <td>#INCO1#</td>',
  '     </tr>',
  '    </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '          <td width="260">',TEXT-w18,'</td>',
*  '          <td width="262">',text-W07,'</td>',
  '          <td width="262">',' ','</td>',
  '          <td width="264">',TEXT-w08,'</td>',
  '        </tr>',
  '        <tr>'.
  CONCATENATE '          <td width="260">' wg_header-vkorg '-' wg_desc_vkorg '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*  '          <td>#VKORG#</td>',
  READ TABLE tg_itens INDEX 1.
  SELECT SINGLE *
    FROM t001w
    INTO wl_t001w
      WHERE werks EQ tg_itens-werks.
*  CONCATENATE '          <td width="262">' TG_ITENS-WERKS '-' WL_T001W-NAME1 '</td>' INTO WL_FIELD.
  CONCATENATE '          <td width="262">'  '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*  '          <td>#WERKS#</td>'.
  CONCATENATE '          <td width="264">' wg_header-vkgrp '-' wg_desc_vkgrp '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*  '          <td>#VKGRP#</td>',
  '        </tr>',
  '      </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '         <td width="426">',TEXT-w19,'</td>',
  '          <td width="286">',TEXT-w20,'</td>',
  '          <td width="88">',TEXT-w21,'</td>',
  '        </tr>',
  '        <tr>'.
  CONCATENATE '          <td width="426">' wg_header-kunnr '-' wg_desc_kunnr '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*'          <td>#KUNNR#</td>',
  CONCATENATE '          <td width="286">' wl_kna1-ort01 '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*'          <td width="286">#LOCAL#</td>',
  CONCATENATE '          <td width="286">' wl_kna1-regio '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*'          <td width="88">#UF#</td>',
'        </tr>',
'      </table>',
'      <br />',
'      <table width="800" border="0">',
'        <tr>',
'          <td width="486">',TEXT-w22,'</td>',
'          <td width="486">', TEXT-w29 ,'</td>',
'          <td width="214" align="center">',TEXT-w23,'</td>',
'          <td width="100"> </td>',
'        </tr>',
'        <tr>'.

  LOOP AT tg_itens.
    wl_matnr = tg_itens-matnr.
    SHIFT wl_matnr LEFT DELETING LEADING '0'.
    CONCATENATE '          <td width="486">' wl_matnr ' - ' tg_itens-maktx '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
    add_html: wl_field.

    CONCATENATE '          <td width="486">' tg_itens-werks '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
    add_html: wl_field.

    WRITE tg_itens-zmeng TO wl_qtd.
    CONDENSE wl_qtd NO-GAPS.
    CONCATENATE '          <td width="214" align="right">' wl_qtd '</td>' INTO wl_field.
    add_html: wl_field.
*    '          <td align="right">&nbsp;</td>',
    CONCATENATE '          <td width="100">' tg_itens-zieme '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
    add_html: wl_field,
*    '          <td>&nbsp;</td>',
    '        </tr>  '.
  ENDLOOP.
  add_html:
  '      </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '          <td width="787">',TEXT-w24,'</td>',
  '        </tr>',
  '        <tr>'.
  CONCATENATE '          <td>' wg_header-observacao '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*    '          <td>&nbsp;</td>',
  '        </tr>',
  '      </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '          <td colspan="2">',TEXT-w25,'</td>',
  '          <td width="164">&nbsp;</td>',
  '          <td width="371">&nbsp;</td>',
  '        </tr>',
  '        <tr>',
  '          <td width="70">',TEXT-w26,'</td>'.
  WRITE wg_header-dtde_logist TO wl_data..
  CONCATENATE '          <td width="192">' wl_data '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*    '          <td width="127">&nbsp;</td>',
  '          <td width="60">',TEXT-w27,'</td>'.
  WRITE wg_header-dtate_logist TO wl_data.
  CONCATENATE '          <td width="460">' wl_data '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
**    '          <td wid th="545">&nbsp;</td>',
  '        </tr>',
  '    </table>',
  '    <br /></td>',
  '  </tr>',
  '</table>',
  '<p>&nbsp;</p>',
  '</body>',
  '</html>'.

  SELECT *
    FROM zmail
     INTO  TABLE tl_zmail
     WHERE bukrs EQ wg_header-vkorg
       AND param_espec EQ wg_header-param_espec
       AND tcode EQ sy-tcode.

  CONCATENATE TEXT-w28 wg_header-nro_sol_ov TEXT-s02
   INTO wl_assunto SEPARATED BY space.

  tg_itens_mail[] = tg_itens[].
  SORT tg_itens_mail[] BY werks.
  DELETE ADJACENT DUPLICATES FROM tg_itens_mail[] COMPARING werks.

  LOOP AT tg_itens_mail.
    READ TABLE tl_zmail TRANSPORTING NO FIELDS WITH KEY werks = tg_itens_mail-werks.
    IF sy-subrc IS INITIAL.
      LOOP AT tl_zmail WHERE werks = tg_itens_mail-werks.
        PERFORM envia_email_html TABLES lt_obj_cont
                                 USING tl_zmail-email
                                       wl_assunto.
      ENDLOOP.
    ELSE.
      LOOP AT tl_zmail WHERE werks = ''.
        PERFORM envia_email_html TABLES lt_obj_cont
                                 USING tl_zmail-email
                                       wl_assunto.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*  LOOP AT TL_ZMAIL.
*      PERFORM ENVIA_EMAIL_HTML TABLES LT_OBJ_CONT
*                               USING TL_ZMAIL-EMAIL
*                                     WL_ASSUNTO.
*    ELSE.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " GERA_HTML
*&---------------------------------------------------------------------*
*&      Form  SHOW_HISTORICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_historico .
  DATA: wl_layout TYPE  slis_layout_alv.
  DATA: BEGIN OF tl_motivo OCCURS 0,
          id_historico TYPE zsdt0069-id_historico,
          nro_sol_ov   TYPE zsdt0069-nro_sol_ov,
          nivel(4),
          status(25),
          motivo       TYPE zsdt0069-motivo,
          usnam        TYPE zsdt0069-usnam,
          data_atual   TYPE zsdt0069-data_atual,
          hora_atual   TYPE zsdt0069-hora_atual,
        END OF tl_motivo.

  REFRESH: tl_motivo.
  LOOP AT tg_motivo.
    MOVE-CORRESPONDING: tg_motivo TO tl_motivo.

    READ TABLE tg_0162[] INTO DATA(wl_0162) WITH KEY id_log = tl_motivo-id_historico
                                                     vbeln  = tl_motivo-nro_sol_ov.

    tl_motivo-nivel = wl_0162-nivel.

    IF tg_motivo-status EQ 'L'.
      tl_motivo-status = TEXT-s02.
    ELSEIF tg_motivo-status EQ 'R'.
      tl_motivo-status = TEXT-s04.
    ENDIF.
    APPEND tl_motivo.
  ENDLOOP.

  PERFORM montar_layout USING 'MOTIVO'.
  wl_layout-zebra = c_x.
  wl_layout-colwidth_optimize = c_x.
  wl_layout-window_titlebar = TEXT-t04."'Solicitação Ordem de Venda - Histórico'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
*     I_CALLBACK_USER_COMMAND = 'XUSER_COMMAND'
      is_layout             = wl_layout
      it_fieldcat           = estrutura[]
*     I_STRUCTURE_NAME      = 'ZSDS013'
*     IT_SORT               = T_SORT[]
      i_default             = ' '
      i_save                = ' '
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tl_motivo.
ENDFORM.                    " SHOW_HISTORICO
*&---------------------------------------------------------------------*
*&      Form  build_hierarchy_header
*&---------------------------------------------------------------------*
*       build hierarchy-header-information
*----------------------------------------------------------------------*
*      -->P_L_HIERARCHY_HEADER  strucxture for hierarchy-header
*----------------------------------------------------------------------*
FORM build_hierarchy_header CHANGING
                               p_hierarchy_header TYPE treev_hhdr.

  p_hierarchy_header-heading = TEXT-t05."'Item Fixação'.
  p_hierarchy_header-tooltip = TEXT-t05."'Item Fixação'.
  p_hierarchy_header-width = 50.
  p_hierarchy_header-width_pix = ''.

ENDFORM.                               " build_hierarchy_header
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_hierarchy.

* add data to tree
  DATA: l_posnr_key   TYPE lvc_nkey,
        l_header_key  TYPE lvc_nkey,
        l_last_key    TYPE lvc_nkey,
        wl_posnr      TYPE zsdt0059-posnr,
        wl_fix        TYPE sy-tabix,
        wl_texto      TYPE lvc_value,
        wl_fix_aux(4),
        wl_sort1(30)  VALUE 'POSNR',
        wl_sort2(30)  VALUE 'NIVEL'.


*CALL METHOD tree1->delete_all_nodes.
  REFRESH: tg_preco_frame, <fs_table_frame>.
  SORT: tg_preco BY posnr ASCENDING nivel ASCENDING .
  SORT: <fs_table> BY (wl_sort1) ASCENDING (wl_sort2) ASCENDING.

  LOOP AT <fs_table> INTO <fs_line>.
    IF sy-tabix EQ 1.
      PERFORM add_header_line USING    <fs_line>
                                       ''
                              CHANGING l_header_key.
    ENDIF.
    CLEAR: wg_valor.
    PERFORM get_set_valores USING 'INVISIBLE'
                                  'G'
                          CHANGING wg_valor.
    CONDENSE wg_valor NO-GAPS.
    IF wg_valor IS INITIAL.
      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                            CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.
*    ON CHANGE OF tg_preco-posnr.

      IF wl_posnr IS INITIAL
      OR wl_posnr NE  wg_valor. "TG_PRECO-POSNR.
        ADD 1 TO wl_fix.
        wl_fix_aux = wl_fix.
        SHIFT wl_fix_aux LEFT DELETING LEADING '0'.
        CONDENSE wl_fix_aux NO-GAPS.

        CONCATENATE TEXT-e69 wl_fix_aux '-' wg_valor INTO wl_texto SEPARATED BY space.
        PERFORM add_posnr_line USING    <fs_line>
                                        l_header_key
                                        wl_texto
                                CHANGING l_posnr_key.
      ENDIF.
      PERFORM add_complete_line USING  <fs_line>
                                       l_posnr_key
                              CHANGING l_last_key.

      CLEAR: wl_posnr.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                            CHANGING wl_posnr.
      CONDENSE wl_posnr NO-GAPS.
*    WL_POSNR = TG_PRECO-POSNR.
      PERFORM get_set_valores USING 'ITEM_KEY'
                                        'S'
                           CHANGING l_last_key.
      tg_preco-item_key = l_last_key.
*    MODIFY TG_PRECO .
      MODIFY <fs_table> FROM <fs_line>.
    ENDIF.
  ENDLOOP.

  CALL METHOD tree1->expand_node
    EXPORTING
      i_node_key       = l_header_key
      i_expand_subtree = space.
* calculate totals
  CALL METHOD tree1->update_calculations.

* this method must be called to send the data to the frontend
  CALL METHOD tree1->frontend_update.
ENDFORM.                    " CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*&      Module  REFRESH_PRECO_FRAME  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE refresh_preco_frame INPUT.
  PERFORM refresh_preco_frame.
ENDMODULE.                 " REFRESH_PRECO_FRAME  INPUT
*&---------------------------------------------------------------------*
*&      Form  REFRESH_PRECO_FRAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM refresh_preco_frame .
  DATA: tl_preco_aux LIKE TABLE OF tg_preco WITH HEADER LINE,
        wl_posnr     TYPE zsdt0059-posnr,
        wl_init      TYPE c,
        wl_tabix     TYPE sy-tabix.

  DATA: p_2(13) TYPE p DECIMALS 2,
        p_5(13) TYPE p DECIMALS 5,
        p_4(13) TYPE p DECIMALS 4.

  FIELD-SYMBOLS: <fs_table_aux> TYPE STANDARD TABLE.

  CREATE DATA t_new_table LIKE  <fs_table>.
  ASSIGN t_new_table->* TO <fs_table_aux>.

  UNASSIGN <fs_line>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.

  ASSIGN t_new_line->* TO <fs_line>.
  ASSIGN t_new_line->* TO <fs_line_aux>.

  IF wg_header-param_espec EQ c_m OR wg_header-param_espec EQ c_z.
    CLEAR ok_code.
    READ TABLE <fs_table> INTO <fs_line> INDEX 1.
    CLEAR: wg_valor.
    PERFORM get_set_valores USING 'POSNR'
                                  'G'
                           CHANGING wg_valor.
    CONDENSE wg_valor NO-GAPS.

    LOOP AT <fs_table> INTO <fs_line>.
      wl_tabix = sy-tabix.
      CLEAR: wg_valor_aux.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                             CHANGING wl_posnr.
      CONDENSE wg_valor NO-GAPS.
      IF wg_valor NE wl_posnr.
        DELETE <fs_table> INDEX wl_tabix.
      ENDIF.
    ENDLOOP.
*    DELETE TG_PRECO WHERE POSNR NE TG_PRECO-POSNR.
*  TL_PRECO_AUX[]  = TG_PRECO[].
    <fs_table_aux>[] = <fs_table>[].

    CALL METHOD tree1->delete_all_nodes.
    REFRESH: tg_preco, tg_preco_frame, <fs_table>, <fs_table_frame>,
             tg_cond_esp.
** calculate totals
*    CALL METHOD tree1->update_calculations.
*
*** this method must be called to send the data to the frontend
*    CALL METHOD tree1->frontend_update.

*    IF TG_ITENS[] IS INITIAL.
*      WL_INIT = C_X.
*    ENDIF.
    DO wg_header-num_fixacao TIMES.
*      IF WL_INIT IS NOT INITIAL.
*        CALL METHOD OBG_TOOLBAR->HANDLE_USER_COMMAND_ITENS( E_UCOMM = 'ADD' ).
*      ENDIF.
      CLEAR: tg_cond_esp, wa_style.
      REFRESH: tg_cond_esp-style, style.

      wl_posnr = sy-index.
      wa_style-fieldname = 'QTE_VENC'.
      wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
      INSERT  wa_style INTO TABLE style .
      INSERT LINES OF style INTO TABLE tg_cond_esp-style.
      tg_cond_esp-fixacao = sy-index.
      APPEND tg_cond_esp.
*      LOOP AT TL_PRECO_AUX.
*        TL_PRECO_AUX-POSNR = WL_POSNR.
*        APPEND TL_PRECO_AUX TO TG_PRECO.
*        CLEAR: TG_PRECO.
*      ENDLOOP.
      LOOP AT tg_preco_n.
        CLEAR: wl_tabix.
        LOOP AT <fs_table> INTO <fs_line_aux>.
          CLEAR: wg_valor, wl_tabix.
          ASSIGN COMPONENT 'POSNR'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
          wg_valor = <fs_campo>.

          IF wg_valor EQ wl_posnr.
            CLEAR: wg_valor.
            ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
            wg_valor = <fs_campo>.
            IF wg_valor EQ tg_preco_n-cod_fp.
              wl_tabix = sy-tabix.
            ENDIF.
          ENDIF.
        ENDLOOP.
        PERFORM get_set_valores USING 'PRECO_ITEM'
                                      'S'
                                CHANGING  tg_preco_n-preco.

        CLEAR: tg_preco_n-preco.
        MOVE-CORRESPONDING tg_preco_n TO <fs_line>.

        PERFORM get_set_valores USING 'POSNR'
                                      'S'
                                CHANGING wl_posnr.

        IF tg_preco_n-tipo_calc EQ c_f.
          CONDENSE tg_preco_n-formula NO-GAPS.
          PERFORM get_set_valores USING tg_preco_n-field
                                         'S'
                                CHANGING tg_preco_n-formula.
        ELSE.
          CLEAR tg_preco_n-formula.
          PERFORM get_set_valores USING tg_preco_n-field
                                         'S'
                                CHANGING tg_preco_n-formula.
        ENDIF.
        IF wl_tabix IS INITIAL.
          APPEND <fs_line> TO <fs_table>.
        ELSEIF wl_tabix IS NOT INITIAL.
          MODIFY <fs_table> FROM <fs_line> INDEX wl_tabix.
        ENDIF.
      ENDLOOP.
    ENDDO.

    PERFORM create_hierarchy.
  ENDIF.
  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.

ENDFORM.                    " REFRESH_PRECO_FRAME
*&---------------------------------------------------------------------*
*&      Form  add_posnr_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 1 to tree
*----------------------------------------------------------------------*
*      -->P_LS_PRECO  TY_PRECO
*      -->P_RELEATKEY   relatkey
*     <-->p_node_key    new node-key
*----------------------------------------------------------------------*
FORM add_posnr_line USING     ps_preco "TYPE TY_PRECO
                               p_relat_key TYPE lvc_nkey
                               p_text TYPE lvc_value
                     CHANGING  p_node_key TYPE lvc_nkey.

  FIELD-SYMBOLS: <fs_preco> TYPE any.
  UNASSIGN <fs_preco>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.

* Cria uma field-symbol como work area
  ASSIGN t_new_line->* TO <fs_preco>.

  DATA: l_node_text TYPE lvc_value,
        ls_preco    TYPE ty_preco,
        wl_monat    TYPE ty_preco-monat.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  ls_item_layout-t_image = icon_val_quantity_structure." '@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensified.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text = p_text." ps_preco-posnr.

  DATA: ls_node TYPE lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

*  ASSIGN <FS_LINE> TO <FS_PRECO>.
*
*
  MOVE-CORRESPONDING: ps_preco TO ls_preco.
  wl_monat = ls_preco-monat.
  CLEAR: ls_preco.
  ls_preco-monat = wl_monat.
  MOVE-CORRESPONDING: ls_preco TO <fs_preco>.
*  LS_PRECO-monat = LS_PRECO_Aux-monat.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = <fs_preco>
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_carrid_line
*&---------------------------------------------------------------------*
*&      Form  add_cmplete_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 3 to tree
*----------------------------------------------------------------------*
*      -->P_LS_PRECO  TY_PRECO
*      -->P_RELEATKEY   relatkey
*     <-->p_node_key    new node-key
*----------------------------------------------------------------------*
FORM add_complete_line USING   ps_preco "TYPE TY_PRECO
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi,
        ls_preco       TYPE ty_preco_n.
*  ls_item_layout-fieldname = 'FORMULA2'.
  ls_item_layout-class   = cl_gui_column_tree=>item_class_text.
*  ls_item_layout-editable = 'X'.
*  append ls_item_layout to lt_item_layout.

*  clear ls_item_layout.
*  ls_item_layout-fieldname = 'PLANETYPE'.
*  ls_item_layout-alignment = cl_gui_column_tree=>align_right.
*  append ls_item_layout to lt_item_layout.
  MOVE-CORRESPONDING: ps_preco TO ls_preco.
  l_node_text =  ls_preco-bezei.

  DATA: ls_node TYPE lvc_s_layn.
  IF ls_preco-tipo_calc EQ c_c
  OR ls_preco-tipo_calc EQ c_r.
    ls_node-n_image   = icon_sum. "SPACE.
  ELSEIF ls_preco-tipo_calc EQ c_f.
    ls_node-n_image   = icon_attachment.
  ELSE.
    ls_node-n_image   = icon_set_state.
  ENDIF.
  ls_node-exp_image = space.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      is_outtab_line   = ps_preco
      i_node_text      = l_node_text
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.
ENDFORM.                               " add_complete_line
*&---------------------------------------------------------------------*
*&      Form  add_carrid_line
*&---------------------------------------------------------------------*
*       add hierarchy-level 1 to tree
*----------------------------------------------------------------------*
*      -->P_LS_SFLIGHT  sflight
*      -->P_RELEATKEY   relatkey
*     <-->p_node_key    new node-key
*----------------------------------------------------------------------*
FORM add_header_line USING     ps_preco "TYPE TY_PRECO
                               p_relat_key TYPE lvc_nkey
                     CHANGING  p_node_key TYPE lvc_nkey.

  DATA: l_node_text TYPE lvc_value,
*        LS_PRECO_aux TYPE TY_PRECO_N,
        ls_preco    TYPE ty_preco.

* set item-layout
  DATA: lt_item_layout TYPE lvc_t_layi,
        ls_item_layout TYPE lvc_s_layi.
  ls_item_layout-t_image = icon_te_costs_assign. "'@3P@'.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  APPEND ls_item_layout TO lt_item_layout.

* add node
  l_node_text =  TEXT-t05."'Item Fixação'.

  DATA: ls_node TYPE lvc_s_layn.
  ls_node-n_image   = space.
  ls_node-exp_image = space.
*  MOVE-CORRESPONDING: PS_PRECO to LS_PRECO_Aux.
*  LS_PRECO-monat = LS_PRECO_Aux-monat.

  CALL METHOD tree1->add_node
    EXPORTING
      i_relat_node_key = p_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = ls_preco
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    IMPORTING
      e_new_node_key   = p_node_key.

ENDFORM.                               " add_header_line
*&---------------------------------------------------------------------*
*&      Form  PRECO_FRAME_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*----------------------------------------------------------------------*
FORM preco_frame_alv  USING    p_node_key.
  DATA: tl_filter    TYPE lvc_t_filt,
        wl_filter    TYPE lvc_s_filt,
        tl_children  TYPE lvc_t_nkey WITH HEADER LINE,
        wl_sort1(30) VALUE 'NIVEL',
        wl_sort2(30) VALUE 'POSNR'.

  FIELD-SYMBOLS: <fs_line_frame> TYPE any.
  UNASSIGN <fs_line_frame>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table_frame>.

  ASSIGN t_new_line->* TO <fs_line_frame>.

  CALL METHOD tree1->get_children
    EXPORTING
      i_node_key  = p_node_key
    IMPORTING
      et_children = tl_children[].

  READ TABLE tl_children INTO tl_children INDEX 1.

  CALL METHOD tree1->get_outtab_line
    EXPORTING
      i_node_key    = tl_children
    IMPORTING
      e_outtab_line = <fs_line_frame>.

*  READ TABLE tl_children INTO wl_children INDEX 1.
*
*  READ TABLE tg_preco_frame INTO tg_preco_frame INDEX wl_children-table_line.

  CLEAR: wg_valor.
*  PERFORM GET_SET_VALORES USING 'POSNR'
*                                'G'
*                        CHANGING WG_VALOR.
  ASSIGN COMPONENT 'POSNR' OF STRUCTURE <fs_line_frame> TO  <fs_campo>.
  wg_valor = <fs_campo>.
  CONDENSE wg_valor NO-GAPS.

  REFRESH: tl_filter.
  wl_filter-fieldname = 'POSNR'."c_dmbtr.
  wl_filter-sign      = 'I'. "c_i.
  wl_filter-option    = 'EQ'. "c_ne.
  wl_filter-low       = wg_valor. "TG_PRECO_FRAME-POSNR.

  APPEND wl_filter TO tl_filter.

  wl_filter-fieldname = 'INVISIBLE'."c_dmbtr.
  wl_filter-sign      = 'I'. "c_i.
  wl_filter-option    = 'EQ'. "c_ne.
  wl_filter-low       = space. "TG_PRECO_FRAME-POSNR.

  APPEND wl_filter TO tl_filter.

  " 21.02.2024 - RAMON ----------------------->
  wl_filter-fieldname = 'REDIST'."c_dmbtr.
  wl_filter-sign      = 'I'. "c_i.
  wl_filter-option    = 'EQ'. "c_ne.
  wl_filter-low       = space.

  APPEND wl_filter TO tl_filter.
  " 21.02.2024 - RAMON -----------------------<

  IF container_s2 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
*    WA_STABLE-ROW        = C_X.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = container_s2.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_column_width
      EXPORTING
        id    = 1
        width = 30.

    CALL METHOD splitter->set_column_sash
      EXPORTING
        id    = 1
        type  = splitter->type_movable
        value = splitter->true.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_delete_filter.
    APPEND wl_function TO tl_function.

    CREATE OBJECT grid8
      EXPORTING
        i_parent = container_s2.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid8.

* Register event handler
    SET HANDLER obg_toolbar->on_toolbar_p_fram FOR grid8.
    SET HANDLER obg_toolbar->handle_user_command_p_fram FOR grid8.

*    PERFORM MONTAR_LAYOUT USING 'PRECO_FRAME2'.
    REFRESH: t_fieldcatalog.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = wg_header-tp_venda
*      IMPORTING
*       e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

*    SORT  <FS_TABLE> BY (WL_SORT1) ASCENDING (WL_SORT2) ASCENDING.
    wa_layout-stylefname = 'STYLE'.
    wa_layout-info_fname =  'LINE_COLOR'.

    """ tabela de preço 16.02.2024 # aba preço #aba preco "alv aba preco
    CALL METHOD grid8->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

    CALL METHOD grid8->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid8->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid8->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid8,
*              lcl_event_handler=>on_hotspot_click for grid1,
              lcl_event_handler=>on_data_changed_finished_preco FOR grid8,
              lcl_event_handler=>on_data_changed_preco FOR grid8.
*              lcl_event_handler=>on_onf4 FOR grid1.
  ELSE.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_mb_filter.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_delete_filter.
    APPEND wl_function TO tl_function.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_column_width
      EXPORTING
        id    = 1
        width = 20.
*    CALL METHOD GRID8->SET_FILTER_CRITERIA
*      EXPORTING
*        IT_FILTER = TL_FILTER.

    REFRESH: t_fieldcatalog.
    CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
      EXPORTING
        i_tp_venda      = wg_header-tp_venda
*      IMPORTING
*       e_table         = t_new_table
      TABLES
        te_fieldcatalog = t_fieldcatalog.

    CALL METHOD grid8->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = <fs_table>.

    CALL METHOD grid8->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.                    " PRECO_FRAME_ALV
*&---------------------------------------------------------------------*
*&      Form  GET_SET_VALORES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0056_WAERS  text
*      -->P_3512   text
*----------------------------------------------------------------------*
FORM get_set_valores  USING   p_campo
                              p_get_set
                     CHANGING p_valor.


  ASSIGN COMPONENT p_campo  OF STRUCTURE <fs_line> TO <fs_campo>.

  IF sy-subrc EQ 0.

    IF p_get_set EQ 'S'.
      MOVE p_valor TO <fs_campo>.
    ELSEIF p_get_set EQ 'G'.
      MOVE <fs_campo> TO p_valor.
    ENDIF.

  ELSE.

    CLEAR p_valor.

  ENDIF.

ENDFORM.                    " GET_SET_VALORES
*&---------------------------------------------------------------------*
*&      Form  REDEFINE_OBJETOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM redefine_objetos .
  DATA: lt_events TYPE cntl_simple_events,
        l_event   TYPE cntl_simple_event.

  CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
    EXPORTING
      i_tp_venda      = wg_header-tp_venda
*      IMPORTING
*     e_table         = t_new_table
    TABLES
      te_fieldcatalog = t_fieldcatalog.

  wa_layout-stylefname = 'STYLE'.
  wa_layout-info_fname =  'LINE_COLOR'.

  CALL METHOD grid4->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = tl_function
      is_layout            = wa_layout
*     i_save               = 'X'
    CHANGING
      it_fieldcatalog      = t_fieldcatalog[]
      it_outtab            = <fs_table>.
*     create Hierarchy-header
  DATA l_hierarchy_header TYPE treev_hhdr.
  DATA: ls_variant TYPE disvariant.

  LOOP AT t_fieldcatalog INTO w_fieldcatalog.
    IF w_fieldcatalog-fieldname EQ 'BEZEI'.
      w_fieldcatalog-no_out = c_x.
    ENDIF.
    w_fieldcatalog-tabname = '<FS_TABLE_FRAME>'.
    MODIFY t_fieldcatalog FROM w_fieldcatalog.
  ENDLOOP.


  PERFORM build_hierarchy_header CHANGING l_hierarchy_header.
  ls_variant-report = sy-repid.

  CALL METHOD tree1->free.
  FREE: tree1.
* create tree control
  CREATE OBJECT tree1
    EXPORTING
      parent                      = container_s1
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'
      no_html_header              = 'X'
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.

* create emty tree-control
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = l_hierarchy_header
*     it_list_commentary  = lt_list_commentary
*     i_logo              = l_logo
*     i_background_id     = 'ALV_BACKGROUND'
      i_save              = 'A'
      is_variant          = ls_variant
    CHANGING
      it_outtab           = <fs_table_frame> "TG_PRECO_FRAME[] "table must be emty !!
      it_fieldcatalog     = t_fieldcatalog.

  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.

* define the events which will be passed to the backend
  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.
  CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.
  CLEAR l_event.
*    l_event-eventid = cl_gui_column_tree=>eventid_header_click.
*    APPEND l_event TO lt_events.
*    CLEAR l_event.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  l_event-appl_event = 'X'.
  APPEND l_event TO lt_events.
* register events
  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

* set handler for tree1
  SET HANDLER tree_event_receiver->handle_double_click       FOR tree1.
  SET HANDLER tree_event_receiver->handle_item_double_click  FOR tree1.
  SET HANDLER tree_event_receiver->handle_expand_no_children FOR tree1.
ENDFORM.                    " REDEFINE_OBJETOS
*&---------------------------------------------------------------------*
*&      Form  RETORNA_PRECO_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_3411   text
*      <--P_WG_VALOR  text
*      <--P_LOOP  text
*      <--P_AT  text
*      <--P_<FS_TABLE>  text
*      <--P_INTO  text
*      <--P_<FS_LINE>  text
*----------------------------------------------------------------------*
FORM retorna_preco_item  USING  p_preco_item
                                p_posnr
                         CHANGING p_valor.

  IF <fs_table> IS ASSIGNED.
    UNASSIGN <fs_line>.

    CREATE DATA t_new_line LIKE LINE OF <fs_table>.

    ASSIGN t_new_line->* TO <fs_line>.

    LOOP AT tg_preco_n
          WHERE preco EQ p_preco_item.

      LOOP AT <fs_table> INTO <fs_line>.
        IF p_posnr NE space.
          CLEAR: wg_valor.
          PERFORM get_set_valores USING 'POSNR'
                                        'G'
                               CHANGING wg_valor.
          IF wg_valor NE p_posnr.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'COD_FP'
                                      'G'
                             CHANGING wg_valor.

        IF wg_valor EQ tg_preco_n-cod_fp.
          CLEAR: wg_valor.
          PERFORM get_set_valores USING tg_preco_n-field
                                        'G'
                                 CHANGING wg_valor.

          TRANSLATE wg_valor USING '. '.
          TRANSLATE wg_valor USING ',.'.
          CONDENSE wg_valor NO-GAPS.

          p_valor = wg_valor.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " RETORNA_PRECO_ITEM
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ENQUEUE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_RC  text
*----------------------------------------------------------------------*
FORM verifica_enqueue  CHANGING p_rc.
  DATA: tl_enq     TYPE TABLE OF seqg3 WITH HEADER LINE,
        wl_num_enq TYPE sy-tabix,
        wl_arg     TYPE seqg3-garg.

  CONCATENATE sy-mandt wg_header-tp_venda INTO wl_arg.

  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gname  = 'ZSDT0057'
      garg   = wl_arg
      guname = space
    IMPORTING
      number = wl_num_enq
    TABLES
      enq    = tl_enq.
  IF wl_num_enq <> 0.
    READ TABLE tl_enq INDEX 1.
    MESSAGE e601(mc) WITH tl_enq-guname tl_enq-gname.

  ENDIF.

ENDFORM.                    " VERIFICA_ENQUEUE
*&---------------------------------------------------------------------*
*&      Form  ADD_ITEM_PRECO_FRAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_ITENS_POSNR  text
*----------------------------------------------------------------------*
FORM add_item_preco_frame USING    p_posnr.
  DATA: tl_preco_aux LIKE TABLE OF tg_preco WITH HEADER LINE,
        wl_posnr     TYPE zsdt0059-posnr,
        wl_init      TYPE c,
        wl_tabix     TYPE sy-tabix.

  DATA: p_2(13) TYPE p DECIMALS 2,
        p_5(13) TYPE p DECIMALS 5,
        p_4(13) TYPE p DECIMALS 4.

  FIELD-SYMBOLS: <fs_table_aux> TYPE STANDARD TABLE.

  CREATE DATA t_new_table LIKE  <fs_table>.
  ASSIGN t_new_table->* TO <fs_table_aux>.

  UNASSIGN <fs_line>.
  CREATE DATA t_new_line LIKE LINE OF <fs_table>.

  ASSIGN t_new_line->* TO <fs_line>.
  ASSIGN t_new_line->* TO <fs_line_aux>.

  <fs_table_aux>[] = <fs_table>[].

  READ TABLE <fs_table_aux> INTO <fs_line> INDEX 1.
  CLEAR: wg_valor.
  PERFORM get_set_valores USING 'POSNR'
                                'G'
                         CHANGING wg_valor.
  CONDENSE wg_valor NO-GAPS.

  LOOP AT <fs_table_aux> INTO <fs_line>.
    wl_tabix = sy-tabix.
    CLEAR: wg_valor_aux.
    PERFORM get_set_valores USING 'POSNR'
                                  'G'
                           CHANGING wl_posnr.
    CONDENSE wg_valor NO-GAPS.
    IF wg_valor NE wl_posnr.
      DELETE <fs_table_aux> INDEX wl_tabix.
    ENDIF.
  ENDLOOP.

  CALL METHOD tree1->delete_all_nodes.
  REFRESH: tg_preco, tg_preco_frame, <fs_table_frame>.

  LOOP AT tg_preco_n.
    CLEAR: wl_tabix.
    LOOP AT <fs_table> INTO <fs_line_aux>.
      CLEAR: wg_valor, wl_tabix.
      ASSIGN COMPONENT 'POSNR'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
      wg_valor = <fs_campo>.

      IF wg_valor EQ p_posnr.
        CLEAR: wg_valor.
        ASSIGN COMPONENT 'COD_FP'  OF STRUCTURE <fs_line_aux> TO <fs_campo>.
        wg_valor = <fs_campo>.
        IF wg_valor EQ tg_preco_n-cod_fp.
          wl_tabix = sy-tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: tg_preco_n-posnr, <fs_line>, tg_preco_n-cbot.
    MOVE-CORRESPONDING tg_preco_n TO <fs_line>.
    PERFORM get_set_valores USING 'POSNR'
                                  'S'
                            CHANGING p_posnr.

    IF tg_preco_n-tipo_calc EQ c_f.
      CONDENSE tg_preco_n-formula NO-GAPS.
      PERFORM get_set_valores USING tg_preco_n-field
                                     'S'
                            CHANGING tg_preco_n-formula.
    ELSE.
      CLEAR tg_preco_n-formula.
      PERFORM get_set_valores USING tg_preco_n-field
                                     'S'
                            CHANGING tg_preco_n-formula.
    ENDIF.
    IF wl_tabix IS INITIAL.
      APPEND <fs_line> TO <fs_table>.
    ELSEIF wl_tabix IS NOT INITIAL.
      MODIFY <fs_table> FROM <fs_line> INDEX wl_tabix.
    ENDIF.
  ENDLOOP.
  PERFORM create_hierarchy.
  CLEAR: tg_cond_esp.
  tg_fix_red-fixacao = tg_cond_esp-fixacao = p_posnr.
  APPEND tg_fix_red.
  APPEND tg_cond_esp.
  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.
ENDFORM.                    " ADD_ITEM_PRECO_FRAME
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL'
      OR 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      IF rb_email IS  INITIAL.
        CLEAR p_email.
      ENDIF.

      PERFORM gera_boleto.

*      CALL FUNCTION 'Z_SD_PRINT_BOLETO' "PSA
*        EXPORTING
*          doc_numero     = wl_boleto
*          tipo           = 'O'  "Ordem
*          hbkid          = wg_cond_pgt-hbkid "US 81799 - CBRAND
*          email          = p_email
*          "instrucoes     = wl_instrucoes "PSA
*        EXCEPTIONS
*          nao_localizado = 1
*          OTHERS         = 2.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  EXIBE_COND_ESP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM exibe_cond_esp .


  IF obg_dialogbox IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_layout-no_toolbar = c_x.
*    WA_STABLE-ROW        = C_X.

    CREATE OBJECT obg_dialogbox
      EXPORTING
        width   = 300
        height  = 200
        top     = 30
        left    = 30
        caption = TEXT-t06 "'Solicitação Ordem de Venda - Condições Especiais'
*       style   = cl_gui_control=>ws_sysmenu
        repid   = sy-repid
        dynnr   = sy-dynnr.

    CREATE OBJECT grid9
      EXPORTING
        i_parent = obg_dialogbox.

    wa_layout-stylefname = 'STYLE'.
    CALL METHOD grid9->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_cond_esp[].

    CALL METHOD grid9->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid9->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>changed_finished_cond_esp FOR grid9,
                 lcl_event_handler=>changed_cond_esp FOR grid9,
                 lcl_event_handler=>on_close         FOR obg_dialogbox.

  ENDIF.

*  DATA: WL_LAYOUT TYPE  SLIS_LAYOUT_ALV.
*  WL_LAYOUT-ZEBRA = C_X.
**  WL_LAYOUT-NO_ROWMARK = C_X.
**  WL_LAYOUT-EDIT = C_X.
*  WL_LAYOUT-EDIT_MODE = C_X.
*  WL_LAYOUT-WINDOW_TITLEBAR = 'Solicitação Ordem de Venda - Condições Especiais'.
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*         EXPORTING
*              I_CALLBACK_PROGRAM       = SY-REPID
*              I_CALLBACK_USER_COMMAND  = 'XUSER_COMMAND'
*              I_CALLBACK_PF_STATUS_SET = 'X_SET_PFSTATUS'
*              IS_LAYOUT               = WL_LAYOUT
*              IT_FIELDCAT             = ESTRUTURA[]
**            IT_SORT                 = T_SORT[]
*              I_SAVE                  = 'A'
*              I_SCREEN_START_COLUMN   = 3
*              I_SCREEN_START_LINE     = 3
*              I_SCREEN_END_COLUMN     = 60
*              I_SCREEN_END_LINE       = 13
*         TABLES
*              T_OUTTAB                = TG_COND_ESP.

ENDFORM.                    " EXIBE_COND_ESP
*&---------------------------------------------------------------------*
*&      Form  REMOVE_PRECO_FRAME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM remove_preco_frame .
  DATA: wl_tabix TYPE sy-tabix.

  LOOP AT tg_fix_red INTO tg_fix_red.

    READ TABLE tg_itens TRANSPORTING NO FIELDS WITH KEY fixacao = tg_fix_red-fixacao.

    IF ( sy-subrc IS NOT INITIAL ).

      LOOP AT <fs_table> INTO <fs_line>.

        wl_tabix = sy-tabix.
        PERFORM get_set_valores USING 'POSNR' 'G' wg_valor.
        IF ( wg_valor EQ tg_fix_red-fixacao ).
          DELETE <fs_table> INDEX wl_tabix.
        ENDIF.

      ENDLOOP.
    ENDIF.

    DELETE tg_cond_esp WHERE fixacao EQ tg_fix_red-fixacao.
    DELETE tg_fix_red.
    SUBTRACT 1 FROM wg_header-num_fixacao.

  ENDLOOP.

  CALL METHOD tree1->delete_all_nodes.
  REFRESH: tg_preco, tg_preco_frame, <fs_table_frame>.
  PERFORM create_hierarchy.

ENDFORM.                    " REMOVE_PRECO_FRAME
*&---------------------------------------------------------------------*
*&      Form  MONTA_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monta_log .

  CLEAR: wg_header_old, wg_cond_pgt_old.
  REFRESH: tg_itens_old, tg_pgt_ant_old, tg_logistica_old, tg_adto_ext_old, tg_instrucao_old, tg_form_lote_old.

  MOVE-CORRESPONDING: wg_header TO wg_header_old,
                      wg_cond_pgt TO wg_cond_pgt_old.

  tg_itens_old[]     = VALUE #( FOR ls_itens IN tg_itens[] ( CORRESPONDING #( ls_itens ) ) ).
  tg_pgt_ant_old[]   = VALUE #( FOR ls_pgt_ant IN tg_pgt_ant[] ( CORRESPONDING #( ls_pgt_ant ) ) ).
  tg_logistica_old[] = VALUE #( FOR ls_logistica IN tg_logistica[] ( CORRESPONDING #( ls_logistica ) ) ).
  tg_adto_ext_old[]  = VALUE #( FOR ls_adto_ext IN tg_adto_ext[] ( CORRESPONDING #( ls_adto_ext ) ) ).
  tg_instrucao_old[] = VALUE #( FOR ls_instrucao IN tg_instrucao[] ( CORRESPONDING #( ls_instrucao ) ) ).
  tg_form_lote_old[] = VALUE #( FOR ls_form_lote IN tg_form_lote[] ( CORRESPONDING #( ls_form_lote ) ) ).

ENDFORM.                    " MONTA_LOG
*&---------------------------------------------------------------------*
*&      Form  SAVE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM save_log .

  DATA: wl_tabix     TYPE sy-tabix.
  DATA: wl_tabix_aux TYPE sy-tabix.
  DATA: fieldname(30) TYPE c.

  DATA: _structure_0053 TYPE zsdt0053,
        _structure_0054 TYPE zsdt0054,
        _structure_0213 TYPE zsdt0213,
        _structure_0055 TYPE zsdt0055,
        _structure_0063 TYPE zsdt0063,
        _structure_0066 TYPE zsdt0066,
        _structure_0045 TYPE zeinstrucao,
        _structure_0073 TYPE zsdt0073.

  IF wg_acao EQ c_modif
  OR wg_acao EQ c_modif_qtd
  OR wg_acao EQ c_redist
  OR wg_acao EQ c_modif_c_ov.

*    BREAK-POINT.
    TRY .


*   // Header
        DATA(t_field) = zcl_solicitacao_ov=>get_fieldname_structure( wg_header ).
        LOOP AT t_field INTO DATA(_field).
          fieldname = |WG_HEADER-{ _field-name }|.
          PERFORM input_log USING fieldname 'header' space.
        ENDLOOP.

*   // Condição de Pagamento
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( wg_cond_pgt ).
        LOOP AT t_field INTO _field.
          fieldname = |WG_COND_PGT-{ _field-name }|.
          PERFORM input_log USING fieldname 'cond_pagamento' space.
        ENDLOOP.

*   // Itens
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0053 ).
        LOOP AT tg_itens.
          wl_tabix_aux = sy-tabix.

          CLEAR: tg_itens_old.
          READ TABLE tg_itens_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_ITENS-{ _field-name }|.
            PERFORM input_log USING fieldname 'itens' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Volume Por Lote
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0213 ).

        LOOP AT it_0213_aux INTO wa_0213_aux.
          CLEAR: tg_pgt_ant_old.
          wa_0213_aux = it_0213_aux_old[ sy-tabix ].

          LOOP AT t_field INTO _field.
            fieldname = |WA_0213_AUX-{ _field-name }|.
            PERFORM input_log USING fieldname 'pgto_antecipado' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Pagamento Antecipado
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0054 ).

        LOOP AT tg_pgt_ant.
          CLEAR: tg_pgt_ant_old.
          READ TABLE tg_pgt_ant_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_PGT_ANT-{ _field-name }|.
            PERFORM input_log USING fieldname 'pgto_antecipado' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Logistica
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0055 ).

        LOOP AT tg_logistica.
          CLEAR: tg_logistica_old.
          READ TABLE tg_logistica_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_LOGISTICA-{ _field-name }|.
            PERFORM input_log USING fieldname 'logistica' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Adiantemento Exterior
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0063 ).

        LOOP AT tg_adto_ext.
          CLEAR: tg_adto_ext_old.
          READ TABLE tg_adto_ext_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_ADTO_EXT-{ _field-name }|.
            PERFORM input_log USING fieldname 'adto.exterior' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Formação de Lote
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0066 ).

        LOOP AT tg_form_lote.
          CLEAR: tg_form_lote_old[].
          READ TABLE tg_form_lote_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_FORM_LOTE-{ _field-name }|.
            PERFORM input_log USING fieldname 'form.lote' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Instrução
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0045 ).

        LOOP AT tg_instrucao.
          CLEAR: tg_instrucao_old.
          READ TABLE tg_instrucao_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_INSTRUCAO-{ _field-name }|.
            PERFORM input_log USING fieldname 'instrução' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

*   // Condições especiais
        t_field = zcl_solicitacao_ov=>get_fieldname_structure( _structure_0073 ).

        LOOP AT tg_cond_esp.
          CLEAR: tg_cond_esp_old.
          READ TABLE tg_cond_esp_old INDEX sy-tabix.

          LOOP AT t_field INTO _field.
            fieldname = |TG_COND_ESP-{ _field-name }|.
            PERFORM input_log USING fieldname 'cond.especiais' wl_tabix_aux.
          ENDLOOP.

          wl_tabix = sy-tabix.
        ENDLOOP.

        LOOP AT tg_0059 WHERE tipo_calc EQ 'V'.
          CLEAR: tg_0059_old.
          READ TABLE tg_0059_old INDEX sy-tabix.
          PERFORM input_log USING 'TG_0059-NIVEL'        'preço' sy-tabix.
          PERFORM input_log USING 'TG_0059-POSNR'        'preço' sy-tabix.
          PERFORM input_log USING 'TG_0059-WAERS'        'preço' sy-tabix.
          PERFORM input_log USING 'TG_0059-CBOT'         'preço' sy-tabix.
          PERFORM input_log USING 'TG_0059-VALDT'        'preço' sy-tabix.
          PERFORM input_log USING 'TG_0059-MONAT'        'preço' sy-tabix.
          PERFORM input_log_preco USING 'TG_0059-FORMULA2' tg_0059-field     'preço' sy-tabix.
          wl_tabix = sy-tabix.

        ENDLOOP.

        MODIFY zsdt0083 FROM TABLE tg_log.
        COMMIT WORK.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

  ENDIF.

ENDFORM.                    " SAVE_LOG
*&---------------------------------------------------------------------*
*&      Form  INPUT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_6063   text
*----------------------------------------------------------------------*
FORM input_log  USING    VALUE(p_6063)
                         VALUE(p_6064)
                         wl_linha .
  DATA: wl_field(30),
        wl_field_old(40),
        wl_field_aux(40),
        wl_field_aux2(40).
  FIELD-SYMBOLS: <fs_field>     TYPE any,
                 <fs_field_old> TYPE any.

  UNASSIGN <fs_field>.
  UNASSIGN <fs_field_old>.

  wl_field = p_6063.
  SPLIT wl_field AT '-' INTO wl_field_aux
                             wl_field_aux2.
  CONCATENATE wl_field_aux '_OLD-' wl_field_aux2 INTO wl_field_old.
  ASSIGN (wl_field) TO <fs_field>.
  ASSIGN (wl_field_old) TO <fs_field_old>.
  IF <fs_field> IS ASSIGNED.
    IF <fs_field> NE <fs_field_old>.
      IF  tg_motivo-id_historico IS INITIAL.
        PERFORM get_next_number
                            USING
                               'ZHISTORIC'
                               '01'
                               c_x
                            CHANGING
                               tg_motivo-id_historico.
      ENDIF.
      SPLIT p_6063 AT '-' INTO wl_field
                               wl_field_aux.

      MOVE: wg_header-nro_sol_ov   TO tg_log-nro_sol_ov,
            wl_linha               TO tg_log-linha,
            tg_motivo-id_historico TO tg_log-id_historico,
            p_6064                 TO tg_log-area,
            wl_field_aux           TO tg_log-campo,
            <fs_field>             TO tg_log-new_value,
            <fs_field_old>         TO tg_log-old_value,
            sy-uname               TO tg_log-usnam,
            sy-datum               TO tg_log-data_atual,
            sy-uzeit               TO tg_log-hora_atual.
      APPEND tg_log.
      CLEAR: tg_log.

    ENDIF.
  ENDIF.

ENDFORM.                    " INPUT_LOG
*&---------------------------------------------------------------------*
*&      Form  INPUT_LOG_preco
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_6063   text
*----------------------------------------------------------------------*
FORM input_log_preco  USING    VALUE(p_6063)
                               VALUE(p_6064)
                               VALUE(p_6065)
                         wl_linha .
  DATA: wl_field(30),
        wl_field_old(40),
        wl_field_aux(40),
        wl_field_aux2(40).
  FIELD-SYMBOLS: <fs_field>     TYPE any,
                 <fs_field_old> TYPE any.

  UNASSIGN <fs_field>.
  UNASSIGN <fs_field_old>.

  wl_field = p_6063.
  SPLIT wl_field AT '-' INTO wl_field_aux
                             wl_field_aux2.
  CONCATENATE wl_field_aux '_OLD-' wl_field_aux2 INTO wl_field_old.
  ASSIGN (wl_field) TO <fs_field>.
  ASSIGN (wl_field_old) TO <fs_field_old>.
  IF <fs_field> IS ASSIGNED.
    IF <fs_field> NE <fs_field_old>.
      IF  tg_motivo-id_historico IS INITIAL.
        PERFORM get_next_number
                            USING
                               'ZHISTORIC'
                               '01'
                               c_x
                            CHANGING
                               tg_motivo-id_historico.
      ENDIF.
      SPLIT p_6063 AT '-' INTO wl_field
                               wl_field_aux.

      MOVE: wg_header-nro_sol_ov   TO tg_log-nro_sol_ov,
            wl_linha               TO tg_log-linha,
            tg_motivo-id_historico TO tg_log-id_historico,
            p_6065                 TO tg_log-area,
            p_6064                 TO tg_log-campo,
            <fs_field>             TO tg_log-new_value,
            <fs_field_old>         TO tg_log-old_value,
            sy-uname               TO tg_log-usnam,
            sy-datum               TO tg_log-data_atual,
            sy-uzeit               TO tg_log-hora_atual.
      APPEND tg_log.
      CLEAR: tg_log.

    ENDIF.
  ENDIF.

ENDFORM.                    " INPUT_LOG_preco

*&---------------------------------------------------------------------*
*&      Module  SAVE_DTVENDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE save_dtvenda INPUT.

  IF wg_acao EQ c_chg_dtv.
    UPDATE zsdt0051 SET data_venda = wg_header-data_venda
                    WHERE nro_sol_ov = wg_header-nro_sol_ov.

    IF sy-subrc IS INITIAL.
      MESSAGE s836(sd) WITH TEXT-m20.

      PERFORM input_log USING 'WG_HEADER-DATA_VENDA' 'header' space.
      MODIFY zsdt0083 FROM TABLE tg_log.
      COMMIT WORK.
    ENDIF.
    CLEAR: wg_acao.
    CALL FUNCTION 'DEQUEUE_EZSDT0051'
      EXPORTING
        nro_sol_ov = wg_header-nro_sol_ov.
  ENDIF.
ENDMODULE.                 " SAVE_DTVENDA  INPUT

*&---------------------------------------------------------------------*
*&      Form  F_PRECO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_preco USING p_tp_venda.
  REFRESH: t_fieldcatalog.
  CALL FUNCTION 'ZSDMF008_CRIA_TABELA_PRC_DINAM'
    EXPORTING
      i_tp_venda      = p_tp_venda
    IMPORTING
      e_table         = t_new_table
    TABLES
      te_fieldcatalog = t_fieldcatalog.

ENDFORM.                    "F_PRECO
*&---------------------------------------------------------------------*
*&      Form  f_bdc_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program
                       p_dynpro
                       p_start
                       p_fnam
                       p_fval.
  CLEAR it_dta.
  it_dta-program   = p_program.
  it_dta-dynpro    = p_dynpro.
  it_dta-dynbegin  = p_start.
  it_dta-fnam      = p_fnam.
  it_dta-fval      = p_fval.
  APPEND it_dta.
ENDFORM.                    "F_BDC_DATA

*&---------------------------------------------------------------------*
*&      Form  f_call_transaction
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*      -->P_MODE     text
*      -->P_UPD      text
*----------------------------------------------------------------------*
FORM f_call_transaction USING p_trans
                              p_mode
                              p_upd.

*  DATA: BEGIN OF tl_msg OCCURS 0,
*         msg TYPE t100-text,
*         fld TYPE bdcmsgcoll-fldname,
*        END OF tl_msg.

  REFRESH: it_msg, it_msgtext.

  CALL TRANSACTION p_trans USING it_dta
    MODE p_mode
    MESSAGES INTO it_msg
    UPDATE p_upd.

  IF it_msg[] IS NOT INITIAL.
    SELECT text
      FROM t100
      INTO TABLE it_msgtext
      FOR ALL ENTRIES IN it_msg
      WHERE arbgb = it_msg-msgid AND
            msgnr = it_msg-msgnr AND
            sprsl = sy-langu.

    LOOP AT it_msgtext.
      TRANSLATE it_msgtext-texto USING '& '.
      CONDENSE it_msgtext-texto.
      MODIFY it_msgtext.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "f_call_transaction

*&---------------------------------------------------------------------*
*&      Form  MONTAR_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(SPOS)       text
*      -->VALUE(FIELDNAME)  text
*      -->VALUE(UP)         text
*      -->VALUE(SUBTOT)     text
*----------------------------------------------------------------------*
FORM montar_ordem USING VALUE(p_spos)      TYPE lvc_s_sort-spos
                        VALUE(p_fieldname) TYPE lvc_s_sort-fieldname
                        VALUE(p_up)        TYPE lvc_s_sort-up
                        VALUE(p_subtot)    TYPE lvc_s_sort-subtot
                        VALUE(p_expa)      TYPE lvc_s_sort-expa.

  CLEAR wa_sort.
  wa_sort-spos      = p_spos.
  wa_sort-fieldname = p_fieldname.
  wa_sort-up        = p_up.
  wa_sort-subtot    = p_subtot.
  wa_sort-expa      = p_expa.
  APPEND wa_sort TO tg_sort.

ENDFORM.                    "MONTAR_SORT
*&---------------------------------------------------------------------*
*&      Form  LIMPAR_DATA_HEDGE_LIB
*&---------------------------------------------------------------------*
FORM limpar_data_hedge_lib USING p_direcao p_fixacao.
  CLEAR: wg_valor.

  " 03.05.2024 - Esse codigo foi desenvolvido pq após o
  " preenchimento da condição o fieldsymbol estava mantendo o valor da ultima
  " linha do grid na referencia, quando é feito um INTO ele sobregravava esse valor, então, limpamos
  " a referencia e trocamos os INTO para ASSIGNING para nao ter mais esse problema
  " RAMON -->
  UNASSIGN <fs_line>.
  " 03.05.2024 - RAMON --<

  IF p_direcao EQ 'G'.
    LOOP AT <fs_table> ASSIGNING <fs_line>.
      var_tabix = sy-tabix.
      PERFORM get_set_valores USING 'VALDT_HEDGE' 'G' CHANGING wg_valor.
      IF ( wg_valor NE '00000000' ).
        CLEAR: var_valor_char.
        var_valor_char = '00000000'.
        CONDENSE var_valor_char NO-GAPS.
        PERFORM get_set_valores USING 'VALDT_HEDGE' 'S' CHANGING var_valor_char.
        MODIFY <fs_table> FROM <fs_line> INDEX var_tabix.
      ENDIF.
    ENDLOOP.
    CLEAR: wg_verifica_incoterm.

  ELSEIF p_direcao EQ 'F'.

    LOOP AT <fs_table> ASSIGNING <fs_line>.
      var_tabix = sy-tabix.
      PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.
      IF p_fixacao EQ wg_valor.
        CLEAR: wg_valor.
        PERFORM get_set_valores USING 'VALDT_HEDGE' 'G' CHANGING wg_valor.
        IF ( wg_valor NE '00000000' ).
          CLEAR: var_valor_char.
          var_valor_char = '00000000'.
          CONDENSE var_valor_char NO-GAPS.
          PERFORM get_set_valores USING 'VALDT_HEDGE' 'S' CHANGING var_valor_char.
          MODIFY <fs_table> FROM <fs_line> INDEX var_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " LIMPAR_DATA_HEDGE_LIB
*&---------------------------------------------------------------------*
*&      Module  PBOO_0202  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pboo_0202 OUTPUT.
  SET PF-STATUS 'PS0202'.
  SET TITLEBAR  'TB0202'.

ENDMODULE.                 " PBOO_0202  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PAI_0202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0202 INPUT.
  CASE ok_code.
    WHEN: 'GRAVAR_CADASTRO'.
    WHEN OTHERS.
      LEAVE  TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " PAI_0202  INPUT
*&---------------------------------------------------------------------*
*&      Form  CADASTRO_CLIENTE
*&---------------------------------------------------------------------*
FORM cadastro_cliente CHANGING wl_code_cadastro.

  DATA: lw_zsdt0075 TYPE zsdt0075.
  DATA: lw_kna1 TYPE kna1,
        lw_t001 TYPE t001.

  MOVE:
       wg_header-vkorg TO wg_zsdt0075-vkorg,
       wg_header-kunnr TO wg_zsdt0075-kunnr,
       sy-datum        TO wg_zsdt0075-bdatu.

  SELECT SINGLE * FROM kna1 INTO lw_kna1 WHERE kunnr EQ wg_zsdt0075-kunnr.
  SELECT SINGLE * FROM t001 INTO lw_t001 WHERE bukrs EQ wg_zsdt0075-vkorg.

  txt_kunnr = lw_kna1-name1.
  txt_bukrs = lw_t001-butxt.

  CALL SCREEN 0202 ENDING AT 85 13 STARTING AT 3 3.

  IF ( sy-ucomm EQ 'SALVAR' ).
    WHILE wg_zsdt0075-bdatu < sy-datum.
      txt_warning = TEXT-m21.
      CALL SCREEN 0202 ENDING AT 70 13 STARTING AT 3 3.
      IF ( sy-ucomm EQ 'CANCELAR' ).
        wg_zsdt0075-bdatu = sy-datum.
      ENDIF.
    ENDWHILE.
  ENDIF.

  CASE sy-ucomm.
    WHEN: 'SALVAR'.

      SELECT SINGLE * FROM zsdt0075 INTO lw_zsdt0075 WHERE kunnr EQ wg_zsdt0075-kunnr
                                                       AND vkorg EQ wg_zsdt0075-vkorg
                                                       AND bdatu EQ sy-datum.
      IF ( sy-subrc NE 0 ).


        lw_zsdt0075-kunnr = wg_zsdt0075-kunnr.
        lw_zsdt0075-vkorg = wg_zsdt0075-vkorg.

        lw_zsdt0075-name1 = lw_kna1-name1.
        lw_zsdt0075-bdatu = wg_zsdt0075-bdatu.
        lw_zsdt0075-usnam = sy-uname.
        lw_zsdt0075-data  = sy-datum.
        lw_zsdt0075-hora  = sy-uzeit.

        INSERT INTO zsdt0075 VALUES lw_zsdt0075.

        COMMIT WORK.

        wl_code_cadastro = 'SALVAR'.
      ENDIF.
    WHEN OTHERS.
      PERFORM modifica_status USING 'R'
                        CHANGING sy-subrc.
      wl_code_cadastro = 'CANCELAR'.
  ENDCASE.

  CLEAR: lw_zsdt0075, lw_kna1,
          wg_zsdt0075-vkorg,
          wg_zsdt0075-kunnr,
          wg_zsdt0075-bdatu.

ENDFORM.                    " CADASTRO_CLIENTE
*&---------------------------------------------------------------------*
*&      Module  STATUS_0410  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0410 OUTPUT.
  SET PF-STATUS 'T_0410'.
  SET TITLEBAR 'S_0410'.

  PERFORM montar_layout USING 'POPUP'.

  IF wa_cont1 IS INITIAL.
    CREATE OBJECT wa_cont1
      EXPORTING
        container_name              = 'S_0410'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT wa_alv1
      EXPORTING
        i_parent          = wa_cont1
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CALL METHOD wa_alv1->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout1
        it_toolbar_excluding          = tl_function
      CHANGING
        it_outtab                     = it_zsdt0100
        it_fieldcatalog               = it_fcat1
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc NE 0 .
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER: lcl_event_handler=>on_hotspot_100 FOR wa_alv1.

  ELSE.
    CALL METHOD wa_alv1->refresh_table_display.
  ENDIF.

ENDMODULE.                 " STATUS_0410  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0410  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0410 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0410  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_VENDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_venda INPUT.

  PERFORM busca_venda.

ENDMODULE.                 " SEARCH_VENDA  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_VENDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_venda.
  TYPES: BEGIN OF ty_0057,
           tp_venda TYPE zsded012,
           bezei    TYPE bezei30,
           auart    TYPE auart,
         END OF ty_0057.

  DATA: it_0057   TYPE TABLE OF ty_0057,
        it_return TYPE TABLE OF ddshretval,
        wa_return LIKE LINE OF it_return.

  REFRESH it_0057.

  IF wg_acao EQ 'ADD'.
    SELECT tp_venda bezei auart
    FROM zsdt0057
     INTO TABLE it_0057
     WHERE status EQ 'X'.
  ELSE.
    SELECT tp_venda bezei auart
    FROM zsdt0057
     INTO TABLE it_0057.
  ENDIF.

  IF it_0057 IS NOT INITIAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'            "#EC *
      EXPORTING
        retfield        = 'TP_VENDA'
        value_org       = 'S'
      TABLES
        value_tab       = it_0057
        return_tab      = it_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2.

    IF sy-subrc = 0.
      READ TABLE it_return INTO wa_return
        INDEX 1.
      wg_header-tp_venda = wa_return-fieldval.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDIF.

ENDFORM.                    " BUSCA_VENDA
*&---------------------------------------------------------------------*
*&      Module  VALIDA_TP_VENDA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_tp_venda INPUT.

  DATA: wa_0057 TYPE zsdt0057.

  CLEAR: wa_0057.

  SELECT SINGLE *
   FROM zsdt0057
    INTO wa_0057
    WHERE tp_venda EQ wg_header-tp_venda.

  IF wa_0057 IS NOT INITIAL.
    IF wa_0057-status IS INITIAL.
      SHIFT wa_0057-tp_venda LEFT DELETING LEADING '0'.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-d16 wa_0057-tp_venda  wa_0057-bezei TEXT-d17.
      tp_venda_error = 'I'.
    ELSE.
      SELECT *
        FROM zsdt0235
         INTO TABLE it_0235
       WHERE tp_venda EQ wg_header-tp_venda.
    ENDIF.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-d16 wg_header-tp_venda TEXT-d18.
    tp_venda_error = 'X'.
  ENDIF.

ENDMODULE.                 " VALIDA_TP_VENDA  INPUT
*&---------------------------------------------------------------------*
*&      Form  update_bezei
*&---------------------------------------------------------------------*
FORM update_bezei TABLES p_table STRUCTURE tg_itens
                           c_bezei
                           p_bezei
                           t_bezei
                           n_bezei
                           b_bezei " 29.10.2024 - 147331 - RAMON
                     USING p_index TYPE any
                           p_valor_bezei TYPE any
                           p_col_temp TYPE c "<-- coluna temporaria **
                           p_redist TYPE flag. " 21.03.2025 - RAMON

  " ** Coluna temporaria: essa coluna é marcada com 'T', indicando que se houver modificação
  " na aba prod/qtd ela pode ser sobrescrevida.
  "  Para manter o funcionamento adequado foi deixada como parametro fixo 'T' nas chamadas
  " de atualização normal, e quando for mudança de lote ficou como vazia

  DATA: posicao            TYPE sy-tabix,
        wg_valor_c(30),
        wg_valor_p(30),
        wg_valor_t(30),
        wg_valor_n(30),
        wg_valor_b(30), " 29.10.2024 - 147331 - RAMON
        lv_cbot(30)        TYPE c,
        lv_cbot_redist(30) TYPE c,
        sair_c             TYPE c,
        sair_p             TYPE c,
        sair_t             TYPE c,
        sair_n             TYPE c,
        sair_b             TYPE c, " 29.10.2024 - 147331 - RAMON

        lv_monat_c         TYPE c LENGTH 2,
        lv_monat_p         TYPE c LENGTH 2,
        lv_monat_t         TYPE c LENGTH 2,
        lv_monat_n         TYPE c LENGTH 2,
        lv_monat_b         TYPE c LENGTH 2, " 29.10.2024 - 147331 - RAMON

        lv_safra_c         TYPE c LENGTH 4,
        lv_safra_p         TYPE c LENGTH 4,
        lv_safra_t         TYPE c LENGTH 4,
        lv_safra_n         TYPE c LENGTH 4,
        lv_safra_b         TYPE c LENGTH 4, " 29.10.2024 - 147331 - RAMON

        var_tp_row         TYPE c LENGTH 1,
        var_valor_aux      TYPE c LENGTH 30,
        lv_valor_n         TYPE c LENGTH 30.

  DATA: wl_itens LIKE LINE OF p_table.


  READ TABLE tg_itens INTO wl_itens INDEX p_index. "Busca a linha que esta sendo alterada.

  sair_c = sair_p = sair_t = sair_n = sair_b = space. " 29.10.2024 - 147331 - RAMON

  " 07.04.2025 - RAMON -->
*****  LOOP AT <fs_table> INTO <fs_line>.
*****
*****    posicao = sy-tabix.
*****
*****    CLEAR wg_valor.
*****
*****    PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor. " Busca a Fixacao para pegar pegar os valores do Frame de premio e chicado
*****
*****    IF wg_valor EQ wl_itens-fixacao.
*****
*****      CLEAR wg_valor. PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
*****
*****      CASE wg_valor.
*****        WHEN 'CHICAGO FRAME'.
*****          PERFORM get_set_valores USING 'PRECO' 'G' CHANGING wg_valor_c.
*****        WHEN 'PREMIO FRAME'.
*****          PERFORM get_set_valores USING 'PRECO' 'G' CHANGING wg_valor_p.
*****        WHEN 'TAXA CAMBIO FRAME'.
*****          PERFORM get_set_valores USING 'PRECO' 'G' CHANGING wg_valor_t.
*****          " 01.03.2024 - RAMON - BEZEI N -->
*****        WHEN 'NYFRAME'.
*****          PERFORM get_set_valores USING 'PRECO' 'G' CHANGING wg_valor_n.
*****          " 01.03.2024 - RAMON - BEZEI N --<
*****
*****          " 29.10.2024 - 147331 - RAMON -->
*****        WHEN 'BM&F FRAME'.
*****          PERFORM get_set_valores USING 'PRECO' 'G' CHANGING wg_valor_b.
*****          " 29.10.2024 - 147331 - RAMON --<
*****
*****      ENDCASE.
*****    ENDIF.
*****  ENDLOOP.

  " 07.04.2025 - RAMON - 166561 -->
  DATA lt_tab TYPE zsdc_preco_din_zsdt0062.

  PERFORM f_get_tabela_dinamica CHANGING lt_tab.

  LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<fs_preco>)
      WHERE posnr = wl_itens-fixacao
        AND ( bezei = gc_chicago_f
         OR bezei = gc_premio_f
         OR bezei = gc_taxa_f
         OR bezei = gc_ny_f
         OR bezei = gc_bmef_f ).

    CASE <fs_preco>-bezei.
      WHEN gc_chicago_f.
        wg_valor_c = <fs_preco>-preco.
      WHEN gc_premio_f.
        wg_valor_p = <fs_preco>-preco.
      WHEN gc_taxa_f.
        wg_valor_t = <fs_preco>-preco.
      WHEN gc_ny_f.
        wg_valor_n = <fs_preco>-preco.
      WHEN gc_bmef_f.
        wg_valor_b = <fs_preco>-preco.
    ENDCASE.
  ENDLOOP.

  " 07.04.2025 - RAMON - 166561 --<
  " 07.04.2025 - RAMON --<

  LOOP AT <fs_table> INTO <fs_line>.

    posicao = sy-tabix.

    CLEAR var_tp_row.
    CLEAR var_valor_char.
    CLEAR wg_valor.

    PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.

    IF wg_valor EQ wl_itens-fixacao.

      PERFORM get_set_valores USING 'VALDT' 'G' CHANGING var_valor_char.

      CLEAR wg_valor. PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

      " 14.05.2024 - 140755 - RAMON ----------->
      PERFORM get_set_valores USING 'TP_ROW' 'G' CHANGING var_tp_row.
      " 14.05.2024 - 140755 - RAMON -----------<

**** Adicionando na 59 para os Bezeis C1 ... C15 ##### INICIO #####
      IF wg_valor IN c_bezei.

        IF var_valor_char NE 0.
          PERFORM get_set_valores USING 'CBOT' 'G' CHANGING lv_cbot. " busca a ultima data preenchida para inclusão do CBOT
        ENDIF.

        PERFORM get_set_valores USING 'MONAT' 'G' CHANGING var_valor_char.

        " 19.02.2024 - RAMON - redistribuição -->>

        IF lv_monat_c IS INITIAL.
          lv_monat_c = var_valor_char.
        ENDIF.

        " 01.03.2024 - RAMON ->
        PERFORM get_set_valores USING 'SAFRA' 'G' CHANGING var_valor_aux.

        IF lv_safra_c IS INITIAL.
          lv_safra_c = var_valor_aux.
        ENDIF.

        " 01.03.2024 - RAMON -<

        IF wg_redistribuir = abap_true AND p_redist = abap_true.

          var_valor_char = '00'.

          IF lv_cbot_redist IS INITIAL AND lv_cbot IS NOT INITIAL.
            lv_cbot_redist = lv_cbot.
          ENDIF.


        ENDIF.
        " 19.02.2024 - RAMON - redistribuição --<<

        " 14.05.2024 - 140755 - RAMON ----------->
        IF var_valor_char EQ '00' OR var_tp_row =  'T'.
          "IF var_valor_char EQ '00'.

          " 14.05.2024 - 140755 - RAMON <------------
          CLEAR wg_valor.

          PERFORM get_set_valores USING 'POSNR1' 'G' CHANGING wg_valor.

          IF wg_valor EQ '000000' OR wl_itens-posnr EQ wg_valor.

            IF sair_c IS INITIAL .

              sair_c = 'X'.

              PERFORM get_set_valores USING 'CBOT'      'S' CHANGING lv_cbot.

              PERFORM get_set_valores USING 'QTDFIXADA' 'S' CHANGING p_valor_bezei.

              PERFORM get_set_valores USING 'POSNR1'    'S' CHANGING wl_itens-posnr.

              PERFORM get_set_valores USING 'PRECO'     'S' CHANGING wg_valor_c.

              PERFORM get_set_valores USING 'VALDT'     'S' CHANGING sy-datum.

              PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_c.

              PERFORM get_set_valores USING 'SAFRA' 'S' CHANGING lv_safra_c.

              " 14.02.2024 - 121095 - RBL -->
              IF wg_redistribuir = abap_true AND p_redist = abap_true.

                PERFORM f_change_bezei_redist USING 'C' wl_itens-fixacao.

                wg_valor = abap_true.
                PERFORM get_set_valores USING 'REDIST' 'S' CHANGING wg_valor.

                "wg_valor = wg_header-tp_venda. " nao da para fazer esse por causa da gravar
                "PERFORM get_set_valores USING 'COD_FP' 'S' CHANGING wg_valor.

                wg_valor  = wl_itens-fixacao.
                PERFORM get_set_valores USING 'POSNR' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'CBOT' 'S' CHANGING lv_cbot_redist.

                wg_valor = '999'.

                PERFORM get_set_valores USING 'ITEM_KEY' 'S' CHANGING wg_valor.

                "PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.
                "PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_c.

                PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING gv_ultimo_nvl. " 21.05.2024

                APPEND <fs_line> TO <fs_table>.

              ELSE.

                " 14.05.2024 - 140755 - RAMON - SE A AÇÃO É MODIFICAR QTDE, ENTÃO GRAVAMOS O CAMPO TP_ROW
                IF wg_acao = c_modif_qtd.
                  var_tp_row = p_col_temp.
                  PERFORM get_set_valores USING 'TP_ROW' 'S' CHANGING p_col_temp. "<-- T = temporario
                ENDIF.
                " 14.05.2024 - 140755 - RAMON --<

                MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              ENDIF.

              "MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              " 14.02.2024 - 121095 - RBL --<

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.
**** Adicionando na 59 para os Bezeis C1 ... C15 ##### FIM #####


**** Adicionando na 59 para os Bezeis P1 ... P15 #### INICIO ####
      IF wg_valor IN p_bezei.

        IF var_valor_char NE 0 OR var_tp_row =  'T'." <-- 14.05.2024 - 140755 - RAMON
          PERFORM get_set_valores USING 'CBOT' 'G' CHANGING lv_cbot.
        ENDIF.

        PERFORM get_set_valores USING 'MONAT' 'G' CHANGING var_valor_char.

        " 19.02.2024 - RAMON - redistribuição -->>

        IF lv_monat_p IS INITIAL.
          lv_monat_p = var_valor_char.
        ENDIF.

        " 01.03.2024 - RAMON ->
        PERFORM get_set_valores USING 'SAFRA' 'G' CHANGING var_valor_aux.

        IF lv_safra_p IS INITIAL.
          lv_safra_p = var_valor_aux.
        ENDIF.

        " 01.03.2024 - RAMON -<

        IF wg_redistribuir = abap_true AND p_redist = abap_true.

          var_valor_char = '00'.

          IF lv_cbot_redist IS INITIAL AND lv_cbot IS NOT INITIAL.
            lv_cbot_redist = lv_cbot.
          ENDIF.

        ENDIF.
        " 19.02.2024 - RAMON - redistribuição --<<

        IF var_valor_char EQ '00' OR var_tp_row =  'T'. " <-- 14.05.2024 - 140755 - RAMON
          CLEAR wg_valor. PERFORM get_set_valores USING 'POSNR1' 'G' CHANGING wg_valor.
          IF wg_valor EQ '000000' OR wl_itens-posnr EQ wg_valor.
            IF sair_p IS INITIAL .

              sair_p = 'X'.
              PERFORM get_set_valores USING 'CBOT'      'S' CHANGING lv_cbot.
              PERFORM get_set_valores USING 'QTDFIXADA' 'S' CHANGING p_valor_bezei.
              PERFORM get_set_valores USING 'POSNR1'    'S' CHANGING wl_itens-posnr.
              PERFORM get_set_valores USING 'PRECO'     'S' CHANGING wg_valor_p.
              PERFORM get_set_valores USING 'VALDT'     'S' CHANGING sy-datum.
              PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_p.
              PERFORM get_set_valores USING 'SAFRA' 'S' CHANGING lv_safra_p.

              " 14.02.2024 - 121095 - RBL -->
              IF wg_redistribuir = abap_true AND p_redist = abap_true.

                PERFORM f_change_bezei_redist USING 'P' wl_itens-fixacao.

                wg_valor = abap_true.
                PERFORM get_set_valores USING 'REDIST' 'S' CHANGING wg_valor.

                "wg_valor = wg_header-tp_venda. " nao da para fazer esse por causa da gravar
                "PERFORM get_set_valores USING 'COD_FP' 'S' CHANGING wg_valor.

                wg_valor  = wl_itens-fixacao.
                PERFORM get_set_valores USING 'POSNR' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'CBOT' 'S' CHANGING lv_cbot_redist.

                wg_valor = '999'.

                PERFORM get_set_valores USING 'ITEM_KEY' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING gv_ultimo_nvl.  " 21.05.2024

                "PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_p.

                APPEND <fs_line> TO <fs_table>.

              ELSE.

                " 14.05.2024 - 140755 - RAMON - SE A AÇÃO É MODIFICAR QTDE, ENTÃO GRAVAMOS O CAMPO TP_ROW
                IF wg_acao = c_modif_qtd.
                  var_tp_row = p_col_temp.
                  PERFORM get_set_valores USING 'TP_ROW' 'S' CHANGING p_col_temp. "<-- T = temporario
                ENDIF.
                " 14.05.2024 - 140755 - RAMON --<

                MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              ENDIF.

              "MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              " 14.02.2024 - 121095 - RBL --<

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.
**** Adicionando na 59 para os Bezeis P1 ... P15 #### FIM ####

      IF wg_valor IN t_bezei.

        IF var_valor_char NE 0 OR var_tp_row =  'T'. " <-- 14.05.2024 - 140755 - RAMON
          PERFORM get_set_valores USING 'CBOT' 'G' CHANGING lv_cbot.
        ENDIF.

        PERFORM get_set_valores USING 'MONAT' 'G' CHANGING var_valor_char.

        " 19.02.2024 - RAMON - redistribuição -->>

        IF lv_monat_t IS INITIAL.
          lv_monat_t = var_valor_char.
        ENDIF.

        " 01.03.2024 - RAMON ->
        PERFORM get_set_valores USING 'SAFRA' 'G' CHANGING var_valor_aux.

        IF lv_safra_t IS INITIAL.
          lv_safra_t = var_valor_aux.
        ENDIF.

        " 01.03.2024 - RAMON -<

        IF wg_redistribuir = abap_true AND p_redist = abap_true.

          var_valor_char = '00'.

          IF lv_cbot_redist IS INITIAL AND lv_cbot IS NOT INITIAL.
            lv_cbot_redist = lv_cbot.
          ENDIF.

        ENDIF.
        " 19.02.2024 - RAMON - redistribuição --<<

        IF var_valor_char EQ '00' OR var_tp_row =  'T'. " <-- 14.05.2024 - 140755 - RAMON
          CLEAR wg_valor. PERFORM get_set_valores USING 'POSNR1' 'G' CHANGING wg_valor.
          IF wg_valor EQ '000000' OR wl_itens-posnr EQ wg_valor.
            IF sair_t IS INITIAL .

              sair_t = 'X'.
              PERFORM get_set_valores USING 'CBOT'      'S' CHANGING lv_cbot.
              PERFORM get_set_valores USING 'QTDFIXADA' 'S' CHANGING p_valor_bezei.
              PERFORM get_set_valores USING 'POSNR1'    'S' CHANGING wl_itens-posnr.
              PERFORM get_set_valores USING 'PRECO'     'S' CHANGING wg_valor_t.
              PERFORM get_set_valores USING 'VALDT'     'S' CHANGING sy-datum.
              PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_t.
              PERFORM get_set_valores USING 'SAFRA' 'S' CHANGING lv_safra_t.

              " 14.02.2024 - 121095 - RBL -->
              IF wg_redistribuir = abap_true AND p_redist = abap_true.

                PERFORM f_change_bezei_redist USING 'T' wl_itens-fixacao.

                wg_valor = abap_true.
                PERFORM get_set_valores USING 'REDIST' 'S' CHANGING wg_valor.

                "wg_valor = wg_header-tp_venda. " nao da para fazer esse por causa da gravar
                "PERFORM get_set_valores USING 'COD_FP' 'S' CHANGING wg_valor.

                wg_valor  = wl_itens-fixacao.
                PERFORM get_set_valores USING 'POSNR' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'CBOT' 'S' CHANGING lv_cbot_redist.

                wg_valor = '999'.

                PERFORM get_set_valores USING 'ITEM_KEY' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING gv_ultimo_nvl. " 21.05.2024

                "PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_t.

                APPEND <fs_line> TO <fs_table>.

              ELSE.

                " 14.05.2024 - 140755 - RAMON - SE A AÇÃO É MODIFICAR QTDE, ENTÃO GRAVAMOS O CAMPO TP_ROW
                IF wg_acao = c_modif_qtd.
                  var_tp_row = p_col_temp.
                  PERFORM get_set_valores USING 'TP_ROW' 'S' CHANGING p_col_temp. "<-- T = temporario
                ENDIF.
                " 14.05.2024 - 140755 - RAMON --<

                MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              ENDIF.

              "MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              " 14.02.2024 - 121095 - RBL --<

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      " 29.02.2024 - 135234 - RAMON	-->

      IF wg_valor IN n_bezei.

        IF var_valor_char NE 0 .
          PERFORM get_set_valores USING 'CBOT' 'G' CHANGING lv_cbot.
        ENDIF.

        PERFORM get_set_valores USING 'MONAT' 'G' CHANGING var_valor_char.

        " 19.02.2024 - RAMON - redistribuição -->>

        IF lv_monat_n IS INITIAL.
          lv_monat_n = var_valor_char.
        ENDIF.
        " 19.02.2024 - RAMON - redistribuição --<<

        " 01.03.2024 - RAMON ->
        PERFORM get_set_valores USING 'SAFRA' 'G' CHANGING var_valor_aux.

        IF lv_safra_n IS INITIAL.
          lv_safra_n = var_valor_aux.
        ENDIF.

        " 01.03.2024 - RAMON -<

        IF wg_redistribuir = abap_true AND p_redist = abap_true.

          var_valor_char = '00'.

          IF lv_cbot_redist IS INITIAL AND lv_cbot IS NOT INITIAL.
            lv_cbot_redist = lv_cbot.
          ENDIF.

        ENDIF.
        " 19.02.2024 - RAMON - redistribuição --<<

        IF var_valor_char EQ '00' OR var_tp_row =  'T'. " <-- 14.05.2024 - 140755 - RAMON

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'POSNR1' 'G' CHANGING wg_valor.


          IF wg_valor EQ '000000' OR wl_itens-posnr EQ wg_valor.

            IF sair_n IS INITIAL .

              sair_n = 'X'.
              PERFORM get_set_valores USING 'CBOT'      'S' CHANGING lv_cbot.
              PERFORM get_set_valores USING 'QTDFIXADA' 'S' CHANGING p_valor_bezei.
              PERFORM get_set_valores USING 'POSNR1'    'S' CHANGING wl_itens-posnr.
              PERFORM get_set_valores USING 'PRECO'     'S' CHANGING wg_valor_n.
              PERFORM get_set_valores USING 'VALDT'       'S' CHANGING sy-datum.
              PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_n.
              PERFORM get_set_valores USING 'SAFRA' 'S' CHANGING lv_safra_n.

              " 14.02.2024 - 121095 - RBL -->
              IF wg_redistribuir = abap_true AND p_redist = abap_true.

                PERFORM f_change_bezei_redist USING 'N' wl_itens-fixacao.

                wg_valor = abap_true.
                PERFORM get_set_valores USING 'REDIST' 'S' CHANGING wg_valor.

                "wg_valor = wg_header-tp_venda. " nao da para fazer esse por causa da gravar
                "PERFORM get_set_valores USING 'COD_FP' 'S' CHANGING wg_valor.

                wg_valor  = wl_itens-fixacao.
                PERFORM get_set_valores USING 'POSNR' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'CBOT' 'S' CHANGING lv_cbot_redist.

                wg_valor = '999'.

                PERFORM get_set_valores USING 'ITEM_KEY' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING gv_ultimo_nvl.  " 21.05.2024

                "PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_n.

                APPEND <fs_line> TO <fs_table>.

              ELSE.

                " 14.05.2024 - 140755 - RAMON - SE A AÇÃO É MODIFICAR QTDE, ENTÃO GRAVAMOS O CAMPO TP_ROW
                IF wg_acao = c_modif_qtd.
                  var_tp_row = p_col_temp.
                  PERFORM get_set_valores USING 'TP_ROW' 'S' CHANGING p_col_temp. "<-- T = temporario
                ENDIF.
                " 14.05.2024 - 140755 - RAMON --<

                MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              ENDIF.

              "MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              " 14.02.2024 - 121095 - RBL --<

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      " 29.02.2024 - 135234 - RAMON	--<

      " 29.10.2024 - 147331 - RAMON -->
**** Adicionando na 59 para os Bezeis B1 ... B15 ##### INICIO #####
      IF wg_valor IN b_bezei.

        IF var_valor_char NE 0.
          PERFORM get_set_valores USING 'CBOT' 'G' CHANGING lv_cbot. " busca a ultima data preenchida para inclusão do CBOT
        ENDIF.

        PERFORM get_set_valores USING 'MONAT' 'G' CHANGING var_valor_char.

        IF lv_monat_b IS INITIAL.
          lv_monat_b = var_valor_char.
        ENDIF.

        PERFORM get_set_valores USING 'SAFRA' 'G' CHANGING var_valor_aux.

        IF lv_safra_b IS INITIAL.
          lv_safra_b = var_valor_aux.
        ENDIF.

        IF wg_redistribuir = abap_true AND p_redist = abap_true.

          var_valor_char = '00'.

          IF lv_cbot_redist IS INITIAL AND lv_cbot IS NOT INITIAL.
            lv_cbot_redist = lv_cbot.
          ENDIF.


        ENDIF.

        IF var_valor_char EQ '00' OR var_tp_row =  'T'.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'POSNR1' 'G' CHANGING wg_valor.

          IF wg_valor EQ '000000' OR wl_itens-posnr EQ wg_valor.

            IF sair_b IS INITIAL .

              sair_b = 'X'.

              PERFORM get_set_valores USING 'CBOT'      'S' CHANGING lv_cbot.

              PERFORM get_set_valores USING 'QTDFIXADA' 'S' CHANGING p_valor_bezei.

              PERFORM get_set_valores USING 'POSNR1' 'S' CHANGING wl_itens-posnr.

              PERFORM get_set_valores USING 'PRECO' 'S' CHANGING wg_valor_b.

              PERFORM get_set_valores USING 'VALDT' 'S' CHANGING sy-datum.

              PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_b.

              PERFORM get_set_valores USING 'SAFRA' 'S' CHANGING lv_safra_b.

              IF wg_redistribuir = abap_true AND p_redist = abap_true.

                PERFORM f_change_bezei_redist USING 'B' wl_itens-fixacao.

                wg_valor = abap_true.
                PERFORM get_set_valores USING 'REDIST' 'S' CHANGING wg_valor.

                "wg_valor = wg_header-tp_venda. " nao da para fazer esse por causa da gravar
                "PERFORM get_set_valores USING 'COD_FP' 'S' CHANGING wg_valor.

                wg_valor  = wl_itens-fixacao.
                PERFORM get_set_valores USING 'POSNR' 'S' CHANGING wg_valor.

                PERFORM get_set_valores USING 'CBOT' 'S' CHANGING lv_cbot_redist.

                wg_valor = '999'.

                PERFORM get_set_valores USING 'ITEM_KEY' 'S' CHANGING wg_valor.

                "PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.
                "PERFORM get_set_valores USING 'MONAT' 'S' CHANGING lv_monat_B.

                PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING gv_ultimo_nvl. " 21.05.2024

                APPEND <fs_line> TO <fs_table>.

              ELSE.

                IF wg_acao = c_modif_qtd.
                  var_tp_row = p_col_temp.
                  PERFORM get_set_valores USING 'TP_ROW' 'S' CHANGING p_col_temp. "<-- T = temporario
                ENDIF.


                MODIFY <fs_table> FROM <fs_line> INDEX posicao.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.
**** Adicionando na 59 para os Bezeis B1 ... B15 ##### FIM #####
      " 29.10.2024 - 147331 - RAMON --<


    ENDIF.
  ENDLOOP.

ENDFORM.                    " UPDATE_C_P
*&---------------------------------------------------------------------*
*&      Form  update_bezei
*&---------------------------------------------------------------------*
FORM update_bezei_2 TABLES p_table STRUCTURE tg_itens
                           c_bezei
                           p_bezei
                           t_bezei
                           n_bezei
                           b_bezei
                     USING p_index TYPE any
                           p_valor_bezei TYPE any
                           p_col_temp TYPE c
                           p_redist TYPE flag.

  DATA wl_itens LIKE LINE OF p_table.

  READ TABLE tg_itens INTO wl_itens INDEX p_index.

  CHECK sy-subrc EQ 0.

  DATA lt_tab TYPE zsdc_preco_din_zsdt0062.

  PERFORM f_get_tabela_dinamica CHANGING lt_tab.

  " -------- bezei C
  PERFORM f_preenche_bezei_preco
    USING 'C'
          p_redist
          p_col_temp
          p_valor_bezei
          wl_itens
          gc_chicago_f
          c_bezei[]
 CHANGING lt_tab.

  " -------- bezei P
  PERFORM f_preenche_bezei_preco
    USING 'P'
          p_redist
          p_col_temp
          p_valor_bezei
          wl_itens
          gc_premio_f
          p_bezei[]
 CHANGING lt_tab.

  " -------- bezei T
  PERFORM f_preenche_bezei_preco
    USING 'T'
          p_redist
          p_col_temp
          p_valor_bezei
          wl_itens
          gc_taxa_f
          t_bezei[]
 CHANGING lt_tab.

  " -------- bezei N
  PERFORM f_preenche_bezei_preco
    USING 'N'
          p_redist
          p_col_temp
          p_valor_bezei
          wl_itens
          gc_ny_f
          n_bezei[]
 CHANGING lt_tab.

  " -------- bezei B
  PERFORM f_preenche_bezei_preco
    USING 'B'
          p_redist
          p_col_temp
          p_valor_bezei
          wl_itens
          gc_bmef_f
          b_bezei[]
 CHANGING lt_tab.

  PERFORM f_set_tabela_dinamica USING lt_tab.

ENDFORM.                    " UPDATE_C_P
*&---------------------------------------------------------------------*
*&      Form  BUSCA_VALORES_59
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_valores_59 USING fixacao.
  DATA: posicao TYPE sy-tabix.

  LOOP AT <fs_table> INTO <fs_line>.
    CLEAR wg_valor.

    PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor. " Busca a Fixacao para pegar pegar os valores do Frame de premio e chicado
    IF wg_valor EQ fixacao.

      CLEAR wg_valor. PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.
      CASE wg_valor.
        WHEN 'C1'.
          PERFORM get_set_valores USING 'MONAT' 'G' CHANGING g_monat_c.  "Guarda o Valor de Monat para inserir quando for salvar.
        WHEN 'P1'.
          PERFORM get_set_valores USING 'MONAT' 'G' CHANGING g_monat_p. "Guarda o Valor de Monat para inserir quando for salvar.
      ENDCASE.

    ENDIF.
  ENDLOOP.

  LOOP AT <fs_table> INTO <fs_line>.
    posicao = sy-tabix.

    PERFORM get_set_valores USING 'POSNR' 'G' CHANGING wg_valor.
    IF wg_valor EQ fixacao.
      CLEAR wg_valor.PERFORM get_set_valores USING 'VALDT' 'G' CHANGING wg_valor.
      IF wg_valor NE '00000000'.
        CLEAR wg_valor.PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.
        IF wg_valor EQ '00'.

          CLEAR wg_valor.PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

          IF wg_valor IN c_bezei.
            CLEAR wg_valor.PERFORM get_set_valores USING 'MONAT' 'S' CHANGING g_monat_c.
            MODIFY <fs_table> FROM <fs_line> INDEX posicao.
          ELSEIF wg_valor IN p_bezei.
            CLEAR wg_valor.PERFORM get_set_valores USING 'MONAT' 'S' CHANGING g_monat_p.
            MODIFY <fs_table> FROM <fs_line> INDEX posicao.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.



ENDFORM.                    " BUSCA_VALORES_59
*&---------------------------------------------------------------------*
*&      Form  BUSCA_BEZEI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_bezei_56 .

  IF gt_zsdt0056[] IS INITIAL.

    SELECT * FROM zsdt0056
      INTO TABLE gt_zsdt0056.

    "REFRESH: r_bezei, p_bezei, c_bezei, b_bezei. " 29.10.2024 - 147331 - RAMON -->
    "CLEAR: r_bezei_line, p_bezei_line, c_bezei_line, b_bezei." 29.10.2024 - 147331 - RAMON -->


  ENDIF.

  CHECK r_bezei[] IS INITIAL OR
        p_bezei[] IS INITIAL OR
        c_bezei[] IS INITIAL OR
        " 29.10.2024 - 147331 - RAMON -->
        b_bezei[] IS INITIAL.
  " 29.10.2024 - 147331 - RAMON --<

  LOOP AT gt_zsdt0056 INTO gw_zsdt0056.

    var_len1  = strlen( gw_zsdt0056-bezei ).

    CASE var_len1.
      WHEN: '2' OR '3'.
        IF ( gw_zsdt0056-bezei(1) EQ 'T' ).
          CLEAR: r_bezei_line.

          r_bezei_line-sign   =  'I'.
          r_bezei_line-option = 'EQ'.
          r_bezei_line-low    = gw_zsdt0056-bezei.
          r_bezei_line-high   = gw_zsdt0056-bezei.
          APPEND r_bezei_line TO r_bezei.

        ENDIF.
        IF ( gw_zsdt0056-bezei(1) EQ 'C' ).
          CLEAR: r_bezei_line.

          c_bezei_line-sign   =  'I'.
          c_bezei_line-option = 'EQ'.
          c_bezei_line-low    = gw_zsdt0056-bezei.
          c_bezei_line-high   = gw_zsdt0056-bezei.
          APPEND c_bezei_line TO c_bezei.

        ENDIF.
        IF ( gw_zsdt0056-bezei(1) EQ 'P' ).
          CLEAR: r_bezei_line.

          p_bezei_line-sign   =  'I'.
          p_bezei_line-option = 'EQ'.
          p_bezei_line-low    = gw_zsdt0056-bezei.
          p_bezei_line-high   = gw_zsdt0056-bezei.
          APPEND p_bezei_line TO p_bezei.

        ENDIF.

        " 16.01.2024 - RAMON BEZEI 'N' -->
        IF ( gw_zsdt0056-bezei(1) EQ 'N' ).
          CLEAR: n_bezei_line.

          n_bezei_line-sign   =  'I'.
          n_bezei_line-option = 'EQ'.
          n_bezei_line-low    = gw_zsdt0056-bezei.
          n_bezei_line-high   = gw_zsdt0056-bezei.
          APPEND n_bezei_line TO n_bezei.

        ENDIF.
        " 16.01.2024 - RAMON BEZEI 'N' --<

        " 29.10.2024 - 147331 - RAMON -->
        IF ( gw_zsdt0056-bezei(1) EQ 'B' ).
          CLEAR: b_bezei_line.

          b_bezei_line-sign   =  'I'.
          b_bezei_line-option = 'EQ'.
          b_bezei_line-low    = gw_zsdt0056-bezei.
          b_bezei_line-high   = gw_zsdt0056-bezei.
          APPEND b_bezei_line TO b_bezei.

        ENDIF.
        " 29.10.2024 - 147331 - RAMON --<

      WHEN OTHERS.
        CLEAR: gw_zsdt0056, var_len1.
        CONTINUE.
    ENDCASE.
    CLEAR: gw_zsdt0056, var_len1.
  ENDLOOP.

ENDFORM.                    " BUSCA_BEZEI
*&---------------------------------------------------------------------*
*&      Form  INPUT_MONAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM input_monat .
  DATA: posicao TYPE sy-tabix.

  CLEAR: g_monat_c, g_monat_p, g_monat_t.

  " 03.05.2024 - Esse codigo foi desenvolvido pq após o
  " preenchimento da condição o fieldsymbol estava mantendo o valor da ultima
  " linha do grid na referencia, quando é feito um INTO ele sobregravava esse valor, então, limpamos
  " a referencia e trocamos os INTO para ASSIGNING para nao ter mais esse problema
  " RAMON -->
  UNASSIGN <fs_line>.
  " 03.05.2024 - RAMON --<

  LOOP AT <fs_table> ASSIGNING <fs_line>.

    posicao = sy-tabix.

    CLEAR wg_valor.

    PERFORM get_set_valores USING 'VALDT' 'G' CHANGING wg_valor.

    IF wg_valor NE '00000000'.

      CLEAR wg_valor.

      PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

      CASE wg_valor.
        WHEN 'C1'.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.

          MOVE wg_valor TO g_monat_c.

        WHEN 'P1'.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.

          MOVE wg_valor TO g_monat_p.

          " 12.12.2023 - 128467 - RBL -->
        WHEN 'T1'.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.

          MOVE wg_valor TO g_monat_t.

          " 12.12.2023 - 128467 - RBL --<

          " 01.03.2023 - 128467 - RBL -->
        WHEN 'N1'.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.

          MOVE wg_valor TO g_monat_n.

          " 01.03.2023 - 128467 - RBL --<

      ENDCASE.

      CLEAR wg_valor.

      PERFORM get_set_valores USING 'MONAT' 'G' CHANGING wg_valor.

      IF wg_valor EQ '00'.

        CLEAR wg_valor.

        PERFORM get_set_valores USING 'BEZEI' 'G' CHANGING wg_valor.

        IF wg_valor IN c_bezei.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'S' CHANGING g_monat_c.

          MODIFY <fs_table> FROM <fs_line> INDEX posicao.

        ELSEIF wg_valor IN p_bezei.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'S' CHANGING g_monat_p.

          MODIFY <fs_table> FROM <fs_line> INDEX posicao.

          " 12.12.2023 - 128467 - RBL -->

        ELSEIF wg_valor IN r_bezei.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'S' CHANGING g_monat_t.

          MODIFY <fs_table> FROM <fs_line> INDEX posicao.

          " 12.12.2023 - 128467 - RBL --<

          " 01.03.2024 - 128467 - RBL -->

        ELSEIF wg_valor IN n_bezei.

          CLEAR wg_valor.

          PERFORM get_set_valores USING 'MONAT' 'S' CHANGING g_monat_n.

          MODIFY <fs_table> FROM <fs_line> INDEX posicao.

          " 01.03.2024 - 128467 - RBL --<

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " INPUT_MONAT
*&---------------------------------------------------------------------*
*&      Form  LIBERA_OV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM libera_ov .

  DATA: it_lfa1  TYPE STANDARD TABLE OF lfa1,
        it_lfb1  TYPE STANDARD TABLE OF lfb1,
        it_lfm1  TYPE STANDARD TABLE OF lfm1,
        wa_lfa1  TYPE lfa1,
        vl_check TYPE char1.

  "Adicionar o perform aqui para verificar a data de vencimento < que a data de hoje.
  CASE wg_header-tp_venda.
    WHEN '12'.
    WHEN OTHERS.
      PERFORM: verifica_erros.
  ENDCASE.


  IF NOT ( tg_msg_ret[] IS INITIAL ).
    MESSAGE e836(sd) WITH TEXT-m14
                     wg_header-nro_sol_ov
                     TEXT-m25.
    LEAVE TO SCREEN 100.

  ELSE.

*  Inicio Bloqueio cliente com restrições CS2017001253 WB

    SELECT SINGLE * FROM kna1 INTO @DATA(b_kna1) WHERE kunnr EQ @wg_header-kunnr.

    CASE abap_true.
      WHEN b_kna1-sperr OR "  Bloqueio Geral p/ todas empresas
           b_kna1-aufsd OR "  Bloqueio de ordem centralizado para cliente
           b_kna1-lifsd OR "  Bloqueio de remessa centralizado para cliente
           b_kna1-faksd OR "  Bloqueio centralizado de faturamento para cliente
           b_kna1-cassd.   "  Bloqueio de contatos central para cliente
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-ee3. EXIT.
    ENDCASE.

    SELECT SINGLE *
      FROM knb1
        INTO @DATA(b_knb1)
          WHERE kunnr EQ @wg_header-kunnr
            AND bukrs EQ @wg_header-vkorg.

*  Bloqueio Especifico para uma empresas
    IF     NOT b_knb1-sperr IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-ee1. EXIT.
    ENDIF.

* Verifica se existe fornecedor bloqueado associado ao Cliente

*    IF NOT B_KNA1-STKZN IS INITIAL.
*      SELECT SINGLE *
*        FROM LFA1
*        INTO @DATA(B_LFA1)
*        WHERE STCD2 EQ @B_KNA1-STCD2
*          AND STCD3 EQ @B_KNA1-STCD3.
*    ELSE.
*      SELECT SINGLE *
*        FROM LFA1
*        INTO B_LFA1
*        WHERE STCD1 EQ B_KNA1-STCD1
*          AND STCD3 EQ B_KNA1-STCD3.
*    ENDIF.
*
*    CASE ABAP_TRUE.
*      WHEN B_LFA1-SPERR OR
*           B_LFA1-SPERM.
*        MESSAGE S836(SD) DISPLAY LIKE 'E' WITH text-EE2. EXIT.
*      WHEN OTHERS.
*        IF B_LFA1-SPERQ IS NOT INITIAL.
*          MESSAGE S836(SD) DISPLAY LIKE 'E' WITH text-EE2. EXIT.
*        ENDIF.
*    ENDCASE.

*****************************************************************************************
* Verifica se existe fornecedor bloqueado associado ao Cliente - LG CS2017001713 INÍCIO *
*****************************************************************************************

    CLEAR: it_lfa1, vl_check.

    IF NOT b_kna1-stkzn IS INITIAL.

      SELECT *
        FROM lfa1
        INTO TABLE it_lfa1
        WHERE stcd2 EQ b_kna1-stcd2
          AND stcd3 EQ b_kna1-stcd3.

    ELSE.

      SELECT *
        FROM lfa1 INTO TABLE it_lfa1
        WHERE stcd1 EQ b_kna1-stcd1
          AND stcd3 EQ b_kna1-stcd3.

    ENDIF.

    IF it_lfa1 IS NOT INITIAL.

      SELECT *
        FROM lfm1
        INTO TABLE it_lfm1
        FOR ALL ENTRIES IN it_lfa1
        WHERE lifnr EQ it_lfa1-lifnr.

      SELECT *
        FROM lfb1
        INTO TABLE it_lfb1
        FOR ALL ENTRIES IN it_lfa1
        WHERE lifnr EQ it_lfa1-lifnr.

      SORT it_lfa1 BY sperr sperm sperq ASCENDING.
      DELETE it_lfa1 WHERE sperr EQ abap_true
                        OR sperm EQ abap_true
                        OR sperq NE space.

      IF it_lfa1 IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-ee2.
        EXIT.
      ELSE.

        LOOP AT it_lfa1 INTO wa_lfa1.

          READ TABLE it_lfm1 WITH KEY lifnr = wa_lfa1-lifnr
                                      sperm = abap_true TRANSPORTING NO FIELDS.
          IF sy-subrc IS NOT INITIAL.
            READ TABLE it_lfb1 WITH KEY lifnr = wa_lfa1-lifnr
                                        sperr = abap_true TRANSPORTING NO FIELDS.
            IF sy-subrc IS NOT INITIAL.
              vl_check = abap_true.
            ENDIF.
          ENDIF.

        ENDLOOP.

        IF vl_check IS INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-ee2.
          EXIT.
        ENDIF.

      ENDIF.

    ENDIF.

**************************************************************************************
* Verifica se existe fornecedor bloqueado associado ao Cliente - LG CS2017001713 FIM *
**************************************************************************************


*  Fim Bloqueio cliente com restrições CS2017001253 WB

    SELECT SINGLE * FROM zsdt0057 INTO wl_0057 WHERE tp_venda EQ wg_header-tp_venda.

    CASE wl_0057-boleto.
      WHEN: 'X'.

        wl_code_cadastro = 'SALVAR'.

        SELECT SINGLE *
          FROM zsdt0075
          INTO wl_zsdt0075
           WHERE kunnr EQ wg_header-kunnr
             AND vkorg EQ wg_header-vkorg
             AND bdatu GE sy-datum.

        IF ( sy-subrc NE 0 ).

          IF wg_cond_pgt-pgto_ant EQ c_n.

            txt_warning = TEXT-m22."'Pag. Atencipado não pode ser sem boleto OU form. de pagt. não pode ser diferente de [D]'.
            PERFORM: cadastro_cliente CHANGING wl_code_cadastro.

          ELSEIF ( wg_cond_pgt-pgto_ant IS INITIAL ) AND ( wg_cond_pgt-zlsch NE c_d ).

            txt_warning = TEXT-m23."'Form. de pagt. não pode ser diferente de [D - Boleto] '.
            PERFORM: cadastro_cliente CHANGING wl_code_cadastro.

          ENDIF.
        ENDIF.
    ENDCASE.

    IF ( wl_code_cadastro NE 'CANCELAR' ).

      PERFORM modifica_status USING 'L'
                              CHANGING sy-subrc.
      IF sy-subrc IS INITIAL.
        wg_acao = c_atual.
        MESSAGE s836(sd) WITH TEXT-m14
                         wg_header-nro_sol_ov
                         TEXT-m24.
        LEAVE TO SCREEN 100.
      ENDIF.

    ENDIF.
  ENDIF.

ENDFORM.                    " LIBERA_OV
*&      Module  TELAS_PBO  OUTPUT
MODULE telas_pbo OUTPUT.
  DATA: o_tela TYPE REF TO lcl_busca_tela.
  CREATE OBJECT o_tela.
  o_tela->busca_tela( ).
ENDMODULE.                 " TELAS_PBO  OUTPUT
*&      Module  TELAS_PAI  INPUT
MODULE telas_pai INPUT.
  CREATE OBJECT o_tela.
  o_tela->busca_tela( ).
ENDMODULE.                 " TELAS_PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  TRATAR_DISPARO_FRETE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM tratar_disparo_frete .


  CHECK NOT tl_itens_frete IS INITIAL.

  DATA: tl_itens_frete_final TYPE TABLE OF ty_itens_frete,
        tl_itens_frete_tmp   TYPE TABLE OF ty_itens_frete,
        tl_itens_frete_aux   TYPE TABLE OF ty_itens_frete,
        wl_itens_frete_aux   TYPE ty_itens_frete.
  DATA: qtd_lines TYPE sy-tabix.

  FIELD-SYMBOLS: <frete>     TYPE ty_itens_frete,
                 <excluir>   TYPE ty_itens_frete,
                 <frete_aux> TYPE ty_itens_frete.

  DATA: tabix_fre TYPE sy-tabix.

  SORT tl_itens_frete BY fixacao seq.
  MOVE tl_itens_frete TO tl_itens_frete_aux.
  DELETE ADJACENT DUPLICATES FROM tl_itens_frete     COMPARING ALL FIELDS.
  DELETE ADJACENT DUPLICATES FROM tl_itens_frete_aux COMPARING fixacao.

  LOOP AT tl_itens_frete_aux ASSIGNING <frete_aux>.

    CASE <frete_aux>-direcao.
      WHEN 'E'.

        MOVE tl_itens_frete TO tl_itens_frete_tmp.
        DELETE tl_itens_frete_tmp WHERE fixacao NE <frete_aux>-fixacao.

        qtd_lines = lines( tl_itens_frete_tmp ).
        LOOP AT tl_itens_frete_tmp ASSIGNING <frete>.
          CASE sy-tabix.
            WHEN '1' OR qtd_lines.
            WHEN OTHERS.
              <frete>-excluir = abap_true.
          ENDCASE.
          APPEND <frete> TO tl_itens_frete_final.
        ENDLOOP.

      WHEN 'L'.

        MOVE tl_itens_frete TO tl_itens_frete_tmp.
        DELETE tl_itens_frete_tmp WHERE fixacao NE <frete_aux>-fixacao.

        LOOP AT tl_itens_frete_tmp ASSIGNING <frete> WHERE fixacao EQ <frete_aux>-fixacao.
          tabix_fre = sy-tabix.
          ADD 1 TO tabix_fre.

          CASE <frete>-direcao.
            WHEN 'L'.
              READ TABLE tl_itens_frete_tmp ASSIGNING <excluir> INDEX tabix_fre.
              IF sy-subrc IS INITIAL AND <excluir>-direcao EQ 'E' AND <frete_aux>-fixacao EQ <excluir>-fixacao.
                <frete>-excluir   = abap_true.
                <excluir>-excluir = abap_true.
              ENDIF.
              READ TABLE tl_itens_frete_tmp ASSIGNING <excluir> INDEX tabix_fre.
              IF sy-subrc IS INITIAL AND <excluir>-direcao EQ 'L' AND <frete_aux>-fixacao EQ <excluir>-fixacao.
                <frete>-excluir   = abap_true.
              ENDIF.
          ENDCASE.
          APPEND <frete> TO tl_itens_frete_final.
        ENDLOOP.

    ENDCASE.
  ENDLOOP.

  DELETE tl_itens_frete_final WHERE excluir EQ abap_true.
  DELETE ADJACENT DUPLICATES FROM tl_itens_frete_final COMPARING nro_ov fixacao direcao.
  MOVE tl_itens_frete_final TO tl_itens_frete.

ENDFORM.                    " TRATAR_DISPARO_FRETE

*&---------------------------------------------------------------------*
*&      Form  CALLBACK_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RECORD_TAB   text
*      -->SHLP         text
*      -->CALLCONTROL  text
*----------------------------------------------------------------------*
FORM callback_f4 TABLES record_tab STRUCTURE seahlpres
                     CHANGING shlp TYPE shlp_descr
                       callcontrol LIKE ddshf4ctrl.
  DATA:
    ls_intf LIKE LINE OF shlp-interface,
    ls_prop LIKE LINE OF shlp-fieldprop.

  CLEAR: ls_prop-shlpselpos,
         ls_prop-shlplispos.

  REFRESH: shlp-interface.
  ls_intf-shlpfield = 'F0001'.
  ls_intf-valfield  = 'LIFNR'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
  ls_intf-shlpfield = 'F0002'.
  ls_intf-valfield  = 'ORT01'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
  ls_intf-shlpfield = 'F0003'.
  ls_intf-valfield  = 'LGORT'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
  ls_intf-shlpfield = 'F0004'.
  ls_intf-valfield  = 'LGOBE'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.

ENDFORM.                    "callback_f4

*&---------------------------------------------------------------------*
*&      Form  CALLBACK_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->RECORD_TAB   text
*      -->SHLP         text
*      -->CALLCONTROL  text
*----------------------------------------------------------------------*
FORM callback_f4_ins TABLES record_tab STRUCTURE seahlpres
                     CHANGING shlp TYPE shlp_descr
                       callcontrol LIKE ddshf4ctrl.
  DATA:
    ls_intf LIKE LINE OF shlp-interface,
    ls_prop LIKE LINE OF shlp-fieldprop.

  CLEAR: ls_prop-shlpselpos,
         ls_prop-shlplispos.

  REFRESH: shlp-interface.

  shlp-interface = VALUE #(
    ( shlpfield = 'F0001'  valfield  = 'INSTRUCAO' f4field   = abap_true )
    ( shlpfield = 'F0002'  valfield  = 'TERMINAL'  f4field   = abap_true )
    ( shlpfield = 'F0003'  valfield  = 'CHARG'     f4field   = abap_true )
    ( shlpfield = 'F0004'  valfield  = 'MATNR'     f4field   = abap_true )
    ( shlpfield = 'F0005'  valfield  = 'VOLEH'     f4field   = abap_true )
   ).

ENDFORM.                    "callback_f4

*&---------------------------------------------------------------------*
*&      Module  PAI_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0300 INPUT.

  DATA : it_texto TYPE TABLE OF string.

  CASE sy-ucomm.
    WHEN 'OK'.

      CALL METHOD obj_custom_editor->get_text_as_stream
        IMPORTING
          text = gt_editor.

      obj_frete->check_email( EXPORTING i_table = gt_editor
                              IMPORTING e_table = gt_email ).

      obj_frete->envio_frete( EXPORTING t_0045 = tg_ins_frete_aux
                                        email  = gt_email
                                       ).
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-m28.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " PAI_0300  INPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0300 OUTPUT.


  SET PF-STATUS 'T_0300'.
  SET TITLEBAR 'S_0300'.

  FREE: gt_editor.

  IF NOT obj_custom_txt IS INITIAL AND NOT obj_custom_editor IS INITIAL.
    CALL METHOD obj_custom_txt->free( ).
    CALL METHOD obj_custom_editor->free( ).
  ENDIF.

  CREATE OBJECT: obj_custom_txt    EXPORTING container_name = 'C_EMAIL',
                 obj_custom_editor EXPORTING
                                              wordwrap_mode     = 1
                                              wordwrap_position = 76
                                              max_number_chars  = 200
                                              parent         = obj_custom_txt.

  CALL METHOD obj_custom_editor->set_toolbar_mode( toolbar_mode = obj_custom_editor->false ).
  CALL METHOD obj_custom_editor->set_statusbar_mode( statusbar_mode = obj_custom_editor->false ).
  CALL METHOD obj_custom_editor->set_readonly_mode( readonly_mode = obj_custom_editor->false ).

ENDMODULE.                 " PBO_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  F4_NRO_SOL_OV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_nro_sol_ov INPUT.

  TYPES: BEGIN OF f4,
           nro_sol_ov TYPE zsded013,
           name1      TYPE name1_gp,
           mcod3      TYPE mcdd3,
           tp_venda   TYPE zsded012,
           vkorg      TYPE vkorg,
           data_atual TYPE rrseldate,
         END OF f4.

  DATA: f4_nro_sol_ov TYPE TABLE OF f4.

  SELECT a~nro_sol_ov k~name1 k~mcod3 a~tp_venda a~vkorg a~data_atual
    INTO TABLE f4_nro_sol_ov
    FROM zsdt0051 AS a
    INNER JOIN kna1 AS k ON k~kunnr EQ a~kunnr
    WHERE status NE 'D'.

  SORT f4_nro_sol_ov BY nro_sol_ov DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NRO_SOL_OV'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-NRO_SOL_OV'
      value_org       = 'S'
    TABLES
      value_tab       = f4_nro_sol_ov
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.

MODULE f4_id_contrato INPUT.

  DATA: it_fieldtab TYPE TABLE OF dfies,
        wa_fieldtab TYPE dfies.

  TYPES: BEGIN OF ff4,
           id_contrato            TYPE zsdt0143-id_contrato,
           id_contrato_referencia TYPE zsdt0143-id_contrato_referencia,
           contrato               TYPE zsdt0143-contrato,
           contrato_cliente       TYPE zsdt0143-contrato_cliente,
           empresa                TYPE zsdt0143-empresa,
           safra                  TYPE zsdt0143-safra,
           cliente                TYPE zsdt0143-cliente,
           tp_venda               TYPE zsdt0143-tp_venda,
           dt_venda               TYPE zsdt0143-dt_venda,
         END OF ff4.

  DATA: f4_id_contrato TYPE TABLE OF zsdt0143.

  CHECK wg_header-param_espec = c_a OR
        wg_header-param_espec = c_x OR
        wg_header-param_espec = c_z.

  FREE: it_fieldtab.
  PERFORM get_info_field USING: 'ZSDT0143' 'ID_CONTRATO' wa_fieldtab.
  wa_fieldtab-position  = 1.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'ID_CONTRATO_REFERENCIA' wa_fieldtab.
  wa_fieldtab-position  = 2.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CONTRATO' wa_fieldtab.
  wa_fieldtab-position  = 3.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CONTRATO_CLIENTE' wa_fieldtab.
  wa_fieldtab-position  = 4.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'EMPRESA'     wa_fieldtab.
  wa_fieldtab-position  = 5.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'SAFRA'       wa_fieldtab.
  wa_fieldtab-position  = 6.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'CLIENTE'     wa_fieldtab.
  wa_fieldtab-position  = 7.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'TP_VENDA'    wa_fieldtab.
  wa_fieldtab-position  = 8.
  APPEND wa_fieldtab TO it_fieldtab.

  PERFORM get_info_field USING: 'ZSDT0143' 'DT_VENDA'    wa_fieldtab.
  wa_fieldtab-position  = 9.
  APPEND wa_fieldtab TO it_fieldtab.

  SELECT *
    INTO TABLE f4_id_contrato
    FROM zsdt0143
    WHERE cancelado <> abap_true.

  SORT f4_id_contrato BY dt_venda DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_CONTRATO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-ID_CONTRATO'
      value_org       = 'S'
    TABLES
      value_tab       = f4_id_contrato
      field_tab       = it_fieldtab
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  GET_INFO_FIELD
*&---------------------------------------------------------------------*
FORM get_info_field USING p_tabname   TYPE ddobjname
                          p_fieldname TYPE dfies-fieldname
                          p_info      TYPE dfies.

  DATA: dfies_tab TYPE STANDARD TABLE OF dfies.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = p_tabname
      fieldname = p_fieldname
    TABLES
      dfies_tab = dfies_tab.

  READ TABLE dfies_tab INTO p_info INDEX 1.

  CASE p_fieldname.

    WHEN 'ID_CONTRATO'.
      p_info-reptext   = 'Id. Contrato'.
      p_info-scrtext_s = 'Id. Contrato'.
      p_info-scrtext_m = 'Id. Contrato'.
      p_info-scrtext_l = 'Id. Contrato'.

    WHEN 'ID_CONTRATO_REFERENCIA'.
      p_info-reptext   = 'Id. Contrato Refer'.
      p_info-scrtext_s = 'Id. Contrato Refer'.
      p_info-scrtext_m = 'Id. Contrato Refer'.
      p_info-scrtext_l = 'Id. Contrato Refer'.
      p_info-outputlen = 20.

    WHEN 'CONTRATO'.
      p_info-reptext   = 'Contrato'.
      p_info-scrtext_s = 'Contrato'.
      p_info-scrtext_m = 'Contrato'.
      p_info-scrtext_l = 'Contrato'.

    WHEN 'CONTRATO_CLIENTE'.
      p_info-reptext   = 'Contrato Cliente'.
      p_info-scrtext_s = 'Contrato Cliente'.
      p_info-scrtext_m = 'Contrato Cliente'.
      p_info-scrtext_l = 'Contrato Cliente'.

    WHEN 'EMPRESA'.
      p_info-reptext   = 'Empresa'.
      p_info-scrtext_s = 'Empresa'.
      p_info-scrtext_m = 'Empresa'.
      p_info-scrtext_l = 'Empresa'.

    WHEN 'SAFRA'.
      p_info-reptext   = 'Safra'.
      p_info-scrtext_s = 'Safra'.
      p_info-scrtext_m = 'Safra'.
      p_info-scrtext_l = 'Safra'.

    WHEN 'CLIENTE'.
      p_info-reptext   = 'Cliente'.
      p_info-scrtext_s = 'Cliente'.
      p_info-scrtext_m = 'Cliente'.
      p_info-scrtext_l = 'Cliente'.

    WHEN 'TP_VENDA'.
      p_info-reptext   = 'Tp.Venda'.
      p_info-scrtext_s = 'Tp.Venda'.
      p_info-scrtext_m = 'Tp.Venda'.
      p_info-scrtext_l = 'Tp.Venda'.

    WHEN 'DT_VENDA'.
      p_info-reptext   = 'Dt.Venda'.
      p_info-scrtext_s = 'Dt.Venda'.
      p_info-scrtext_m = 'Dt.Venda'.
      p_info-scrtext_l = 'Dt.Venda'.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_CBOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_GOOD_ROW_ID  text
*----------------------------------------------------------------------*
FORM atualiza_cbot  USING    p_row_id.

  TYPES: BEGIN OF ty_59,
           nro_sol_ov TYPE zsdt0059-nro_sol_ov,
           cbot       TYPE zsdt0059-cbot,
           nivel      TYPE zsdt0059-nivel,
           posnr      TYPE zsdt0059-posnr,
           cod_fp     TYPE zsdt0059-cod_fp,
           field      TYPE zsdt0059-field,
         END OF ty_59.

  DATA wa_59 TYPE ty_59.

  CLEAR wa_59.

  READ TABLE <fs_table> ASSIGNING <fs_line> INDEX p_row_id.

  PERFORM get_set_valores USING 'CBOT'   'G' CHANGING wa_59-cbot.
*  PERFORM GET_SET_VALORES USING 'NIVEL'  'G' CHANGING WA_59-NIVEL.
  PERFORM get_set_valores USING 'POSNR'  'G' CHANGING wa_59-posnr.
  PERFORM get_set_valores USING 'COD_FP' 'G' CHANGING wa_59-cod_fp.
*  PERFORM GET_SET_VALORES USING 'FIELD'  'G' CHANGING WA_59-FIELD.

  wa_59-nro_sol_ov = wg_header-nro_sol_ov.

  UPDATE zsdt0059 SET cbot = wa_59-cbot
         WHERE nro_sol_ov EQ wa_59-nro_sol_ov
*           AND NIVEL      EQ WA_59-NIVEL
           AND posnr      EQ wa_59-posnr
           AND cod_fp     EQ wa_59-cod_fp.
*           AND FIELD      EQ WA_59-FIELD.

  IF sy-subrc IS INITIAL.

    PERFORM add_59.

    LOOP AT tg_0059 WHERE tipo_calc EQ 'V'.

      CLEAR: tg_0059_old.
      READ TABLE tg_0059_old INDEX sy-tabix.
      PERFORM input_log USING 'TG_0059-CBOT' 'Preço' space.

    ENDLOOP.

    MODIFY zsdt0083 FROM TABLE tg_log.
    COMMIT WORK.
  ENDIF.

  FREE tg_0059.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_59
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_59 .

  DATA: tl_input_0059 TYPE TABLE OF zsdt0059 WITH HEADER LINE,
        wl_valor(60).

  UNASSIGN <fs_line>.
  LOOP AT <fs_table> ASSIGNING <fs_line>.
    CLEAR: wg_valor.
    PERFORM get_set_valores USING 'COD_FP'
                                  'G'
                         CHANGING wg_valor.
    CONDENSE wg_valor NO-GAPS.
    LOOP AT tg_preco_n
      WHERE cod_fp EQ wg_valor.

      MOVE-CORRESPONDING: tg_preco_n TO tl_input_0059.

      MOVE: tg_preco_n-formula    TO tl_input_0059-formula,
            tg_preco_n-ocbot      TO tl_input_0059-ocbot,
            tg_preco_n-preco      TO tl_input_0059-preco,
            tg_preco_n-c_decimais TO tl_input_0059-c_decimais,
            wg_header-nro_sol_ov  TO tl_input_0059-nro_sol_ov,
            sy-uname              TO tl_input_0059-usnam,
            sy-datum              TO tl_input_0059-data_atual,
            sy-uzeit              TO tl_input_0059-hora_atual.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING tg_preco_n-field
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.
      tl_input_0059-formula2 = wl_valor.

      TRANSLATE tl_input_0059-formula2 USING '. '.
      TRANSLATE tl_input_0059-formula2 USING ',.'.
      CONDENSE tl_input_0059-formula2  NO-GAPS.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'WAERS'
                                    'G'
                           CHANGING  wl_valor.
      CONDENSE wl_valor NO-GAPS.
      tl_input_0059-waers = wl_valor.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'INVISIBLE'
                                    'G'
                           CHANGING  wl_valor.
      CONDENSE wl_valor NO-GAPS.
      tl_input_0059-invisible = wl_valor.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'CBOT'
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.

      tl_input_0059-cbot = wl_valor.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'MONAT'
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.

      tl_input_0059-monat = wl_valor.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'VALDT'
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.

      tl_input_0059-valdt = wl_valor.

      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.
      tl_input_0059-posnr = wl_valor.


      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'POSNR1'
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.
      tl_input_0059-posnr1 = wl_valor.


      CLEAR: wl_valor.
      PERFORM get_set_valores USING 'VALDT_HEDGE'
                                    'G'
                           CHANGING wl_valor.
      CONDENSE wl_valor NO-GAPS.
      tl_input_0059-valdt_hedge = wl_valor.

      APPEND tl_input_0059.
      CLEAR:tl_input_0059.
    ENDLOOP.
  ENDLOOP.

  tg_0059[] = tl_input_0059[].

  FREE tl_input_0059.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CL_GUI_ALV_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM cl_gui_alv_grid USING p_dir.

  FREE: tl_function.
  CLEAR wl_function.

  wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND wl_function TO tl_function.
  wl_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND wl_function TO tl_function.

  CASE p_dir.
    WHEN 'INSTRUCAO' OR 'FORM_LOTE' OR 'PEDIDO'.
    WHEN OTHERS.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND wl_function TO tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND wl_function TO tl_function.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CHECK_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_contrato INPUT.

*-CS2022000332-#78223-02.06.2022-JT-inicio
  CHECK wg_header-param_espec = c_a AND
        wg_header-param_espec = c_x AND
        wg_header-param_espec = c_z .
*-CS2022000332-#78223-02.06.2022-JT-inicio

  DATA: contrato TYPE zsdt0045-contrato,
        soma     TYPE zsdt0045-quantidade.

  CLEAR: wg_header-vkorg       ,
         wg_header-vkgrp       ,
         wg_header-data_venda  ,
         wg_header-correto     ,
         wg_header-kunnr       ,
         wg_header-matnr       ,
         wg_header-dtde_logist ,
         wg_header-dtate_logist.

  SELECT SINGLE *
     FROM zsdt0143
     INTO @DATA(wa_0143)
     WHERE contrato EQ @wg_header-bstkd
       AND cancelado EQ @abap_false.

  IF NOT sy-subrc IS INITIAL.
    CLEAR wa_0143.
    MESSAGE |Contrato { wg_header-bstkd } não existe!| TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE: wa_0143-empresa       TO wg_header-vkorg       ,
        wa_0143-vendedor      TO wg_header-vkgrp       ,
        wa_0143-dt_venda      TO wg_header-data_venda  ,
        wa_0143-corretor      TO wg_header-correto     ,
        wa_0143-cliente       TO wg_header-kunnr       ,
        wa_0143-tipo_padrao   TO wg_header-matnr       ,
        wa_0143-de_embarque   TO wg_header-dtde_logist ,
        wa_0143-ate_embarque  TO wg_header-dtate_logist.

  contrato = wg_header-bstkd.

  SELECT *
    FROM zsdt0045
    INTO TABLE @DATA(it_soma)
    WHERE bukrs    EQ @wg_header-vkorg
      AND contrato EQ @contrato.

  LOOP AT it_soma INTO DATA(wa_soma).
    ADD wa_soma-quantidade TO soma.
  ENDLOOP.

  IF soma GE wa_0143-quatidade.
    CLEAR wg_header-bstkd.
    MESSAGE |Limite da Quantidade do Contrato Excedida!| TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_id_contrato INPUT.

  CHECK wg_header-param_espec EQ c_a OR
*-CS2021000615 - 17.06.2021 - JT - inicio
        wg_header-param_espec EQ c_ax OR
        wg_header-param_espec EQ c_z.
*-CS2021000615 - 17.06.2021 - JT - fim

  DATA: l_contrato TYPE zsdt0045-contrato,
        l_soma     TYPE zsdt0045-quantidade.

  CLEAR: wg_header-vkorg       ,
         wg_header-vkgrp       ,
         wg_header-data_venda  ,
         wg_header-correto     ,
         wg_header-kunnr       ,
         wg_header-matnr       ,
         wg_header-dtde_logist ,
         wg_header-dtate_logist.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wg_header-id_contrato
    IMPORTING
      output = wg_header-id_contrato.

  SELECT SINGLE *
     FROM zsdt0143
     INTO wa_0143
     WHERE id_contrato  EQ wg_header-id_contrato
       AND cancelado    EQ abap_false.

  IF NOT sy-subrc IS INITIAL.
    CLEAR wa_0143.
    MESSAGE |Contrato { wg_header-id_contrato } não existe!| TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  MOVE: wa_0143-empresa       TO wg_header-vkorg       ,
        wa_0143-vendedor      TO wg_header-vkgrp       ,
        wa_0143-dt_venda      TO wg_header-data_venda  ,
        wa_0143-corretor      TO wg_header-correto     ,
        wa_0143-cliente       TO wg_header-kunnr       ,
        wa_0143-tipo_padrao   TO wg_header-matnr       ,
        wa_0143-de_embarque   TO wg_header-dtde_logist ,
        wa_0143-ate_embarque  TO wg_header-dtate_logist,
        wa_0143-contrato      TO wg_header-bstkd       ,

        " 18.12.2023 - 128467 - RBL -->
         wa_0143-porto      TO wg_header-porto       .
  " 18.12.2023 - 128467 - RBL --<

  l_contrato = wg_header-bstkd.

  SELECT *
    FROM zsdt0045
    INTO TABLE it_soma
    WHERE bukrs    EQ wg_header-vkorg
      AND contrato EQ l_contrato.

  LOOP AT it_soma INTO wa_soma.
    ADD wa_soma-quantidade TO l_soma.
  ENDLOOP.

  IF l_soma GE wa_0143-quatidade.
    CLEAR wg_header-bstkd.
    MESSAGE |Limite da Quantidade do Contrato Excedida!| TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CHECK_INS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_ins .

  DATA: contrato   TYPE zsdt0045-contrato,
        soma       TYPE zsdt0045-btgew,
        nro_sol_ov TYPE zsdt0045-objek,
        quantidade TYPE zsdt0143-quatidade.

  CLEAR erro_ins.

  CHECK wg_header-param_espec NE 'Z'.

  contrato = wg_header-bstkd.
  nro_sol_ov = wg_header-nro_sol_ov.

  SELECT *
      FROM zsdt0143
      INTO TABLE @DATA(it_0143)
      WHERE contrato  EQ @wg_header-bstkd
        AND empresa   EQ @wg_header-vkorg  "*-CS2022000332-#78223-02.06.2022-JT-inicio
        AND cancelado EQ @abap_false.

  IF NOT sy-subrc IS INITIAL.
    FREE it_0143.
  ELSE.
    LOOP AT it_0143 INTO DATA(wa_0143).
      wa_0143-quatidade = ( wa_0143-quatidade * ( ( wa_0143-tolerancia / 100 ) + 1 ) ).
      ADD wa_0143-quatidade TO quantidade.
    ENDLOOP.
  ENDIF.

  SELECT *
    FROM zsdt0045
    INTO TABLE @DATA(it_soma)
    WHERE bukrs    EQ @wg_header-vkorg
      AND contrato EQ @contrato
      AND objek    NE @nro_sol_ov.

  LOOP AT it_soma INTO DATA(wa_soma).
    ADD wa_soma-btgew TO soma.
  ENDLOOP.

  LOOP AT tg_instrucao INTO DATA(wa_inst).
    ADD wa_inst-btgew TO soma.
  ENDLOOP.

  IF soma GE quantidade.
    erro_ins = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_ZSDT0148
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_HEADER_NRO_SOL_OV  text
*----------------------------------------------------------------------*
FORM modifica_zsdt0148  USING    p_wg_header_nro_sol_ov TYPE zsdt0051-nro_sol_ov.

  DATA: wa_zsdt0148 TYPE zsdt0148.

  SELECT SINGLE *
    FROM zsdt0148
    INTO wa_zsdt0148
    WHERE nro_sol_ov EQ p_wg_header_nro_sol_ov
      AND status     NE 'C'.

  IF wa_zsdt0148 IS NOT INITIAL.

    UPDATE zsdt0148 SET nro_sol_ov = ' '
    WHERE nro_cont  = wa_zsdt0148-nro_cont
      AND nro_tranc = wa_zsdt0148-nro_tranc.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  REFRESH_LOG_0162
*&---------------------------------------------------------------------*
*       Ao reenviar a solicitação, valida se existe alguma aprovação
*     ativa e limpa o log de aprovações.
*----------------------------------------------------------------------*
FORM refresh_log_0162 .

  SELECT *
      FROM zsdt0162
      INTO TABLE @DATA(tl_0162)
        WHERE vbeln     EQ @wg_header-nro_sol_ov
          AND ck_recusa EQ ''.

  IF ( tl_0162[] IS NOT INITIAL ).
    LOOP AT tl_0162 INTO DATA(wl_0162).
      UPDATE zsdt0162
      SET ck_recusa     = 'S'
          dt_estorno    = sy-datum
          hr_estorno    = sy-uzeit
          user_estorno  = sy-uname
      WHERE id_log EQ wl_0162-id_log.
    ENDLOOP.
    COMMIT WORK.
  ENDIF.

  CLEAR: wl_0162.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  SHOW_ESTRAT
*&---------------------------------------------------------------------*
*       Exibe a estratégia de aprovação para a solicitação
*----------------------------------------------------------------------*
FORM show_estrat .

  IF ( tg_estrat[] IS INITIAL ).
    MESSAGE 'Não foi encontrado estratégia para solicitação!' TYPE 'I'.
    EXIT.
  ENDIF.

  DATA: wl_layout TYPE  slis_layout_alv.

  PERFORM montar_layout USING 'ESTRAT'.

  wl_layout-zebra = c_x.
  wl_layout-colwidth_optimize = c_x.
  wl_layout-window_titlebar = 'Estratégia de liberação'.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = v_report
      is_layout             = wl_layout
      it_fieldcat           = estrutura[]
      i_default             = ' '
      i_save                = ' '
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_estrat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0301  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0301 OUTPUT.

  SET PF-STATUS 'PF301'.
  SET TITLEBAR 'TI301'.

  wa_layout =
  VALUE #(
           zebra      = abap_true
           no_rowmark = abap_true
           cwidth_opt = abap_true
           sel_mode   = 'C'
           stylefname = 'STYLE'
         ).

  IF container11 IS INITIAL.

    CREATE OBJECT container11
      EXPORTING
        container_name = 'CC_LOTE'.

    CREATE OBJECT grid11
      EXPORTING
        i_parent = container11.

    PERFORM montar_layout USING 'LOTE'.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid11.

    lt_f4[] = VALUE #( (
                         fieldname = 'LGORT'
                         register  = abap_true
                         getbefore = abap_true
                     ) ).

    SET HANDLER: obg_toolbar->on_toolbar_0301  FOR grid11,
                 obg_toolbar->handle_user_0301 FOR grid11.

    CALL METHOD grid11->set_table_for_first_display
      EXPORTING
        is_layout       = wa_layout
      CHANGING
        it_fieldcatalog = t_fieldcatalog[]
        it_outtab       = t_0213.

    CALL METHOD grid11->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid11->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD grid11->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER: obg_toolbar->on_data_ch_f_0301 FOR grid11,
                 obg_toolbar->on_data_ch_0301   FOR grid11,
                 obg_toolbar->on_f4_0301        FOR grid11.

    grid11->set_ready_for_input( 1 ).

  ELSE.

    CALL METHOD grid11->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0301  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0301 INPUT.

  CASE sy-ucomm.
    WHEN 'CANCELAR'.
*     FREE t_0213.
      t_0213[] = t_0213_orig[].
      LOOP AT t_0213_orig INTO DATA(worig).
        READ TABLE it_0213 INTO DATA(w213) WITH KEY posnr = worig-posnr
                                                    lgort = worig-lgort.
        IF sy-subrc <> 0.
          APPEND worig  TO it_0213.
        ENDIF.
      ENDLOOP.
      LEAVE TO SCREEN 0.

    WHEN 'CONFIRMAR'.
      PERFORM add_0213_to_0053.

      IF NOT line_exists( t_0213[ volum = 0 ] ) AND flag NE 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ADD_0213
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_0213.

  TYPES: BEGIN OF ty_lgort,
           lgort TYPE lgort_d.
  TYPES: END   OF ty_lgort.
  TYPES: BEGIN OF ty_posnr,
           posnr TYPE posnr_va.
  TYPES: END   OF ty_posnr.

  DATA: t_lgort1 TYPE TABLE OF ty_lgort,
        t_lgort2 TYPE TABLE OF ty_lgort,
        t_posnr  TYPE TABLE OF ty_posnr.

*-CS2023000189-#126959-07.11.2023-JT-inicio
  SELECT *
    FROM zsdt0213
    INTO TABLE @DATA(t_0213_old)
   WHERE nro_sol_ov = @wg_header-nro_sol_ov
     AND status     = @abap_off.
*-CS2023000189-#126959-07.11.2023-JT-fim

  DATA(t0213) = VALUE zsdt0213_t( FOR l0213 IN it_0213 WHERE ( lgort IS NOT INITIAL ) ( CORRESPONDING #( l0213 ) ) ) .

  APPEND LINES OF t0213 TO it_0213_aux.

  IF it_0213_aux IS NOT INITIAL.
    MODIFY zsdt0213 FROM TABLE it_0213_aux.
    COMMIT WORK.
  ENDIF.

*-CS2023000189-#126959-07.11.2023-JT-inicio
  SELECT *
    FROM zsdt0213
    INTO TABLE @DATA(t_0213_new)
   WHERE nro_sol_ov = @wg_header-nro_sol_ov
     AND status     = @abap_off.

  CHECK t_0213_new[] <> t_0213_old[].

*-------------------------------------
*-verifica qual posicao sofreu ajuste
*-------------------------------------
  DATA(t_0213_pos) = t_0213_old[].
  APPEND LINES OF t_0213_new[] TO t_0213_pos[].

  SORT t_0213_pos BY posnr.
  DELETE ADJACENT DUPLICATES FROM t_0213_pos COMPARING posnr.

  FREE: t_posnr.

  LOOP AT t_0213_pos INTO DATA(w_0213_pos).
    FREE: t_lgort1, t_lgort2.

    LOOP AT t_0213_old INTO DATA(w_0213_old) WHERE posnr = w_0213_pos-posnr.
      APPEND w_0213_old-lgort TO t_lgort1.
    ENDLOOP.
    LOOP AT t_0213_new INTO DATA(w_0213_new) WHERE posnr = w_0213_pos-posnr.
      APPEND w_0213_new-lgort TO t_lgort2.
    ENDLOOP.

    IF t_lgort1[] <> t_lgort2[].
      APPEND w_0213_pos-posnr  TO t_posnr.
    ENDIF.
  ENDLOOP.

  CHECK t_posnr[] IS NOT INITIAL.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Integrando Trace Cotton...'.

*-------------------
* enviar trace cotton
*-------------------
  LOOP AT t_posnr INTO DATA(w_posnr).
    CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
      EXPORTING
        i_nro_sol_ov = wg_header-nro_sol_ov
        i_posnr      = w_posnr-posnr
        i_acao       = 'C'
      EXCEPTIONS
        OTHERS       = 1.
  ENDLOOP.
*-CS2023000189-#126959-07.11.2023-JT-fim

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ADD_0213_TO_0053
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM add_0213_to_0053.
  DATA: quantidade_inst TYPE numc10,
        inst_click      TYPE zsdt0045-instrucao,
        mensagem        TYPE char90,
        qtd_vinculada   TYPE string,
        qtd_usada       TYPE string,
        qtd_cadastrada  TYPE string,
        p_numero        TYPE numc10.

  IF line_exists( t_0213[ volum = 0 ] ).
    MESSAGE 'Existe Volume não Registrado!' TYPE 'I'.
    EXIT.
  ENDIF.

*-CS2023000189-#126959-07.11.2023-JT-inicio
  IF t_0213[] IS INITIAL.
    MESSAGE 'Ordem de Venda está sem Lotes!' TYPE 'I'.
    t_0213[] = t_0213_orig[].
    flag     = abap_true.
    EXIT.
  ENDIF.
*-CS2023000189-#126959-07.11.2023-JT-fim

  flag = abap_false.

  LOOP AT t_0213 INTO DATA(w_0123).

    LOOP AT it_0213 INTO DATA(w_0123_t) WHERE lgort = w_0123-lgort AND posnr NE w_0123-posnr.

      qtd_usada = qtd_usada + w_0123_t-volum.

    ENDLOOP.

    IF w_click-instrucao_ant IS NOT INITIAL.
      inst_click = w_click-instrucao_ant.
    ELSE.
      inst_click = w_click-instrucao.
    ENDIF.

    "SELECT SINGLE QUANTIDADE FROM ZSDT0045 INTO QUANTIDADE_INST WHERE  CHARG EQ W_0123-LGORT AND INSTRUCAO EQ INST_CLICK .

    " READ TABLE TL_0045 INTO DATA(W_0045) WITH KEY CHARG = W_0123-LGORT
    "                                                   INSTRUCAO = INST_CLICK .

    LOOP AT tl_0045 INTO DATA(w_0045) WHERE charg = w_0123-lgort. " and  INSTRUCAO = INST_CLICK .

      p_numero = p_numero + w_0045-quantidade.

    ENDLOOP.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = p_numero
      IMPORTING
        output = p_numero.



    DATA p_num TYPE numc10.

    p_num = qtd_usada + w_0123-volum.

    IF p_numero LT p_num.
      qtd_vinculada =  w_0123-volum.
      qtd_cadastrada = p_numero - qtd_usada.
      CONCATENATE 'QUANTIDADE VINCULADA' qtd_vinculada 'DO LOTE' w_0123-lgort 'É SUPERIOR A QUANTIDADE DISPONIVEL' qtd_cadastrada INTO mensagem SEPARATED BY space.
      MESSAGE mensagem TYPE 'I'.


      IF NOT line_exists( it_0213[ lgort = w_0123-lgort posnr = w_0123-posnr ] ).
        w_0123-volum = 0.
        READ TABLE t_0213 TRANSPORTING NO FIELDS WITH KEY posnr = w_0123-posnr
                                                     lgort = w_0123-lgort
                                                     nro_sol_ov = w_0123-nro_sol_ov.

        MODIFY t_0213 FROM w_0123 INDEX sy-tabix  TRANSPORTING volum.
      ELSEIF line_exists( it_0213[ lgort = w_0123-lgort posnr = w_0123-posnr ] ).
        READ TABLE it_0213 INTO DATA(teste) WITH KEY posnr = w_0123-posnr
                                                     lgort = w_0123-lgort
                                                     nro_sol_ov = w_0123-nro_sol_ov.
        w_0123-volum = teste-volum.
        READ TABLE t_0213 TRANSPORTING NO FIELDS WITH KEY posnr = w_0123-posnr
                                                          lgort = w_0123-lgort
                                                          nro_sol_ov = w_0123-nro_sol_ov.
        MODIFY t_0213 FROM w_0123 INDEX sy-tabix  TRANSPORTING volum.
      ENDIF.
      flag = abap_true.
      EXIT.
    ENDIF.

    IF NOT line_exists( it_0213[ lgort = w_0123-lgort posnr = w_0123-posnr ] ).
      APPEND w_0123 TO it_0213.
    ELSE. "ELSEIF LINE_EXISTS( IT_0213[ LGORT = W_0123-LGORT POSNR = W_0123-POSNR ] ).
      READ TABLE it_0213 TRANSPORTING NO FIELDS WITH KEY posnr = w_0123-posnr
                                                         lgort = w_0123-lgort
                                                         nro_sol_ov = w_0123-nro_sol_ov.
      MODIFY it_0213 FROM w_0123 INDEX sy-tabix  TRANSPORTING volum.
    ENDIF.
    CLEAR: qtd_usada.
  ENDLOOP.

  TRY .
      tg_itens[ posnr = w_click-posnr ]-volum = REDUCE zsded029( INIT x = 0 FOR l_0213 IN t_0213 NEXT x = x + l_0213-volum ).
    CATCH cx_sy_itab_line_not_found.
  ENDTRY.

  IF flag NE abap_true.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_DROPDOWN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_dropdown .

  DATA lt_dropdown TYPE lvc_t_drop.
  DATA: lt_dral TYPE lvc_t_dral.

  APPEND VALUE #( handle = '2' value = 'C'  ) TO lt_dropdown.
  APPEND VALUE #( handle = '2' value = 'R'  ) TO lt_dropdown.

*  APPEND VALUE #( handle = '3' value = '001'  ) TO lt_dropdown.
*  APPEND VALUE #( handle = '3' value = '002'  ) TO lt_dropdown.

  CALL METHOD grid1->set_drop_down_table
    EXPORTING
      it_drop_down       = lt_dropdown
      it_drop_down_alias = lt_dral.

ENDFORM.

MODULE user_command_0110 INPUT.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      DATA: lit_zsdt0053_aux TYPE TABLE OF zsdt0053.

      IF gwa_change_ruc_item-numero_ruc IS INITIAL.
        MESSAGE 'Não foi informado o Numero da RUC!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF gwa_change_ruc_item-numero_ruc EQ gwa_change_ruc_item-numero_ruc_old.
        MESSAGE 'Informe uma RUC diferente do item da solicitação!' TYPE 'I'.
        EXIT.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0053 INTO @DATA(lwa_0053)
       WHERE nro_sol_ov EQ @gwa_change_ruc_item-nro_sol_ov
         AND posnr      EQ @gwa_change_ruc_item-posnr.

      IF sy-subrc NE 0.
        MESSAGE 'Registro Solicitação Venda não foi gravado!' TYPE 'I'.
        EXIT.
      ENDIF.

      IF lwa_0053-vbeln IS NOT INITIAL.
        SELECT SINGLE *
          FROM lips INTO @DATA(lwa_lips)
         WHERE vgbel EQ @lwa_0053-vbeln.

        IF ( sy-subrc EQ 0 ) AND ( lwa_lips-vbeln IS NOT INITIAL ).
          SELECT SINGLE *
            FROM zdoc_exp INTO @DATA(lwa_doc_exp)
           WHERE vbeln EQ @lwa_lips-vbeln.

          IF ( sy-subrc EQ 0 ) AND ( lwa_doc_exp-id_due IS NOT INITIAL ).
            MESSAGE |Item da Solicitação já foi vinculada a DU-e Id: { lwa_doc_exp-id_due } !| TYPE 'I'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

      "Checar se possui nomeação para RUC Informada - Ini
      CLEAR: lit_zsdt0053_aux[].

      SELECT *
        FROM zsdt0053 INTO TABLE lit_zsdt0053_aux
       WHERE numero_ruc EQ gwa_change_ruc_item-numero_ruc.

      DELETE lit_zsdt0053_aux WHERE id_nomeacao_tran IS INITIAL.
      DELETE lit_zsdt0053_aux WHERE nro_sol_ov EQ gwa_change_ruc_item-nro_sol_ov
                                AND posnr      EQ gwa_change_ruc_item-posnr.

      IF lit_zsdt0053_aux[] IS NOT INITIAL.
        READ TABLE lit_zsdt0053_aux INTO DATA(lwa_zsdt0053_aux) INDEX 1.
        IF ( sy-subrc EQ 0 ) AND ( lwa_zsdt0053_aux-id_nomeacao_tran IS NOT INITIAL ).
          gwa_change_ruc_item-id_nomeacao_tran = lwa_zsdt0053_aux-id_nomeacao_tran.
        ENDIF.
      ENDIF.
      "Checar se possui nomeação para RUC Informada - Fim

      UPDATE zsdt0053 SET numero_ruc       = gwa_change_ruc_item-numero_ruc
                          id_nomeacao_tran = gwa_change_ruc_item-id_nomeacao_tran
       WHERE nro_sol_ov EQ gwa_change_ruc_item-nro_sol_ov
         AND posnr      EQ gwa_change_ruc_item-posnr.

      gwa_change_ruc_item-change_item = abap_true.

      MESSAGE 'Numero da RUC alterado com sucesso!' TYPE 'S'.

      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
      EXIT.
  ENDCASE.

ENDMODULE.

MODULE status_0110 OUTPUT.
  SET PF-STATUS 'PF0110'.
  SET TITLEBAR 'TB0110' WITH gwa_change_ruc_item-posnr.
ENDMODULE.

FORM fm_get_par_user.

  CHECK git_parametros_user[] IS INITIAL.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = git_parametros_user
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_TRATA_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_trata_fieldcatalog .
  LOOP AT t_fieldcatalog INTO w_fieldcatalog.
    CASE wg_header-param_espec.
      WHEN c_a OR c_e OR c_ax OR c_z.

*-CS2021000615 - 17.06.2021 - AOENNING - inicio
        IF line_exists( it_0235[ tabela = 'ZSDT0053' campo = w_fieldcatalog-fieldname ] ).
          w_fieldcatalog-no_out = c_x.
        ELSE.
          w_fieldcatalog-no_out = space.
        ENDIF.
*-CS2021000615 - 17.06.2021 - AOENNING - fim

      WHEN OTHERS.
    ENDCASE.
* INICIO - IR238739 - 23.06.2025 - RRIBEIRO - STEFANINI
    IF w_fieldcatalog-fieldname EQ 'PMEIN'.
      w_fieldcatalog-tabname = 'TG_ITENS'.
      w_fieldcatalog-ref_field = 'ZIEME'.
      w_fieldcatalog-ref_table = 'ZSDT0053'.
    ENDIF.
* FIM - IR238739 - 23.06.2025 - RRIBEIRO - STEFANINI

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PF_MODIFY_FRETE_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_FORM_LOTE  text
*----------------------------------------------------------------------*
FORM pf_modify_frete_entrada.

*  DATA: posicaox TYPE n LENGTH 2 VALUE 5.
*  DATA: posicaoy TYPE n LENGTH 2 VALUE 2.
*  DATA: largura TYPE n LENGTH 2.
*  DATA: comprimento TYPE n LENGTH 2.
*
*  vbak-kvgr5 = wa_emite_entrada-kvgr5.
*
*  largura = posicaox + 30.
*  comprimento = posicaoy.
*
*  CALL SCREEN 0800
*  STARTING AT
*  posicaox "posição X
*  posicaoy "posição Y
*  ENDING AT
*  largura  "Largura
*  comprimento. "Comprimento

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0800  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0800 OUTPUT.
  SET PF-STATUS 'PF0800'.
  SET TITLEBAR 'TB0800'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0800  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0800 INPUT.
*  CASE sy-ucomm.
*    WHEN 'CANCEL'.
*      LEAVE TO SCREEN 0.
*    WHEN 'OK'.
*
*      DATA: wl_header_inx2 TYPE  bapisdh1x,
*            tl_return_aux  TYPE TABLE OF bapiret2   WITH HEADER LINE,
*            wl_vbeln       TYPE vbeln,
*            wl_header_in   TYPE bapisdh1,
*            wl_header_inx  TYPE bapisdh1x.
*
*      wa_emite_entrada-kvgr5 = vbak-kvgr5.
*
*      MODIFY tg_form_lote FROM wa_emite_entrada INDEX index_entrada.
*
*      UPDATE  zsdt0066 SET kvgr5 = vbak-kvgr5
*                     WHERE nro_sol_ov EQ wg_header-nro_sol_ov
*                       AND instrucao  EQ wa_emite_entrada-instrucao
*                       AND posnr      EQ wa_emite_entrada-posnr.
*
*      UPDATE zsdt0158 SET kvgr5 = vbak-kvgr5
*                     WHERE nro_sol_ov EQ wg_header-nro_sol_ov.
*
*      CHECK wa_emite_entrada-vbeln IS NOT INITIAL.
*
*      wl_header_in-cust_grp5   = vbak-kvgr5.
*      wl_header_inx-cust_grp5  = abap_true.
*      wl_header_inx-updateflag = 'U'.
*
*      CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
*        EXPORTING
*          salesdocument    = wa_emite_entrada-vbeln
*          order_header_in  = wl_header_in
*          order_header_inx = wl_header_inx
*        TABLES
*          return           = tl_return_aux.
*
*      READ TABLE tl_return_aux  WITH KEY type = 'E'.
*
*      IF sy-subrc IS NOT INITIAL.
*        COMMIT WORK.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = abap_true.
*
*        LEAVE TO SCREEN 0.
*      ELSE.
*        ROLLBACK work.
*        MESSAGE tl_return_aux-message TYPE 'I'.
*      ENDIF.
*
*    ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_ID_PORTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_id_porto INPUT.

  TYPES: BEGIN OF ty_porto,
           porto TYPE ort01_gp,
         END OF ty_porto.

  TYPES: BEGIN OF ty_lfa1_porto,
           lifnr TYPE lifnr,
           ort01 TYPE ort01_gp,
         END OF ty_lfa1_porto.

  DATA: it_porto TYPE TABLE OF ty_porto.
  FIELD-SYMBOLS: <wa_porto>    TYPE ty_lfa1_porto,
                 <wa_f4_porto> TYPE ty_porto.

  SELECT terminal
            FROM zsdt0108
           INTO TABLE @DATA(it_zsdt0108).

  SELECT lifnr , ort01 FROM lfa1
    INTO TABLE @DATA(it_lfa1)
    FOR ALL ENTRIES IN @it_zsdt0108
    WHERE lifnr EQ @it_zsdt0108-terminal.




  DELETE ADJACENT DUPLICATES FROM it_lfa1 COMPARING lifnr.

  LOOP AT it_lfa1 ASSIGNING <wa_porto>.
    APPEND INITIAL LINE TO it_porto  ASSIGNING <wa_f4_porto>.

    MOVE <wa_porto>-ort01 TO <wa_f4_porto>-porto.

  ENDLOOP.





  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PORTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WG_HEADER-PORTO'
      value_org       = 'S'
    TABLES
      value_tab       = it_porto
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GET_SET_VALORES_Z
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_5022   text
*      -->P_5023   text
*      <--P_WG_VALOR  text
*----------------------------------------------------------------------*
FORM get_set_valores_z  USING p_campo
                              p_get_set
                     CHANGING p_valor.


  ASSIGN COMPONENT p_campo  OF STRUCTURE <fs_line> TO <fs_campo>.

  IF  <fs_campo>(1) EQ 'N' AND <fs_campo> NE 'NYFRAME'.

    PERFORM get_set_valores USING 'QTDFIXADA'
                                  'G'
                                  CHANGING wg_valor.

    p_valor = wg_valor.


  ENDIF.



ENDFORM.

*-BUG 120423-10.08.2023-JT-inicio
**********************************************************************
* REENVIA OV TRACE COTTON
**********************************************************************
FORM f_reenvia_trace.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Integrando...'.

  SELECT *
    FROM zsdt0053
    INTO TABLE @DATA(t_0053)
   WHERE nro_sol_ov EQ @wg_header-nro_sol_ov
     AND status     NE 'C'.

  LOOP AT t_0053 INTO DATA(w_0053).
    DATA(l_task) = 'TRACE_ORDEM_VENDA' && w_0053-nro_sol_ov && w_0053-posnr.

    CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE' STARTING NEW TASK l_task
      EXPORTING
        i_nro_sol_ov = w_0053-nro_sol_ov
        i_posnr      = w_0053-posnr
        i_acao       = 'C'
      EXCEPTIONS
        OTHERS       = 1.
  ENDLOOP.

ENDFORM.
**********************************************************************
* REENVIA OV TRACE COTTON
**********************************************************************
FORM f_reenvia_trace_ordens.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = t_rows[].

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecione ao menos uma linha!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Aguarde...Integrando...'.

*----------------------------------------------------
* reenvio
*----------------------------------------------------
  LOOP AT t_rows INTO w_rows.

    READ TABLE tg_itens INTO DATA(wg_itens) INDEX w_rows-index.

    CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
      EXPORTING
        i_nro_sol_ov = wg_header-nro_sol_ov
        i_posnr      = wg_itens-posnr
        i_acao       = 'C'
      EXCEPTIONS
        OTHERS       = 1.
  ENDLOOP.

ENDFORM.
*-BUG 120423-10.08.2023-JT-fim

FORM gera_boleto. "PSA

  "PSA IAGUAL ZSDR016 LINHA 4652
  DATA: wl_instrucoes TYPE zfi_boleto,
        vl_juros_ano  TYPE zsdt0040-juros_ano,
        vlr_juros     TYPE c LENGTH 7,
        vlr_multa     TYPE c LENGTH 7,
        wl_boleto     TYPE j_1bdocnum.

  MOVE wg_header-nro_sol_ov TO wl_boleto.
  MOVE wg_header-tx_juros TO vlr_juros.
  MOVE wg_header-tx_multa TO vlr_multa.

  wl_instrucoes-txt_instrucao1 = 'Sujeito a protesto.'.

  IF wg_header-tx_juros > 0 .
    CONCATENATE 'Após a data de vencimento será cobrado ' vlr_juros ' % de Juros a.a'
            INTO wl_instrucoes-txt_instrucao2 SEPARATED BY space.
  ENDIF.

  IF wg_header-tx_multa > 0 .
    CONCATENATE 'Após a data de vencimento será cobrado ' vlr_multa ' % de Multa'
            INTO wl_instrucoes-txt_instrucao3 SEPARATED BY space.
  ENDIF.


  CALL FUNCTION 'Z_SD_PRINT_BOLETO' "PSA
    EXPORTING
      doc_numero     = wl_boleto
      tipo           = 'O'  "Ordem
      hbkid          = wg_cond_pgt-hbkid
      instrucoes     = wl_instrucoes
    EXCEPTIONS
      nao_localizado = 1
      OTHERS         = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CHECK_PORTO  INPUT
*&---------------------------------------------------------------------*
MODULE check_porto INPUT.

*  IF wg_header-porto IS INITIAL.
*    MESSAGE e016(ds) WITH 'Preencher campo' 'PORTO'.
*  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_valida_campo_alv_initial
*&---------------------------------------------------------------------*
FORM f_valida_campo_alv_initial USING uv_field TYPE c
                                      uv_linha TYPE c
                                      uv_obj TYPE c.

  DATA lv_cname TYPE c LENGTH 30.

  ASSIGN COMPONENT uv_field  OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_campo>).

  CHECK sy-subrc EQ 0.

  IF uv_field = 'QTDFIXADA'.

    IF <fs_campo> = '0,00'.

      tg_msg_ret-field = uv_field.

      tg_msg_ret-aba = c_tab_strip-tab3.

      tg_msg_ret-tabix = uv_linha.

      tg_msg_ret-obj = uv_obj.

      READ TABLE gt_col_name ASSIGNING FIELD-SYMBOL(<fs_name>)
        WITH KEY cname = uv_field.

      IF sy-subrc EQ 0.
        lv_cname = <fs_name>-fname.
      ELSE.
        lv_cname = uv_field.
      ENDIF.

      CONCATENATE TEXT-e01 lv_cname TEXT-e44 uv_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.

      CLEAR: tg_msg_ret.

    ENDIF.

  ELSE.

    IF <fs_campo> IS INITIAL.

      tg_msg_ret-field = uv_field.

      tg_msg_ret-aba = c_tab_strip-tab3.

      tg_msg_ret-tabix = uv_linha.

      tg_msg_ret-obj = uv_obj.

      READ TABLE gt_col_name ASSIGNING <fs_name>
        WITH KEY cname = uv_field.

      IF sy-subrc EQ 0.
        lv_cname = <fs_name>-fname.
      ELSE.
        lv_cname = uv_field.
      ENDIF.

      CONCATENATE TEXT-e01 lv_cname TEXT-e44 uv_linha INTO  tg_msg_ret-msg SEPARATED BY space.

      APPEND tg_msg_ret.

      CLEAR: tg_msg_ret.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preenche_cname
*&---------------------------------------------------------------------*
FORM f_preenche_cname .

  DATA ls_line TYPE ty_col_name.

  CLEAR gt_col_name.

  ls_line-cname = 'CBOT'. ls_line-fname = 'REF. CBOT'.APPEND ls_line TO gt_col_name.
  ls_line-cname = 'WAERS'. ls_line-fname = 'Moeda'.APPEND ls_line TO gt_col_name.
  ls_line-cname = 'QTDFIXADA'. ls_line-fname = 'Qtd. Fixada'. APPEND ls_line TO gt_col_name.
  ls_line-cname = 'MONAT'. ls_line-fname = 'Mês Fixação'.APPEND ls_line TO gt_col_name.
  ls_line-cname = 'SAFRA'. ls_line-fname = 'Safra'.APPEND ls_line TO gt_col_name.
  ls_line-cname = 'VALDT'. ls_line-fname = 'Dt. Fixação'. APPEND ls_line TO gt_col_name.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_zsdt0059_redist
*&---------------------------------------------------------------------*
FORM f_zsdt0059_redist TABLES t_zsdt0059 STRUCTURE zsdt0059.

  DATA lt_zsdt0059_2 TYPE TABLE OF zsdt0059_2.

  LOOP AT t_zsdt0059 ASSIGNING FIELD-SYMBOL(<fs_0059>) WHERE redist = abap_true.

    DATA(lv_tabix) = sy-tabix.

    APPEND <fs_0059> TO lt_zsdt0059_2.

    DELETE t_zsdt0059 INDEX lv_tabix.

  ENDLOOP.

  CHECK lt_zsdt0059_2 IS NOT INITIAL.

  MODIFY zsdt0059_2 FROM TABLE lt_zsdt0059_2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_change_bezei_redist
*&---------------------------------------------------------------------*
FORM f_change_bezei_redist USING u_bezei TYPE c
                                 u_posnr TYPE posnr.

  DATA lv_valor TYPE c LENGTH 30.
  DATA lv_ultimo TYPE c LENGTH 30.
  DATA lv_bezei TYPE c LENGTH 4.
  DATA lv_soma TYPE i.
  DATA lv_n1 TYPE i.
  DATA lv_n2 TYPE i.

  DATA lv_fixacao TYPE posnr.

  FIELD-SYMBOLS <fs_nivel> TYPE zsded018.
  FIELD-SYMBOLS <fs_bezei> TYPE bezei30.
  FIELD-SYMBOLS <fs_fixacao> TYPE posnr.
  FIELD-SYMBOLS <fs_bezei_tab> LIKE c_bezei.

  lv_ultimo = 'X01'. " <-- inicialização

  PERFORM get_set_valores USING 'POSNR' 'G' CHANGING lv_fixacao." 21.05.2024

  LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line2>).

    ASSIGN ('<FS_LINE2>-POSNR') TO <fs_fixacao>.

    CHECK sy-subrc EQ 0.

    CHECK lv_fixacao EQ <fs_fixacao>. " 21.05.2024

    ASSIGN ('<FS_LINE2>-BEZEI') TO <fs_bezei>.

    CHECK strlen( <fs_bezei> ) = 3.

    CHECK <fs_bezei>(1) = u_bezei.

    lv_n1 = lv_ultimo+1(2).
    lv_n2 = <fs_bezei>+1(2).

    IF lv_n1 < lv_n2.
      lv_ultimo = <fs_bezei>.

*      ASSIGN ('<FS_LINE2>-NIVEL') TO <fs_nivel>.
*      lv_ult_nivel = <fs_nivel>.
*      ADD 1 TO lv_ult_nivel.

    ENDIF.

  ENDLOOP.

  IF lv_ultimo IS NOT INITIAL.

    lv_soma = lv_ultimo+1.

    ADD 1 TO lv_soma.

  ENDIF.

  " começa no 16
  IF lv_soma < 16.
    lv_soma = 16.
  ENDIF.

  WRITE lv_soma TO lv_bezei LEFT-JUSTIFIED NO-GAP.

  lv_valor = lv_ultimo(1) && lv_bezei.

  PERFORM get_set_valores USING 'BEZEI' 'S' CHANGING lv_valor.
  "PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING lv_ult_nivel. " 21.05.2024

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_change_bezei_redist
*&---------------------------------------------------------------------*
FORM f_change_bezei_redist_2 USING u_bezei TYPE c
                                 u_posnr TYPE posnr
  CHANGING cv_bezei TYPE c.

  DATA lv_valor TYPE c LENGTH 30.
  DATA lv_ultimo TYPE c LENGTH 30.
  DATA lv_bezei TYPE c LENGTH 4.
  DATA lv_soma TYPE i.
  DATA lv_n1 TYPE i.
  DATA lv_n2 TYPE i.

  DATA lv_fixacao TYPE posnr.

  FIELD-SYMBOLS <fs_nivel> TYPE zsded018.
  FIELD-SYMBOLS <fs_bezei> TYPE bezei30.
  FIELD-SYMBOLS <fs_fixacao> TYPE posnr.
  FIELD-SYMBOLS <fs_bezei_tab> LIKE c_bezei.

  lv_ultimo = 'X01'. " <-- inicialização

  PERFORM get_set_valores USING 'POSNR' 'G' CHANGING lv_fixacao." 21.05.2024

  LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line2>).

    ASSIGN ('<FS_LINE2>-POSNR') TO <fs_fixacao>.

    CHECK sy-subrc EQ 0.

    CHECK lv_fixacao EQ <fs_fixacao>. " 21.05.2024

    ASSIGN ('<FS_LINE2>-BEZEI') TO <fs_bezei>.

    CHECK strlen( <fs_bezei> ) = 3.

    CHECK <fs_bezei>(1) = u_bezei.

    lv_n1 = lv_ultimo+1(2).
    lv_n2 = <fs_bezei>+1(2).

    IF lv_n1 < lv_n2.
      lv_ultimo = <fs_bezei>.

*      ASSIGN ('<FS_LINE2>-NIVEL') TO <fs_nivel>.
*      lv_ult_nivel = <fs_nivel>.
*      ADD 1 TO lv_ult_nivel.

    ENDIF.

  ENDLOOP.

  IF lv_ultimo IS NOT INITIAL.

    lv_soma = lv_ultimo+1.

    ADD 1 TO lv_soma.

  ENDIF.

  " começa no 16
  IF lv_soma < 16.
    lv_soma = 16.
  ENDIF.

  WRITE lv_soma TO lv_bezei LEFT-JUSTIFIED NO-GAP.

  cv_bezei = lv_ultimo(1) && lv_bezei.

  "PERFORM get_set_valores USING 'BEZEI' 'S' CHANGING lv_valor.
  "PERFORM get_set_valores USING 'NIVEL' 'S' CHANGING lv_ult_nivel. " 21.05.2024

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_append_zsdt0059_2
*&---------------------------------------------------------------------*
FORM f_append_zsdt0059_2 TABLES tl_0059 STRUCTURE zsdt0059.

  SELECT * FROM zsdt0059_2
    INTO TABLE @DATA(lt_0059_2)
      WHERE nro_sol_ov EQ @wg_header-nro_sol_ov.

  LOOP AT lt_0059_2 ASSIGNING FIELD-SYMBOL(<fs_0059_2>).

    APPEND INITIAL LINE TO tl_0059 ASSIGNING FIELD-SYMBOL(<fs_0059>).

    MOVE-CORRESPONDING <fs_0059_2> TO <fs_0059>.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_preco_redist
*&---------------------------------------------------------------------*
FORM f_preco_redist TABLES t_zsdt0059 STRUCTURE zsdt0059.

  DATA lv_posnr TYPE posnr.
  DATA lv_bezei TYPE c LENGTH 3.
  DATA lv_field TYPE zsded035.

  DATA tl_input_0059 TYPE zsdt0059.

  PERFORM f_corrige_qtd_preco_redist.

  UNASSIGN <fs_line>." BREAK rblima.

  LOOP AT <fs_table> ASSIGNING <fs_line>.

    CLEAR wg_valor.

    PERFORM get_set_valores
      USING 'REDIST' 'G'
   CHANGING wg_valor.

    CHECK wg_valor = 'X'.


    PERFORM get_set_valores USING 'COD_FP'
                                  'G'
                         CHANGING wg_valor.
    CONDENSE wg_valor NO-GAPS.

    CLEAR: wg_valor_aux.

    PERFORM get_set_valores USING 'BEZEI'
                                  'G'
                         CHANGING wg_valor_aux.

    lv_bezei = wg_valor_aux.

    wg_valor_aux = wg_valor_aux(1) && '1'.

    " 21.05.2024 - RAMON --->
    PERFORM get_set_valores USING 'POSNR' 'G' CHANGING lv_posnr.
    " 21.05.2024 - RAMON ---<

    " 22.02.2024 - RAMON -->
    PERFORM get_set_valores USING 'PRECO' 'G' CHANGING wg_valor.

    IF wg_valor IS NOT INITIAL.
      lv_field = 'PRECO'.
    ELSE.
      lv_field = 'QTDFIXADA'.
    ENDIF.

    " 22.02.2024 - RAMON <--

    READ TABLE t_zsdt0059 TRANSPORTING NO FIELDS
      WITH KEY posnr = lv_posnr
               bezei = lv_bezei
               field = lv_field.

    CHECK sy-subrc NE 0.

    READ TABLE tg_preco_n
      WITH KEY bezei = wg_valor_aux
               field = lv_field.

    IF sy-subrc EQ 0.

      wg_valor = tg_preco_n-cod_fp.

    ELSE.
      EXIT.
    ENDIF.

    CHECK sy-subrc EQ 0.

    LOOP AT tg_preco_n WHERE cod_fp = wg_valor
                         AND bezei = wg_valor_aux
                         AND field = lv_field.

      MOVE-CORRESPONDING: tg_preco_n TO tl_input_0059.

      MOVE: lv_bezei                  TO tl_input_0059-bezei.
      MOVE: tg_preco_n-formula        TO tl_input_0059-formula.
      MOVE: tg_preco_n-ocbot          TO tl_input_0059-ocbot.
      MOVE: tg_preco_n-preco          TO tl_input_0059-preco.
      MOVE: tg_preco_n-c_decimais     TO tl_input_0059-c_decimais.
      MOVE: wg_header-nro_sol_ov      TO tl_input_0059-nro_sol_ov,
            sy-uname                  TO tl_input_0059-usnam,
            sy-datum                  TO tl_input_0059-data_atual,
            sy-uzeit                  TO tl_input_0059-hora_atual.


      CLEAR: wg_valor.
      PERFORM get_set_valores USING tg_preco_n-field
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-formula2 = wg_valor.

      IF tl_input_0059-tipo_calc EQ 'V'.
        tl_input_0059-formula = tl_input_0059-formula2.
      ENDIF.

      TRANSLATE tl_input_0059-formula2 USING '. '.
      TRANSLATE tl_input_0059-formula2 USING ',.'.
      CONDENSE tl_input_0059-formula2  NO-GAPS.


      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'WAERS'
                                    'G'
                           CHANGING  wg_valor.
      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-waers = wg_valor.

      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'CBOT'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.

      tl_input_0059-cbot = wg_valor.

      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'MONAT'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.

      tl_input_0059-monat = wg_valor.

      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'VALDT'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.

      tl_input_0059-valdt = wg_valor.

      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'POSNR'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-posnr = wg_valor.

      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'POSNR1'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-posnr1 = wg_valor.


      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'VALDT_HEDGE'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-valdt_hedge = wg_valor.


      CLEAR: wg_valor.
      PERFORM get_set_valores USING 'NIVEL'
                                    'G'
                           CHANGING wg_valor.
      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-nivel = wg_valor.

      " 14.02.2024 - 121095 - RBL -->
      "IF lv_redist = abap_true.

      CLEAR wg_valor.

      PERFORM get_set_valores
        USING 'REDIST' 'G'
     CHANGING wg_valor.

      CONDENSE wg_valor NO-GAPS.
      tl_input_0059-redist = wg_valor.

      "ENDIF.

      " 14.02.2024 - 121095 - RBL --<

      APPEND tl_input_0059 TO t_zsdt0059.
      CLEAR:tl_input_0059.

    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_corrige_qtd_preco_redist
*&---------------------------------------------------------------------*
FORM f_corrige_qtd_preco_redist .

  DATA lv_achou.
  "BREAK rblima.

  UNASSIGN <fs_line>." BREAK rblima.

  LOOP AT <fs_table> ASSIGNING <fs_line>.

    """" deixa a coluna QTDFIXADA guardada para apagar quando for preço
    """ ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_qtd_1>)." 21.05.2024
    ASSIGN COMPONENT 'REDIST' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_redist>).
    ASSIGN COMPONENT 'FIELD' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_field>).
    ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_bezei>).
    ASSIGN COMPONENT 'POSNR' OF STRUCTURE <fs_line> TO FIELD-SYMBOL(<fs_posnr>).

    CHECK sy-subrc EQ 0.

    CHECK <fs_redist> = 'X'.

    CHECK <fs_field> = 'PRECO'.

    CLEAR lv_achou.

    " esse codigo abaixo, adiciona linhas para o field QTDFIXADA.
    " o programa espera que tenha tanto linha de PRECO quanto QTDFIXADA
    LOOP AT <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line_2>).

      ASSIGN COMPONENT 'REDIST' OF STRUCTURE <fs_line_2> TO FIELD-SYMBOL(<fs_redist2>).
      ASSIGN COMPONENT 'FIELD' OF STRUCTURE <fs_line_2> TO FIELD-SYMBOL(<fs_field2>).
      ASSIGN COMPONENT 'BEZEI' OF STRUCTURE <fs_line_2> TO FIELD-SYMBOL(<fs_bezei2>).
      ASSIGN COMPONENT 'POSNR' OF STRUCTURE <fs_line_2> TO FIELD-SYMBOL(<fs_posnr2>).

      CHECK <fs_posnr> EQ <fs_posnr2>.

      CHECK <fs_redist2> = 'X'.

      CHECK <fs_field2> = 'QTDFIXADA'.

      CHECK <fs_bezei> = <fs_bezei2>.

      lv_achou = 'X'.

      EXIT.

    ENDLOOP.

    CHECK lv_achou IS INITIAL.

    APPEND INITIAL LINE TO <fs_table> ASSIGNING FIELD-SYMBOL(<fs_line_new>).

    MOVE-CORRESPONDING <fs_line> TO <fs_line_new>.

    ASSIGN COMPONENT 'QTDFIXADA' OF STRUCTURE <fs_line_new> TO FIELD-SYMBOL(<fs_qtd>).
    ASSIGN COMPONENT 'PRECO' OF STRUCTURE <fs_line_new> TO FIELD-SYMBOL(<fs_preco>).
    ASSIGN COMPONENT 'FIELD' OF STRUCTURE <fs_line_new> TO FIELD-SYMBOL(<fs_field_new>).
    ASSIGN COMPONENT 'FORMULA' OF STRUCTURE <fs_line_new> TO FIELD-SYMBOL(<fs_formula>).

    CLEAR <fs_preco>.

    <fs_field_new> = 'QTDFIXADA'.
    <fs_formula> = <fs_qtd>.

    """" chegou aqui, adicionou a coluna por QTDFIXADA, então apaga esse valor, da coluna preço
    """CLEAR <fs_qtd_1>.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GUARDA_SALDO_FIX
*&---------------------------------------------------------------------*
FORM f_guarda_saldo_fix USING iv_fix TYPE posnr
                              iv_valor TYPE c.

  DATA lv_valor TYPE c LENGTH 200.

  DATA lv_menge_novo TYPE zsdt0053-zmeng.

  CHECK wg_redistribuir = abap_true.

  CHECK iv_fix IS NOT INITIAL.

  lv_valor = iv_valor.

  CONDENSE lv_valor.

  LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_guar>) WHERE posnr = iv_fix.

    " valor novo
    lv_menge_novo = lv_valor.

    "diferença = valor novo - valor antigo
    <fs_guar>-difer = lv_menge_novo - <fs_guar>-zmeng.

    " valor final igual ao valor antigo + valor da diferença
    <fs_guar>-final = <fs_guar>-zmeng + <fs_guar>-difer.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VALIDA_SALDO_FIX
*&---------------------------------------------------------------------*
FORM f_valida_saldo_fixacao.

  DATA lt_guar_sum TYPE TABLE OF ty_var_guar.
  DATA ls_guard TYPE ty_var_guar.
  DATA lv_fixacoes TYPE c LENGTH 100.

  CHECK wg_header-param_espec = 'M' AND gv_redist_fix IS INITIAL.

  CHECK wg_redistribuir = abap_true.

  LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_guar>).

    ls_guard = <fs_guar>.

    CLEAR: ls_guard-zmeng,
           ls_guard-final,
           ls_guard-posnr,
           ls_guard-guar,
           ls_guard-charg_x,
           ls_guard-charg_old,
           ls_guard-charg,
           ls_guard-updated,
           ls_guard-principal.

    COLLECT ls_guard INTO lt_guar_sum.

  ENDLOOP.

  UNASSIGN <fs_guar>.


  BREAK rblima.

  LOOP AT lt_guar_sum ASSIGNING <fs_guar> WHERE difer <> 0.

    SHIFT <fs_guar>-fixacao LEFT DELETING LEADING '0'.

    lv_fixacoes = lv_fixacoes && ` / ` && <fs_guar>-fixacao.

  ENDLOOP.

  CHECK lv_fixacoes IS NOT INITIAL.

  CLEAR tg_msg_ret.
  "tg_msg_ret-msg = `Distribuição entre as fixações não permitida `.
  tg_msg_ret-msg = `Não é permitido realizar redistribuição entre itens de fixações diferentes`.
  tg_msg_ret-aba = c_tab_strip-tab4.
  tg_msg_ret-field = 'FIXACAO'.
  tg_msg_ret-tabix = 1.
  tg_msg_ret-obj = 'GRID2'.

  APPEND tg_msg_ret.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_set_soma_prod_qtde
*&---------------------------------------------------------------------*
FORM f_set_soma_prod_qtde .

  CHECK grid1 IS BOUND.

  CHECK gv_sub_total_fix IS INITIAL.

  CALL METHOD grid1->get_sort_criteria
    IMPORTING
      et_sort = DATA(lt_sort).

  lt_sort = VALUE #(
                ( spos = 1 fieldname = 'FIXACAO' up = abap_true subtot = abap_true )
                "( spos = 1 fieldname = 'ZMENG' up = abap_false subtot = abap_true )
                ).

  CALL METHOD grid1->set_sort_criteria
    EXPORTING
      it_sort                   = lt_sort
    EXCEPTIONS
      no_fieldcatalog_available = 1
      OTHERS                    = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_tratar_search_help
*&---------------------------------------------------------------------*
FORM f_tratar_search_help CHANGING ct_fieldcat TYPE lvc_t_fcat.

  IF wg_header-auart = 'ZUB' AND wg_header-tp_venda = '00392'.

    READ TABLE ct_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)
      WITH KEY tabname = 'TG_ITENS'
               fieldname = 'CHARG'.

    IF sy-subrc EQ 0.

      CLEAR <fs_fieldcat>-ref_field.
      CLEAR <fs_fieldcat>-ref_table.
      CLEAR <fs_fieldcat>-checktable.
      CLEAR <fs_fieldcat>-f4availabl.

      <fs_fieldcat>-reptext = 'Lote/Safra'.

    ENDIF.

  ENDIF.

  " 18.03.2025 -

  READ TABLE ct_fieldcat ASSIGNING <fs_fieldcat>
  WITH KEY tabname = 'TG_ITENS'
           fieldname = 'PMEIN'.

  IF sy-subrc EQ 0.

    CLEAR <fs_fieldcat>-ref_field.
    CLEAR <fs_fieldcat>-ref_table.
    CLEAR <fs_fieldcat>-checktable.
*      CLEAR <fs_fieldcat>-f4availabl.
*
*      <fs_fieldcat>-reptext = 'Lote/Safra'.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_f4_pmein
*&---------------------------------------------------------------------*
FORM f_f4_pmein USING uv_index TYPE int4.


  TYPES: BEGIN OF y_0371,
           "MATKL TYPE MATKL,
           "WGBEZ TYPE WGBEZ,
           mseh3 TYPE mseh3,
           msehl TYPE msehl,
         END OF y_0371.

  DATA lt_ret TYPE TABLE OF ddshretval.
  DATA lt_pmein TYPE TABLE OF y_0371.
  DATA lv_matnr TYPE matnr18.

  READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX uv_index.

  CHECK sy-subrc EQ 0.

  UNPACK <fs_item>-matnr TO lv_matnr.

  DATA(lt_unidade) = zcl_material=>get_instance( )->get_zsdt0371_table_by_matnr( CONV #( lv_matnr ) ).

  CHECK lt_unidade IS NOT INITIAL.

  lt_pmein = CORRESPONDING #( lt_unidade ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'MSEH3'
      value_org        = 'S'
      callback_program = sy-cprog
      callback_form    = 'CALLBACK_F4_PMEIN'
    TABLES
      value_tab        = lt_pmein
      return_tab       = lt_ret
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.

  IF sy-subrc IS INITIAL.
    LOOP AT lt_ret ASSIGNING FIELD-SYMBOL(<fs_ret>).

      IF <fs_item> IS ASSIGNED.
        CASE <fs_ret>-retfield.
          WHEN 'MSEH3'.

            <fs_item>-pmein = <fs_ret>-fieldval.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CALLBACK_F4
*&---------------------------------------------------------------------*
FORM callback_f4_pmein TABLES record_tab STRUCTURE seahlpres
                     CHANGING shlp TYPE shlp_descr
                              callcontrol LIKE ddshf4ctrl.
  DATA:
    ls_intf LIKE LINE OF shlp-interface,
    ls_prop LIKE LINE OF shlp-fieldprop.

  CLEAR: ls_prop-shlpselpos,
         ls_prop-shlplispos.

  REFRESH: shlp-interface.
  ls_intf-shlpfield = 'F0001'.
  ls_intf-valfield  = 'MSEH3'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.
  ls_intf-shlpfield = 'F0002'.
  ls_intf-valfield  = 'MSEHL'.
  ls_intf-f4field   = 'X'.
  APPEND ls_intf TO shlp-interface.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_update_preco_lote
*&---------------------------------------------------------------------*
FORM f_atualiza_lote USING uo_data TYPE REF TO cl_alv_changed_data_protocol.

  IF line_exists( uo_data->mt_good_cells[ fieldname = 'CHARG' ] ).

    LOOP AT uo_data->mt_good_cells  INTO ls_good WHERE fieldname = 'CHARG'.

      READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX ls_good-row_id.

      CHECK sy-subrc EQ 0.

      READ TABLE it_var_guar ASSIGNING FIELD-SYMBOL(<fs_guard>) WITH KEY posnr = <fs_item>-posnr.

      CHECK sy-subrc EQ 0.

      " se o lote inserido é igual ao inicial, então nao houve alteração de lote
      IF ls_good-value = <fs_guard>-charg_old.

        <fs_guard>-charg = ls_good-value.
        <fs_guard>-charg_x = abap_false.
        <fs_guard>-updated = abap_false.

        " se for diferente então é alteração de lote
      ELSE.

        <fs_guard>-charg = ls_good-value.
        <fs_guard>-charg_x = abap_true.

      ENDIF.

    ENDLOOP.

  ELSEIF line_exists( uo_data->mt_good_cells[ fieldname = 'ZMENG' ] ).

    LOOP AT it_var_guar ASSIGNING <fs_guard> WHERE guar = abap_true AND difer > 0.

      LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_princi>)
        WHERE fixacao = <fs_guard>-fixacao
          AND guar = abap_true
          AND difer < 0.


        " se o lote inserido é igual ao inicial, então nao houve alteração de lote
        IF <fs_princi>-charg = <fs_guard>-charg_old.

          "<fs_guard>-charg = ls_good-value.
          <fs_guard>-charg_x = abap_false.
          <fs_guard>-updated = abap_false.

          " se for diferente então é alteração de lote
        ELSE.
          <fs_guard>-charg_x = abap_true.

        ENDIF.


      ENDLOOP.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form calcula_redistribuicao_2
*&---------------------------------------------------------------------*
FORM calcula_redistribuicao_2 USING ut_itens TYPE tty_itens CHANGING cv_saldo TYPE zsdt0053-zmeng.

  CLEAR cv_saldo.

  LOOP AT ut_itens ASSIGNING FIELD-SYMBOL(<fs_item>).
    ADD <fs_item>-zmeng TO cv_saldo.
  ENDLOOP.

  cv_saldo = cv_saldo - wg_redistribuir_titem.

ENDFORM.                    " CALCULA_REDISTRIBUICAO
*&---------------------------------------------------------------------*
*& Form f_atualiza_var_guard
*&---------------------------------------------------------------------*
FORM f_update_preco_lote USING uo_data TYPE REF TO cl_alv_changed_data_protocol.

  DATA lv_charg_princ TYPE charg_d.
  DATA lv_charg_new TYPE charg_d.
  DATA var_valor TYPE c LENGTH 128.
  DATA var_valor_n TYPE c LENGTH 128.
  DATA lt_item_temp TYPE tty_itens.
  DATA lv_saldo TYPE zsdt0053-zmeng.
  DATA lv_valor_n TYPE zsdt0053-zmeng.

  CHECK uo_data->mt_good_cells IS NOT INITIAL.

  lt_item_temp = tg_itens[].

  " localiza o indice da tabela de item
  LOOP AT uo_data->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_changed>)
      WHERE fieldname = 'ZMENG' OR fieldname = 'CHARG'.

    READ TABLE lt_item_temp[] ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX <fs_changed>-row_id.

    IF <fs_changed>-fieldname = 'ZMENG'.

      <fs_item>-zmeng = <fs_changed>-value .

    ENDIF.

    EXIT.
  ENDLOOP.

  CHECK <fs_changed> IS ASSIGNED.

  " verifica se já está zerado o saldo
  PERFORM calcula_redistribuicao_2
    USING lt_item_temp
  CHANGING lv_saldo.

  CHECK lv_saldo IS INITIAL.

  " 25.04.2025 -->
  PERFORM f_corrigir_var_gar_lote.

  " 25.04.2025 --<

  " percorre a tabela temporaria de valores, onde os valores estao guardados,
  " e não seja retirada de valor
  LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_var_guar>)
      WHERE guar = abap_true
        AND charg_x = abap_true
        AND updated = abap_false.

    DATA(lv_idx_linha) = sy-tabix.

    "Pega o item de onde saiu os valores
    LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_linha_prin>)
        WHERE fixacao = <fs_var_guar>-fixacao
          AND guar = abap_true
          AND difer < 0.

      DATA(lv_idx_princ) = sy-tabix.

      EXIT.

    ENDLOOP.

    CHECK <fs_linha_prin> IS ASSIGNED.

    " verifica se teve alteração no lote
    IF <fs_var_guar>-charg_x = abap_true
      " 25.04.2025 - RAMON - 166561 -->
      AND <fs_linha_prin>-charg NE <fs_var_guar>-charg.
      " 25.04.2025 - RAMON - 166561 -->

      " se sim, atualiza na aba preço a retirada da principal

      WRITE <fs_var_guar>-difer TO var_valor LEFT-JUSTIFIED.

      lv_valor_n = <fs_var_guar>-difer * -1.

      WRITE lv_valor_n TO var_valor_n LEFT-JUSTIFIED.

*      IF sy-uname = 'RBLIMA'.

      PERFORM update_bezei_2
         TABLES tg_itens
                c_bezei
                p_bezei
                r_bezei
                n_bezei
                b_bezei
         USING  lv_idx_princ "<- indice da tab de item
                var_valor_n "<- valor a ser preenchido na aba preço
                space "<- indica se a coluna pode ser sobrescrevida, passar 'T' se sim
                abap_false. "<- indicação se vai para tabela zsdt0059_2, passar 'X' se sim

      " lança valor da linha nova
      PERFORM update_bezei_2 TABLES tg_itens
                                  c_bezei
                                  p_bezei
                                  r_bezei
                                  n_bezei
                                  b_bezei
                           USING  lv_idx_linha "<fs_changed>-row_id
                                  var_valor
                                  space
                                  abap_false.

*      ELSE.
*
*        PERFORM update_bezei
*                TABLES tg_itens
*                       c_bezei
*                       p_bezei
*                       r_bezei
*                       n_bezei
*                       b_bezei
*                USING  lv_idx_princ "<- indice da tab de item
*                       var_valor_n "<- valor a ser preenchido na aba preço
*                       space "<- indica se a coluna pode ser sobrescrevida, passar 'T' se sim
*                       abap_false. "<- indicação se vai para tabela zsdt0059_2, passar 'X' se sim
*
*        " lança valor da linha nova
*        PERFORM update_bezei TABLES tg_itens
*                                    c_bezei
*                                    p_bezei
*                                    r_bezei
*                                    n_bezei
*                                    b_bezei
*                             USING  lv_idx_linha "<fs_changed>-row_id
*                                    var_valor
*                                    space
*                                    abap_false.
*
*      ENDIF.




      " sinaliza que já houve a alteração na aba preço
      <fs_var_guar>-updated = abap_true.

    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_tabela_dinamica
*&---------------------------------------------------------------------*
FORM f_get_tabela_dinamica CHANGING ct_tab TYPE zsdc_preco_din_zsdt0062.

  CHECK <fs_table>[] IS ASSIGNED.

  ct_tab[] = CORRESPONDING #( <fs_table> ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_SET_tabela_dinamica
*&---------------------------------------------------------------------*
FORM f_set_tabela_dinamica USING ut_tab TYPE zsdc_preco_din_zsdt0062.

  CHECK ut_tab[] IS NOT INITIAL.

  <fs_table>[] = CORRESPONDING #( ut_tab[] ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_preenche_bezei_preco
*&---------------------------------------------------------------------*
FORM f_preenche_bezei_preco USING uv_sigla TYPE c
                                  uv_redist TYPE c
                                  uv_tp_row TYPE c
                                  uv_valor_bezei TYPE any
                                  us_item TYPE ty_itens
                                  uv_frame TYPE char20
                                  us_bezei_range TYPE ty_bezei_tab
                         CHANGING ct_tab TYPE zsdc_preco_din_zsdt0062.

  DATA lv_preco_linha TYPE c LENGTH 30.
  DATA lv_valor_char  TYPE c LENGTH 128.
  DATA lv_posicao TYPE sy-tabix.
  DATA lv_cbot(30)        TYPE c.
  DATA lv_monat          TYPE c LENGTH 2.
  DATA lv_safra         TYPE c LENGTH 4.
  DATA lv_cbot_redist(30) TYPE c.
  DATA lv_bezei TYPE bezei30.
  DATA lv_ultimo_bezei TYPE bezei30.

  READ TABLE ct_tab ASSIGNING FIELD-SYMBOL(<fs_preco>)
    WITH KEY posnr = us_item-fixacao
             bezei = uv_frame.

  CHECK sy-subrc EQ 0.

  lv_preco_linha = <fs_preco>-preco.

  LOOP AT ct_tab INTO DATA(ls_preco)
       WHERE posnr = us_item-fixacao
         AND bezei IN us_bezei_range
         AND ( qtdfixada = '0,00'
         OR tp_row = 'T' ).

    lv_posicao = sy-tabix.

    PERFORM f_get_last_bezei USING ls_preco-bezei CHANGING lv_bezei.

    "CHECK uv_tp_row = ls_preco-tp_row.

    READ TABLE ct_tab ASSIGNING FIELD-SYMBOL(<fs_last>)
      WITH KEY posnr = ls_preco-posnr
               bezei = lv_bezei.

    IF sy-subrc EQ 0.

      lv_valor_char = <fs_last>-valdt.
      lv_cbot = <fs_last>-cbot.
      lv_monat = <fs_last>-monat.
      lv_safra = <fs_last>-safra.

    ENDIF.

    ls_preco-cbot = lv_cbot.
    ls_preco-qtdfixada = uv_valor_bezei.
    ls_preco-posnr1 = us_item-posnr.
    ls_preco-preco = lv_preco_linha.
    ls_preco-valdt = sy-datum.
    ls_preco-monat = lv_monat.
    ls_preco-safra = lv_safra.

    IF wg_redistribuir = abap_true AND uv_redist = abap_true.

      PERFORM f_change_bezei_redist_2 USING uv_sigla us_item-fixacao CHANGING lv_ultimo_bezei.

      ls_preco-redist = abap_true.

      IF lv_cbot_redist IS INITIAL AND lv_cbot IS NOT INITIAL.
        lv_cbot_redist = lv_cbot.
      ENDIF.

      ls_preco-bezei = lv_ultimo_bezei.
      ls_preco-redist = abap_true.
      ls_preco-posnr = us_item-fixacao.
      ls_preco-cbot = lv_cbot_redist.
      ls_preco-item_key = '999'.
      ls_preco-nivel = gv_ultimo_nvl.
      ls_preco-tp_row = uv_tp_row.

      APPEND ls_preco TO ct_tab.

    ELSE.

      IF wg_acao = c_modif_qtd.

        ls_preco-tp_row = uv_tp_row.

      ENDIF.

      MODIFY ct_tab FROM ls_preco INDEX lv_posicao.

    ENDIF.

    EXIT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_last_bezei
*&---------------------------------------------------------------------*
FORM f_get_last_bezei USING uv_bezei TYPE bezei30
                   CHANGING cv_bezei TYPE bezei30.

  DATA lv_letra TYPE c LENGTH 1.
  DATA lv_num TYPE numc_3.

  CHECK uv_bezei IS NOT INITIAL.

  lv_letra = uv_bezei.

  lv_num = uv_bezei+1.

  IF lv_num CA '0123456789'.

    IF lv_num > '1'.
      SUBTRACT 1 FROM lv_num.
    ENDIF.

    SHIFT lv_num LEFT DELETING LEADING '0'.

    CONDENSE lv_num NO-GAPS.

  ENDIF.

  cv_bezei = lv_letra && lv_num.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_corrigir_var_gar_lote
*&---------------------------------------------------------------------*
FORM f_corrigir_var_gar_lote .

  " valores adicionados
  LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_var_guar>)
      WHERE guar = abap_true
        AND final > 0.

    " lugar de onde saiu, ou seja, diferença ficou menor que o original
    LOOP AT it_var_guar ASSIGNING FIELD-SYMBOL(<fs_linha_prin>)
      WHERE fixacao = <fs_var_guar>-fixacao
        AND guar = abap_true
        AND difer < 0.
      EXIT.
    ENDLOOP.

    CHECK <fs_linha_prin> IS ASSIGNED.

    IF <fs_var_guar>-charg <> <fs_linha_prin>-charg.
      <fs_var_guar>-charg_x = abap_true.
    ELSE.
      <fs_var_guar>-charg_x = abap_false.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valida_lote_obg
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_valida_lote_obg .


  IF  sy-ucomm EQ c_show_msgre.

    SELECT SINGLE lote_obg
      FROM zsdt0057
      INTO gv_lib_lote
      WHERE tp_venda EQ wg_header-tp_venda.

  ENDIF.


  LOOP AT tg_itens INTO DATA(ls_itens).

    "não libera lote vazio
    CHECK gv_lib_lote IS INITIAL AND wg_header-param_espec = 'M' AND ls_itens-charg IS INITIAL.

    CLEAR tg_msg_ret.
    "tg_msg_ret-msg = `Distribuição entre as fixações não permitida `.

    tg_msg_ret-msg = |É obrigatório o preenchimento do campo Lote. Linha: | && sy-tabix.
    tg_msg_ret-aba = c_tab_strip-tab4.
    tg_msg_ret-field = 'CHARG'.
    tg_msg_ret-tabix = 1.
    tg_msg_ret-obj = 'GRID2'.

    APPEND tg_msg_ret.


  ENDLOOP.





ENDFORM.
