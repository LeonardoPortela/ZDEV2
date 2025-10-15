*&---------------------------------------------------------------------*
*&  Include           ZFIR0092_TOP
*&---------------------------------------------------------------------*


TYPES: BEGIN OF ty_saida,
         bsart        TYPE ekko-bsart,
         ebeln        TYPE ekko-ebeln,
         lifnr        TYPE ekko-lifnr,
         name1_forn   TYPE lfa1-name1,
         matnr        TYPE ekpo-matnr,
         maktx        TYPE makt-maktx,
         ebelp        TYPE ekpo-ebelp,
         werks        TYPE ekpo-werks,
         branch       TYPE zsdt_depara_cen-centro_real,
         lgort        TYPE ekpo-lgort,
         charg        TYPE eket-charg,
         aviso_rec    TYPE ekpo-lgort,
         qtde         TYPE ekpo-lgort,
         data_mov     TYPE ekpo-lgort,
         tp_frete     TYPE ekpo-lgort,
         itinerario   TYPE ekpo-lgort,
         agente_frete TYPE ekpo-lgort,
         vlr_frete    TYPE ekpo-lgort,
         doc_transp   TYPE c LENGTH 4,
         doc_custo    TYPE ekpo-lgort,
         ov_serv      TYPE ekpo-lgort,
         fatura_serv  TYPE ekpo-lgort,
         dacte        TYPE ekpo-lgort,
         xblnr        TYPE ekes-xblnr,
         vbeln        TYPE ekes-vbeln, "LES - Ajuste Preenchimento ZLES0113 US 168927 - WPP --->>
       END OF ty_saida,

       BEGIN OF ty_saida_aviso,
         icon(4),
         placa_cav     TYPE zlest0108-placa_cav,
         bsart         TYPE ekko-bsart,
         ebeln         TYPE ekko-ebeln,
         lifnr         TYPE ekko-lifnr,
         matnr         TYPE ekpo-matnr,
         ebelp         TYPE ekpo-ebelp,
         werks         TYPE ekpo-werks,
         branch        TYPE zsdt_depara_cen-centro_real,
         lgort         TYPE ekpo-lgort,
         charg         TYPE lips-charg,
         vbeln         TYPE likp-vbeln,
         btgew         TYPE likp-btgew,
         erdat         TYPE likp-erdat,
         tp_frete      TYPE ekpo-lgort,
         route         TYPE likp-route,
         agente_frete  TYPE zlest0108-agente_frete,
         vlr_frete     TYPE ekpo-lgort,
         transp(15),
         doccus(10),
         ovserv(10),
         fatserv(10),
         dacte(10),
         damdfe(10),
         st_proc       TYPE zlest0108-st_proc,
         shtyp         TYPE zsdt0011-shtyp,
         kbetr         TYPE zlest0108-kbetr,
         cont_fre      TYPE i,
         inco1         TYPE likp-inco1,
         line_color(4) TYPE c, "Used to store row color attributes
         color_cell    TYPE lvc_t_scol,  " Cell color
         style         TYPE lvc_t_styl,
         add01         TYPE a915-add01,
         verur         TYPE likp-verur,
       END OF ty_saida_aviso,

       BEGIN OF ty_dados_aviso,
         ebeln      TYPE zlest0109-ebeln,
         lifnr      TYPE zlest0109-lifnr,
         ebelp      TYPE zlest0109-ebelp,
         matnr      TYPE zlest0109-matnr,
         qtde_aviso TYPE zlest0109-qtde_aviso,
         peso_bruto TYPE zlest0109-peso_bruto,
         peso_tara  TYPE zlest0109-peso_tara,
         unidade    TYPE zlest0109-unidade,
         vbeln      TYPE zlest0109-vbeln,
         werks      TYPE zlest0109-werks,
         lgort      TYPE zlest0109-lgort,
         charg      TYPE zlest0109-charg,
       END OF ty_dados_aviso,

       BEGIN OF ty_dados_veic,
         pc_veiculo       TYPE zlest0002-pc_veiculo,
         cd_cidade        TYPE zlest0002-cd_cidade,
         cd_uf            TYPE zlest0002-cd_uf,
         cd_renavam       TYPE zlest0002-cd_renavam,
         proprietario     TYPE zlest0002-proprietario,
         des_proprietario TYPE lfa1-name1,
         tp_veiculo       TYPE zlest0002-tp_veiculo,
         cnpj_cpf_prop    TYPE c LENGTH 14,
       END OF ty_dados_veic,

       BEGIN OF ty_tvtk,
         shtyp TYPE tvtk-shtyp,
         laufk TYPE tvtk-laufk,
       END OF ty_tvtk,

       BEGIN OF ty_t161t,
         bsart TYPE t161t-bsart,
         batxt TYPE t161t-batxt,
       END OF ty_t161t,

       BEGIN OF ty_tvakt,
         auart TYPE tvakt-auart,
         bezei TYPE tvakt-bezei,
       END OF ty_tvakt,

       BEGIN OF ty_vbkd,
         vbeln TYPE vbkd-vbeln,
         inco1 TYPE vbkd-inco1,
       END OF ty_vbkd,

       BEGIN OF ty_qtde_aviso,
         peso_bruto1 TYPE zlest0109-qtde_aviso,
         peso_tara1  TYPE zlest0109-qtde_aviso,
         peso_total1 TYPE zlest0109-qtde_aviso,
         peso_bruto2 TYPE zlest0109-qtde_aviso,
         peso_tara2  TYPE zlest0109-qtde_aviso,
         peso_total2 TYPE zlest0109-qtde_aviso,
       END OF ty_qtde_aviso,


       BEGIN OF ty_ordem_car,
         safra(4)     TYPE c,
         nr_ordem(14) TYPE c,
       END OF ty_ordem_car.

TYPES: BEGIN OF ty_dados_transp.
         INCLUDE TYPE zlest0108.
TYPES: END OF ty_dados_transp.

TYPES: BEGIN OF ty_dados_nf.
         INCLUDE TYPE zlest0110.
TYPES: END OF ty_dados_nf.

TYPES: BEGIN OF ty_buscar_aviso,
         ebeln           TYPE ekes-ebeln, "Nr. Pedido
         ebelp           TYPE ekes-ebelp, "Item Pedido
         vbeln           TYPE ekes-vbeln, "Nr. Aviso
         matnr           TYPE lips-matnr, "Material
         xblnr           TYPE ekes-xblnr, "Nota
         posnr           TYPE lips-posnr, "Item Aviso
         lfimg           TYPE lips-lfimg, "Qtdade
         unidade         TYPE lips-meins, "Unidade
         qtde_aviso      TYPE lips-brgew, "Peso Bruto
         eindt           TYPE ekes-eindt, "Data Aviso
         ezeit           TYPE ekes-ezeit, "Hora Aviso

         werks           TYPE lips-werks,
         lgort           TYPE lips-lgort,
         charg           TYPE lips-charg,
         gewei           TYPE lips-gewei,
         vgbel           TYPE lips-vgbel,
         vgpos           TYPE lips-vgpos,

         lifnr           TYPE likp-lifnr,
         lifex           TYPE  likp-lifex,

         cod_remetente   TYPE vbpa-lifnr,
         cod_dest_merc   TYPE vbpa-kunnr,
         cod_loc_coleta  TYPE vbpa-lifnr,
         cod_loc_entrega TYPE vbpa-kunnr,

       END OF ty_buscar_aviso.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: wa_msg TYPE bdcmsgcoll.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS:
         c_x               TYPE c VALUE 'X'.

*&--------------------------------------------------------------------&*
*& Variaveis de Comando                                               &*
*&--------------------------------------------------------------------&*

DATA: ok_code_0102 TYPE syucomm.


*-------------------------------------------------------------------
* Tabelas Internas and Work Areas.
*-------------------------------------------------------------------
DATA: it_saida           TYPE TABLE OF ty_saida,
      wa_saida           TYPE ty_saida,
      it_saida_aviso     TYPE TABLE OF ty_saida_aviso,
      wa_saida_aviso     TYPE ty_saida_aviso,
      it_dados_aviso     TYPE TABLE OF ty_dados_aviso,
      wa_dados_aviso     TYPE ty_dados_aviso,
      it_dados_veic      TYPE TABLE OF ty_dados_veic,
      wa_dados_veic      TYPE ty_dados_veic,
      it_dados_transp    TYPE TABLE OF ty_dados_transp,
      wa_dados_transp    TYPE ty_dados_transp,
      it_dados_nf        TYPE TABLE OF ty_dados_nf,
      wa_dados_nf        TYPE ty_dados_nf,
*
      it_ekko            TYPE TABLE OF ekko,
      wa_ekko            TYPE ekko,
      it_ekpo            TYPE TABLE OF ekpo,
      wa_ekpo            TYPE ekpo,
      it_ekbe            TYPE TABLE OF ekbe,
      wa_ekbe            TYPE ekbe,
      it_eket            TYPE TABLE OF eket,
      wa_eket            TYPE eket,
      it_makt            TYPE TABLE OF makt,
      wa_makt            TYPE makt,
      it_lfa1            TYPE TABLE OF lfa1,
      wa_lfa1            TYPE lfa1,
      it_kna1            TYPE TABLE OF kna1,
      wa_kna1            TYPE kna1,
      wa_mara            TYPE mara,
      wa_likp            TYPE likp,
      wa_lips            TYPE lips,
      it_zlest0109       TYPE TABLE OF zlest0109,
      wa_zlest0109       TYPE zlest0109,
      it_zlest0110       TYPE TABLE OF zlest0110,
      wa_zlest0110       TYPE zlest0110,
      it_zlest0108       TYPE TABLE OF zlest0108,
      wa_zlest0108       TYPE zlest0108,
      it_zlest0108_aux   TYPE TABLE OF zlest0108,
      wa_zlest0108_aux   TYPE zlest0108,
      it_zlest0002       TYPE TABLE OF zlest0002,
      wa_zlest0002       TYPE zlest0002,
      wa_zsdt0011        TYPE zsdt0011,
      it_a900            TYPE TABLE OF a900,
      wa_a900            TYPE a900,
      it_a910            TYPE TABLE OF a910,
      wa_a910            TYPE a910,
      it_a911            TYPE TABLE OF a911,
      wa_a911            TYPE a911,
      it_a915            TYPE TABLE OF a915,
      wa_a915            TYPE a915,
      it_a918            TYPE TABLE OF a918,
      wa_a918            TYPE a918,
      it_a919            TYPE TABLE OF a919,
      wa_a919            TYPE a919,
      it_a942            TYPE TABLE OF a942,
      wa_a942            TYPE a942,
      it_konp            TYPE TABLE OF konp,
      wa_konp            TYPE konp,
      it_zsdt_depara_cen TYPE TABLE OF zsdt_depara_cen,
      wa_zsdt_depara_cen TYPE zsdt_depara_cen,
      it_zlest0111       TYPE TABLE OF zlest0111 WITH HEADER LINE,
      wa_zlest0111       TYPE zlest0111,
      wa_tvakt           TYPE ty_tvakt,
      wa_t161t           TYPE ty_t161t,
      wa_vbkd            TYPE ty_vbkd,
      wa_tvtk            TYPE ty_tvtk,
      wa_zcte_ciot       TYPE zcte_ciot,
      wa_estrutura       TYPE ty_estrutura,
      estrutura          TYPE TABLE OF ty_estrutura,
      wa_qtde_aviso      TYPE ty_qtde_aviso,
      wa_trolz           TYPE trolz,
      wa_tvro            TYPE tvro,
      vg_emitente_nfe    TYPE char01,
      vg_message         TYPE char255,
      it_buscar_aviso    TYPE TABLE OF ty_buscar_aviso.
*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata.

DATA: it_sel_rows     TYPE lvc_t_row,
      wa_sel_rows     TYPE lvc_s_row,
      it_selectedcell TYPE lvc_t_cell,
      wa_selectedcell TYPE lvc_s_cell,
      it_color        TYPE TABLE OF lvc_s_scol,
      wa_color        TYPE lvc_s_scol.

DATA: vg_data_aviso TYPE sy-datum,
      vg_det_peso1  TYPE c,
      vg_det_peso2  TYPE c.

CONSTANTS: aviso_rec_0103 LIKE sy-dynnr VALUE '0103',
           aviso_rec_0104 LIKE sy-dynnr VALUE '0104',
           aviso_rec_0105 LIKE sy-dynnr VALUE '0105',
           aviso_rec_0106 LIKE sy-dynnr VALUE '0106'.

DATA: aviso_dynnr_000  LIKE sy-dynnr.

CONTROLS: info_aviso_tab    TYPE TABSTRIP.

CONSTANTS: av_tb01 TYPE c LENGTH 7 VALUE 'AV_TB01',
           av_tb02 TYPE c LENGTH 7 VALUE 'AV_TB02',
           av_tb03 TYPE c LENGTH 7 VALUE 'AV_TB03',
           av_tb04 TYPE c LENGTH 7 VALUE 'AV_TB04'.

"Variaveis Dados NF
DATA: cte_text_modelo        TYPE c LENGTH 60,
      cte_text_cliente_nome  TYPE c LENGTH 35,
      cte_text_cliente_cgc   TYPE c LENGTH 18,
      cte_text_cfop          TYPE j_1bcfotxt,
      cte_text_numero        TYPE j_1bnfnum9,
      cte_text_cv_uf         TYPE j_1bregio,
      cte_text_cv_ano        TYPE j_1byear,
      cte_text_cv_mes        TYPE j_1bmonth,
      cte_text_cv_cnpj       TYPE j_1bstcd1,
*---> 20.06.2023 - Migração S4 - DG
      "      cte_text_cv_mod        TYPE char02,
      cte_text_cv_mod        TYPE zchar02,
*<--- 20.06.2023 - Migração S4 - DG
      cte_text_cv_serie      TYPE j_1bseries,
      cte_text_motorista_cpf TYPE c LENGTH 14,
      cte_text_produto       TYPE maktx.

"Variaveis Dados Parceiros
DATA: wa_xnome_emit    TYPE c LENGTH 60,
      wa_cnpj_emit     TYPE stcd1,
      wa_ie_emit       TYPE stcd3,
      wa_xnome_rem     TYPE c LENGTH 60,
      wa_cnpj_rem      TYPE stcd1,
      wa_ie_rem        TYPE stcd3,
      wa_xnome_dest    TYPE c LENGTH 60,
      wa_cnpj_dest     TYPE stcd1,
      wa_ie_dest       TYPE stcd3,
      wa_xnome_cole    TYPE c LENGTH 60,
      wa_cnpj_cole     TYPE stcd1,
      wa_ie_cole       TYPE stcd3,
      wa_xnome_entrega TYPE c LENGTH 60,
      wa_cnpj_entrega  TYPE stcd1,
      wa_ie_entrega    TYPE stcd3.

"Variaveis Dados Parceiros
DATA: wa_xnome_moto TYPE c LENGTH 60,
      wa_cpf_moto   TYPE stcd2.

DATA: wa_info_c TYPE kna1,
      wa_info_k TYPE lfa1.


DATA: wg_exit(1)       TYPE c,
      wg_save(1)       TYPE c,
      wg_documento(10),
      w_answer(1),
      wl_erro(1),
      wl_ordemt        TYPE thead-tdname,
      vl_delivery      TYPE bapishpdelivnumb-deliv_numb,
      v_docnum         TYPE j_1bnfe_active-docnum,
      vl_vbeln         TYPE vbfa-vbeln,
      vl_fksto         TYPE vbrk-fksto,
      vl_mjahr         TYPE vbfa-mjahr,
      vl_refkey        TYPE j_1bnflin-refkey,
      vl_fknum         TYPE zlest0108-fknum,
      vl_ov_frete      TYPE zlest0108-ov_frete,
      v_tknum          TYPE vttk-tknum,
      vl_fatura_frete  TYPE zlest0108-fatura_frete,
      vl_bukrs         TYPE j_1bnfdoc-bukrs,
      vl_docnum        TYPE j_1bnflin-docnum,
      vl_message       TYPE char600,
      vl_ponteiro      TYPE zlest0111-cont,
      wl_mode(1),
      vg_view_transp   TYPE c.


DATA: t_success         TYPE STANDARD TABLE OF bapivbrksuccess WITH HEADER LINE,
      w_success         LIKE LINE OF t_success,
      t_billing         TYPE STANDARD TABLE OF bapivbrk WITH HEADER LINE,
      w_billing         LIKE LINE OF t_billing,
      wa_setleaf        TYPE setleaf,
      t_textdatain      TYPE STANDARD TABLE OF bapikomfktx WITH HEADER LINE,
      w_textdatain      LIKE LINE OF t_textdatain,
      x_header          TYPE thead,
      it_lines          TYPE STANDARD TABLE OF tline WITH HEADER LINE,
      wa_lines          LIKE LINE OF it_lines,
      tl_bapiparex      TYPE TABLE OF bapiparex,
      sl_bapiparex      TYPE bapiparex,
      wl_orderheaderin  TYPE bapisdh1,
      wl_orderheaderinx TYPE bapisdh1x,
      wl_bape_vbak      TYPE bape_vbak,
      wl_bape_vbakx     TYPE bape_vbakx,
      t_itemdata        TYPE TABLE OF bapishipmentitem,
      st_itemdata       TYPE bapishipmentitem,
      t_return_vt       LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
      zid               TYPE thead-tdid,
      zname             TYPE thead-tdname,
      t_return          TYPE STANDARD TABLE OF bapireturn1 WITH HEADER LINE.
