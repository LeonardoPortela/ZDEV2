*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Tavares                                         &*
*& Data.....: 24/12/2013                                              &*
*& Descrição: Autorização Liberação Embarque - INSUMOS                &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
"......xx........xx
REPORT  zsdr0038.

*INCLUDE <CL_ALV_CONTROL>.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis, kkblo.
*----------------------------------------------------------------------*
* ESTRUTURA
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_zsdt0082,
         nro_sol            TYPE zsdt0082-nro_sol,
         seq                TYPE zsdt0082-seq,
         seq_lib            TYPE zsdt0082-seq_lib,
         vbeln              TYPE zsdt0082-vbeln,
         posnr              TYPE zsdt0082-posnr,
         vkorg              TYPE zsdt0082-vkorg,
         spart              TYPE zsdt0082-spart,
         vkgrp              TYPE zsdt0082-vkgrp,
         vkbur              TYPE zsdt0082-vkbur,
         auart              TYPE zsdt0082-auart,
         dt_sol             TYPE zsdt0082-dt_sol,
         qte_sol            TYPE zsdt0082-qte_sol,
         usuario_sol        TYPE zsdt0082-usuario_sol,
         dt_liber           TYPE zsdt0082-dt_liber,
         usuario_lib        TYPE zsdt0082-usuario_lib,
         qte_lib            TYPE zsdt0082-qte_lib,
         dt_entrega         TYPE zsdt0082-dt_entrega,
         status             TYPE zsdt0082-status,
         dt_canc            TYPE zsdt0082-dt_canc,
         user_canc          TYPE zsdt0082-user_canc,
         nr_rot             TYPE zsdt0082-nr_rot,
         werks              TYPE zsdt0082-werks,
         desmembrar         TYPE zsdt0082-desmembrar,       "FF #145609
         route_ov           TYPE zsdt0082-route_ov,         "FF #145609
         route_iti          TYPE zsdt0082-route_iti,        "FF #145609
         tp_oper            TYPE zsdt0082-tp_oper,                 "*-CS2025000249-16.06.2025-#182039-JT-inicio
         bloqueio           TYPE zsdt0082-bloqueio,                "*-CS2025000249-16.06.2025-#182039-JT-inicio
         origem_estoque     TYPE zsdt0082-origem_estoque, "// wbarbosa 28/08/25  US-169490
         integrou_werks     TYPE zsdt0082-integrou_werks,          "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_vkbur     TYPE zsdt0082-integrou_vkbur,          "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_kunnr     TYPE zsdt0082-integrou_kunnr,          "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_nr_rot    TYPE zsdt0082-integrou_nr_rot,         "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_nr_rot_pc TYPE zsdt0082-integrou_nr_rot_pc,      "*-CS2025000249-01.09.2025-#189440-JT-inicio
       END OF ty_zsdt0082,

       BEGIN OF ty_vbak,
         vbeln TYPE vbak-vbeln,
         kunnr TYPE vbak-kunnr,
         vkbur TYPE vbak-vkbur,
         spart TYPE vbak-spart,
       END OF ty_vbak,
       "FF #145609 - inicio
       BEGIN OF ty_vbpa,
         vbeln TYPE vbpa-vbeln,
         lzone TYPE vbpa-lzone,
       END OF ty_vbpa,

       BEGIN OF ty_trolz,
         azone TYPE trolz-azone,
         lzone TYPE trolz-lzone,
         route TYPE trolz-route,
       END OF ty_trolz,
       "FF #145609 - fim

       BEGIN OF ty_vbap,
         vbeln  TYPE vbap-vbeln,
         vbelv  TYPE vbap-vbelv,
         matnr  TYPE vbap-matnr,
         arktx  TYPE vbap-arktx,
         matkl  TYPE vbap-matkl,
         kwmeng TYPE vbap-kwmeng,
         vrkme  TYPE vbap-vrkme,
         werks  TYPE vbap-werks,
         netpr  TYPE vbap-netpr,
         waerk  TYPE vbap-waerk,
         posnr  TYPE vbap-posnr,
         meins  TYPE vbap-meins,
         lgort  TYPE vbap-lgort,
         route  TYPE vbap-route,                            "FF #145609
       END OF ty_vbap,

       BEGIN OF ty_vbkd,
         vbeln TYPE vbkd-vbeln,
         posnr TYPE vbkd-posnr,
         inco1 TYPE vbkd-inco1,
         inco2 TYPE vbkd-inco2,
       END OF ty_vbkd,


       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         wrkst TYPE mara-wrkst,
         mtart TYPE mtart,
         spart TYPE spart,  "*-CS2025000249-01.09.2025-#189440-JT
       END OF ty_mara,

       BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         ort01 TYPE kna1-ort01,
         regio TYPE kna1-regio,
         stcd1 TYPE kna1-stcd1,
         stcd2 TYPE kna1-stcd2,
         stcd3 TYPE kna1-stcd3,
         stkzn TYPE kna1-stkzn,
       END OF ty_kna1,

       BEGIN OF ty_vbfa,
         vbelv   TYPE vbfa-vbelv,
         posnv   TYPE vbfa-posnv,
         vbtyp_n TYPE vbfa-vbtyp_n,
         vbtyp_v TYPE vbfa-vbtyp_v,
         vbeln   TYPE vbfa-vbeln,
         posnn   TYPE vbfa-posnn,
         rfmng   TYPE vbfa-rfmng,
       END OF ty_vbfa,

       BEGIN OF ty_saida,
         selecao            TYPE char01, "*-CS2025000249-01.09.2025-#189440-JT-inicio
         nro_sol            TYPE zsdt0082-nro_sol,
         seq                TYPE zsdt0082-seq,
         dt_sol             TYPE zsdt0082-dt_sol, "SMC #121425
         spart              TYPE vbak-spart,
         kunnr              TYPE vbak-kunnr,
         name1              TYPE kna1-name1,
         vkbur              TYPE vbak-vkbur,
         vbeln              TYPE vbap-vbeln,
         posnr              TYPE vbap-posnr,
         matnr              TYPE vbap-matnr,
         matkl              TYPE vbap-matkl,  "*-#187270-07.08.2025-JT-inicio
         arktx              TYPE vbap-arktx,
         wrkst              TYPE mara-wrkst,
         meins              TYPE vbap-meins,
         werks              TYPE vbap-werks,
         inco1              TYPE vbkd-inco1,
         kwmeng             TYPE vbap-kwmeng,
         qte_fat            TYPE vbfa-rfmng,
         qte_sol            TYPE vbap-kwmeng,
         qte_sdo_lib        TYPE vbap-kwmeng,
         qte_sdo_fat        TYPE vbap-kwmeng,
         qte_sdo_sol        TYPE vbap-kwmeng,
         observacao(500),
         roteiro(500),
         vkorg              TYPE vbak-vkorg,
         dt_entrega         TYPE zsdt0082-dt_entrega,
         mtart              TYPE mtart,
         nr_rot             TYPE zsdt0132-nr_rot,
         desroteiro         TYPE char255,
         safra              TYPE zsdt0040-safra,
         cultura            TYPE zsdt0040-cultura,
         vkbur_fat          TYPE zsdt0040-vkbur,    "Centro faturamento
         name1_fat          TYPE t001w-name1, "descrição filial
         lgort              TYPE mard-lgort, "Deposito
         cod_regional       TYPE zsdt0270-cod_regional, "Regional
         regional           TYPE zsdt0270-regional, "Regional
*#133297 #133302 - Inclusão de campos  | ITSOUZA
         regio              TYPE kna1-regio,
         ort01              TYPE kna1-ort01,
         cpf_cnpj           TYPE kna1-stcd1,
         stcd3              TYPE kna1-stcd3,
         rot_desc           TYPE zsdt0132-rot_desc,
         iti_ov             TYPE vbap-route,                "FF #145609
         iti_rote           TYPE trolz-route,               "FF #145609
         "FF #178787 - inicio
         observacao_ve(500),
         t_color            TYPE char4,
         venda_esp          TYPE zsds093-venda_esp,
         "FF #178787 - fim
         tp_oper            TYPE zsdt0082-tp_oper,                 "*-CS2025000249-16.06.2025-#182039-JT-inicio
         integrou_werks     TYPE icon_d,                           "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_vkbur     TYPE icon_d,                           "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_kunnr     TYPE icon_d,                           "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_nr_rot    TYPE icon_d,                           "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_nr_rot_pc TYPE icon_d,                           "*-CS2025000249-01.09.2025-#189440-JT-inicio
       END OF ty_saida,

       BEGIN OF ty_textos_aux,
         nro_sol TYPE zsdt0082-nro_sol,
         seq     TYPE zsdt0082-seq,
         id      TYPE thead-tdid,
       END OF ty_textos_aux.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


TYPES: BEGIN OF ty_aux,
         kunnr                TYPE vbak-kunnr,
         name1                TYPE kna1-name1,
         vkbur                TYPE vbak-vkbur,
         vbeln                TYPE vbap-vbeln,
         posnr                TYPE vbap-posnr,
         spart                TYPE vbak-spart,     "*-US191954-30.09.2025-#191954-JT
         matnr                TYPE vbap-matnr,
         arktx                TYPE vbap-arktx,
         wrkst                TYPE mara-wrkst,
         meins                TYPE vbap-meins,
         qte_sol              TYPE vbap-kwmeng,
         qte_sol2             TYPE vbap-kwmeng,
         nro_sol              TYPE zsdt0082-nro_sol,
         seq                  TYPE zsdt0082-seq,
         dt_sol               TYPE zsdt0082-dt_sol, "SMC #121425
         vkorg                TYPE vbak-vkorg,
         dt_entrega           TYPE zsdt0082-dt_entrega,
         werks                TYPE zsdt0082-werks,
         mtart                TYPE mtart,
         mark,
         nr_rot               TYPE zsdt0132-nr_rot,
         inco1                TYPE vbkd-inco1,
         lgort                TYPE mard-lgort,
         qte_sdo_lib          TYPE vbap-kwmeng,             "FF #169501
         ort01                TYPE kna1-ort01,              "FF #169501
         regio                TYPE kna1-regio,              "FF #169501
         "FF #178787 - inicio
         venda_esp            TYPE zde_prioridade,
         prioridade           TYPE zsdt0082-prioridade,
         flexibilidade        TYPE zsdt0082-flexibilidade,
         carga_auto           TYPE char1,
         observacao_ve(256),
         transf_no_fornecedor TYPE zde_transf_no_fornecedor,
         nr_rot_pc            TYPE zsdt0082-nr_rot_pc,
         desc_rot(30),
         "FF #178787 - fim

         lifnr_pc             TYPE lfa1-lifnr,                       "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
         tp_oper              TYPE zsdt0082-tp_oper,                 "*-CS2025000249-16.06.2025-#182039-JT-inicio
         tp_oper_desc         TYPE char50,                           "*-CS2025000249-16.06.2025-#182039-JT-inicio
       END OF ty_aux.

TYPES: BEGIN OF ty_hist.
         INCLUDE STRUCTURE zsdt0082.
TYPES: desc_status(30),
         qtd_vinc        TYPE lfimg,
         qtd_new         TYPE lfimg,
         qte_lib_back    TYPE lfimg,
         status_back     TYPE zsded045,
         integra_safra   TYPE char01,
         mark.
TYPES END OF ty_hist.

*-US191954-30.09.2025-#191954-JT-inicio
TYPES: BEGIN OF ty_centros_defens,
         werks TYPE werks_d,
         matnr TYPE matnr.
TYPES: END OF ty_centros_defens.
*-US191954-30.09.2025-#191954-JT-fim

TYPES: BEGIN OF ty_zsdt,
         vbeln         TYPE zsdt0041-vbeln,
         doc_simulacao TYPE zsdt0090-doc_simulacao,
       END OF ty_zsdt.

TYPES: BEGIN OF ty_zsdt0090,
         vbeln         TYPE zsdt0090-vbeln,
         doc_simulacao TYPE zsdt0090-doc_simulacao,
       END OF ty_zsdt0090.

TYPES: BEGIN OF ty_zsdt0041,
         vbeln         TYPE zsdt0041-vbeln,
         doc_simulacao TYPE zsdt0041-doc_simulacao,
       END OF ty_zsdt0041,

       BEGIN OF ty_popup,                                   "FF #145609
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
         vkorg TYPE t001w-vkorg,
       END OF ty_popup.

*FF #171337 - inicio
TYPES: BEGIN OF ty_bloqueio,
         vkorg       TYPE vkorg,    " Organização de vendas
         spart       TYPE spart,    " Setor de atividade
         werks       TYPE werks_d,    " Centro
         matkl       TYPE matkl,
         lock_failed TYPE char1,
       END OF ty_bloqueio.

TYPES: BEGIN OF ty_saida_exec,
         inco1    TYPE zsdt0041-werks,
         spart    TYPE zsdt0041-spart,
         auart    TYPE zsdt0041-auart,
         werks    TYPE zsdt0041-werks,
         vbeln    TYPE vbak-vbeln,
         msg(255),
       END OF ty_saida_exec.

*FF #171337 - fim

DATA: t_aux    TYPE TABLE OF ty_aux,
      t_aux_2  TYPE TABLE OF ty_aux,
      wa_aux   TYPE ty_aux,
      w_stable TYPE lvc_s_stbl    VALUE 'XX'.

DATA: lc_distribuicao_insumos TYPE REF TO zcl_distribuicao_insumos.  "*-CS2025000249-16.06.2025-#182039-JT

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: t_0082         TYPE TABLE OF ty_zsdt0082,
      t_0082_lib     TYPE TABLE OF ty_zsdt0082,
      t_0132         TYPE TABLE OF zsdt0132,
      it_alt_qtd_pln TYPE TABLE OF ty_hist,
      t_vbak         TYPE TABLE OF ty_vbak,
      t_vbpa         TYPE TABLE OF ty_vbpa,                 "FF #145609
      t_trolz        TYPE TABLE OF ty_trolz,                "FF #145609
      t_vbkd         TYPE TABLE OF ty_vbkd,
      t_vbap         TYPE TABLE OF ty_vbap,
      t_mara         TYPE TABLE OF ty_mara,
      t_kna1         TYPE TABLE OF ty_kna1,
      t_vbfa         TYPE TABLE OF ty_vbfa,
      t_vbfa_h       TYPE TABLE OF ty_vbfa,
      t_vbfa_h_e     TYPE TABLE OF ty_vbfa,
      t_saida        TYPE TABLE OF ty_saida,
      _saida         TYPE ty_saida,
      tg_editor      TYPE TABLE OF ty_editor,
      tg_texto       TYPE TABLE OF tline WITH HEADER LINE,
      t_hist         TYPE TABLE OF ty_hist,
      t_textos_aux   TYPE TABLE OF ty_textos_aux,
      it_zsdt0041    TYPE TABLE OF ty_zsdt0041,
      it_zsdt0090    TYPE TABLE OF  ty_zsdt0090,
      it_zsdt        TYPE TABLE OF ty_zsdt,
      it_t001w       TYPE TABLE OF t001w,
      it_zsdt0411    TYPE TABLE OF zsdt0411,  "*-CS2025000249-16.06.2025-#182039-JT-inicio
      it_zsdt0270    TYPE TABLE OF zsdt0270,
      it_zsdt0271    TYPE TABLE OF zsdt0271,
      it_bloqueio    TYPE TABLE OF ty_bloqueio,             "FF #171337
      tl_saida_exec  TYPE TABLE OF ty_saida_exec WITH HEADER LINE, "FF #171337
      te_return      TYPE TABLE OF bapiret2,                "FF #171337
      it_zsdt0040    TYPE TABLE OF zsdt0040,
      it_tp_oper     TYPE TABLE OF dd07v,     "*-#187270-07.08.2025-JT-inicio
      wa_tp_oper     TYPE dd07v.              "*-#187270-07.08.2025-JT-inicio

DATA: fcat_alt_qtd_pln TYPE lvc_t_fcat,
      alv_alt_qtd_pln  TYPE REF TO cl_gui_alv_grid,
      cont_alt_qtd_pln TYPE REF TO cl_gui_custom_container,
      c_alv_tm         TYPE REF TO cl_alv_grid_toolbar_manager,
      _function        TYPE ui_functions,
      _layout          TYPE lvc_s_layo,
      _variant         TYPE disvariant,
      _save            TYPE c,
      wa_t001w         TYPE t001w,
      wa_zsdt0270      TYPE zsdt0270,
      wa_zsdt0271      TYPE zsdt0271,
      _stable          TYPE lvc_s_stbl VALUE 'XX'.

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio

DATA: gv_function_alv1 TYPE ui_functions,
      gv_layout_alv1   TYPE lvc_s_layo,
      gv_variant_alv1  TYPE disvariant,
      gv_save_alv1     TYPE c,
      gv_function_alv2 TYPE ui_functions,
      gv_layout_alv2   TYPE lvc_s_layo,
      gv_variant_alv2  TYPE disvariant,
      gv_save_alv2     TYPE c,
      it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.
* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim

"*-CS2025000249-04.09.2025-#189901-JT-inicio
DATA: lc_mm_util   TYPE REF TO zcl_mm_util,
      lv_object    TYPE ausp-objek,
      lv_obj_bloq  TYPE mara-matnr,
      lv_erro      TYPE char01,
      t_objectdata TYPE tt_clobjdat.
"*-CS2025000249-04.09.2025-#189901-JT-fim

*----------------------------------------------------------------------*
* WORS AREA
*----------------------------------------------------------------------*
DATA: wa_0082       TYPE ty_zsdt0082,
      wa_0132       TYPE zsdt0132,
      wa_0082_lib   TYPE ty_zsdt0082,
      wa_zsdt0411   TYPE zsdt0411,    "*-CS2025000249-16.06.2025-#182039-JT-inicio
      wa_vbak       TYPE ty_vbak,
      wa_vbkd       TYPE ty_vbkd,
      wa_vbap       TYPE ty_vbap,
      wa_mara       TYPE ty_mara,
      wa_kna1       TYPE ty_kna1,
      wa_vbfa       TYPE ty_vbfa,
      wa_vbfa_h     TYPE ty_vbfa,
      wa_vbfa_h_e   TYPE ty_vbfa,
      wg_editor     TYPE ty_editor,
      wa_saida      TYPE ty_saida,
      wa_saida_aux  TYPE ty_saida,
      wa_textos_aux TYPE ty_textos_aux,
      qte_lib1      TYPE zsdt0082-qte_lib,
      qte_sol1      TYPE zsdt0082-qte_sol,
      wa_zsdt0041   TYPE ty_zsdt0041,
      wa_zsdt0090   TYPE ty_zsdt0090,
      wa_zsdt       TYPE ty_zsdt,
      wa_zsdt0040   TYPE zsdt0040.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      fcat         TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      t_top_aux    TYPE slis_t_listheader,
      lt_sort      TYPE slis_t_sortinfo_alv,
      ls_sort      TYPE slis_sortinfo_alv,
      init.

DATA: wg_ucomm        TYPE sy-ucomm,
      wg_index        TYPE sy-index,
      wg_dest(50),
      wg_assunto      TYPE string,
      wg_qtde_sol_new TYPE zsdt0082-qte_sol.

DATA: g_container        TYPE scrfname VALUE 'CC_EMAIL',
      g_custom_container TYPE REF TO cl_gui_custom_container,
      obg_textbox        TYPE REF TO cl_gui_textedit.

DATA: tg_obj_cont TYPE TABLE OF solisti1,
      wg_obj_cont TYPE solisti1.

DATA: r_werks      TYPE RANGE OF werks_d.
DATA: r_werks_del      TYPE RANGE OF werks_d. "// WBARBOSA BUG-160708 11/12/2024

DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_alv_popup TYPE REF TO cl_gui_alv_grid,
      gt_popup     TYPE TABLE OF ty_popup,
      gv_continue  TYPE c,
      gt_fieldcat  TYPE lvc_t_fcat,
      gs_fcat      LIKE LINE OF gt_fieldcat,
      t_style      TYPE TABLE OF lvc_s_styl,
      wa_style     TYPE lvc_s_styl.

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio
* Declarações globais para o ALV
TYPES: BEGIN OF ty_dados_alv2.
         INCLUDE TYPE zsds094.
TYPES:   hist TYPE icon_d.
TYPES:   itinerario TYPE icon_d.
TYPES:   color TYPE lvc_t_scol.
TYPES:   style TYPE lvc_t_styl.
TYPES: END OF ty_dados_alv2.

DATA: go_cont_alv1_108 TYPE REF TO cl_gui_custom_container,
      go_alv1_108      TYPE REF TO cl_gui_alv_grid,
      go_cont_alv2_108 TYPE REF TO cl_gui_custom_container,
      go_alv2_108      TYPE REF TO cl_gui_alv_grid.

DATA: gt_alv1_fieldcat TYPE lvc_t_fcat,
      gt_alv2_fieldcat TYPE lvc_t_fcat.

DATA: t_color            TYPE lvc_t_scol,
      w_color            TYPE lvc_s_scol,
      gt_dados_alv1      TYPE zsds093_tt,
      gt_dados_alv2      TYPE TABLE OF ty_dados_alv2, "zsds094_tt,
      gt_dados_alv2_dado TYPE zsds094_tt,
      w_dados_alv2       TYPE ty_dados_alv2, "zsds094_tt,
      w_dados_alv2_dado  TYPE zsds094,
      lv_editar2         TYPE char01.
*     gt_dados_alv2      TYPE zsds094_tt,
*     gt_dados_alv2_disp TYPE TABLE OF ty_dados_alv2.
* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim

"FF #169501 - inicio
DATA: gv_agrupado            TYPE abap_bool VALUE abap_false,
      gt_dados_alv2_original TYPE zsds094_tt.
"FF #169501 - fim

DATA: t_centros_defens TYPE TABLE OF ty_centros_defens, "*-US191954-30.09.2025-#191954-JT-inicio
      w_centros_defens TYPE ty_centros_defens,          "*-US191954-30.09.2025-#191954-JT-inicio
      g_mesmo_centro   TYPE char01.                     "*-US191954-30.09.2025-#191954-JT-inicio

"FF - inicio #145609

" Batch input nova estrutura do campo de tabela
TYPES: BEGIN OF ty_bdcdata,
         program  TYPE bdcdata-program,  " Pool de módulos BDC
         dynpro   TYPE bdcdata-dynpro,   " NÚmero de tela BDC
         dynbegin TYPE bdcdata-dynbegin, " Início BDC de uma tela
         fnam     TYPE bdcdata-fnam,     " Nome do campo
         fval     TYPE bdcdata-fval,     " Valor do campo BDC
       END OF ty_bdcdata,

       BEGIN OF ty_message,
         cliente TYPE rf02d-kunnr,        " Código do cliente
         msgty   TYPE message-msgty,      " Tipo da mensagem
         msgno   TYPE message-msgno,      " Numero da mensagem
         msgtx   TYPE message-msgtx,      " Descrição da mensagem
       END OF   ty_message
       .
" Estruturas ...
DATA:
      st_bdcdata TYPE ty_bdcdata.
.

" Tabelas Internas ....
DATA:
  it_bdcdata TYPE TABLE OF ty_bdcdata,
  it_msg     TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
  it_message TYPE TABLE OF ty_message.


" Variaveis ....
DATA: vg_mode(1)    TYPE c VALUE 'N', " informa o Modo do Call Transaction
      vg_texto(100) TYPE c,        " Texto para o Indicator
      vg_s          TYPE c VALUE 'S',       " Informa o Update do call Transaction
      mensg         LIKE message VALUE IS INITIAL, " variavel que recebe retorno
      msgno         LIKE sy-msgno
      .

"FF - fim


*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      on_data_changed       FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

ENDCLASS.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_on_button_click FOR EVENT button_click  OF cl_gui_alv_grid IMPORTING es_col_id es_row_no,
      handle_on_button_click2 FOR EVENT button_click  OF cl_gui_alv_grid IMPORTING es_col_id es_row_no,
      on_toolbar_108 FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,

      "FF #178787 - inicio
      on_f4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.
    "FF #178787 - fim

ENDCLASS.                    "lcl_event_receiver DEFINITION

CLASS zcl_event DEFINITION.

  PUBLIC SECTION.

    METHODS:
      constructor IMPORTING io_alv_grid  TYPE REF TO cl_gui_alv_grid,
      on_dt_cnd   FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm,
      on_dt_cnd_f FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.

ENDCLASS.

DATA: o_event  TYPE REF TO zcl_event.

DATA: rg_werks TYPE RANGE OF werks_d.

DATA: gr_events       TYPE REF TO lcl_event_receiver.       "FF #178787
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: w_good   TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          lv_maktx TYPE makt-maktx,
          lv_meins TYPE mara-meins,
          lv_netwr TYPE zi_mm_boleta_comp_it_info-netwr.

    LOOP AT er_data_changed->mt_good_cells INTO w_good WHERE fieldname = 'KWMENG'.
      READ TABLE gt_dados_alv2 INTO w_dados_alv2 INDEX w_good-row_id.

      lv_value = w_good-value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = w_good-row_id
          i_fieldname = 'KWMENG'
          i_value     = lv_value.

      w_dados_alv2-kwmeng     = lv_value.
      MODIFY gt_dados_alv2 FROM w_dados_alv2 INDEX w_good-row_id.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells INTO w_good WHERE fieldname = 'NR_ROT_PC'.
      READ TABLE gt_dados_alv2 INTO w_dados_alv2 INDEX w_good-row_id.
      lv_value = w_good-value.

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
      IF w_good-value IS NOT INITIAL.

        DATA(roteiro_invalido) = abap_false.

        SELECT SINGLE zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
               tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
          INTO @DATA(lwa_lat_lon)
          FROM zsdt0132
          INNER JOIN tzone ON tzone~land1 = 'BR'
                          AND tzone~zone1 = zsdt0132~lzone
          WHERE zsdt0132~status  EQ 'A'
            AND zsdt0132~nr_rot  EQ @lv_value.

        IF sy-subrc NE 0.
          MESSAGE 'Roteiro não encontrado!' TYPE 'S'.
          roteiro_invalido = abap_true.
        ELSE.

          CALL METHOD zcl_manutencao_insumos=>chk_latitude_longitude
            EXPORTING
              i_latitude  = lwa_lat_lon-zlatitude
              i_longitude = lwa_lat_lon-zlongitude
              i_origem    = abap_true
            IMPORTING
              e_msg       = DATA(e_msg).

          IF e_msg IS NOT INITIAL.
            roteiro_invalido = abap_true.
            MESSAGE |Problema no Roteiro Ponto Coleta: { lv_value }! Erro: { e_msg } | TYPE 'S'.
          ENDIF.

        ENDIF.

        IF roteiro_invalido = abap_true.
          CLEAR: lv_value.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = w_good-row_id
              i_fieldname = 'NR_ROT_PC'
              i_value     = lv_value.
        ENDIF.

      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--


      IF lv_value IS INITIAL OR lv_value  = '0000000000'.
        w_dados_alv2-route      = abap_off.
        MODIFY gt_dados_alv2 FROM w_dados_alv2 INDEX w_good-row_id.
      ENDIF.
    ENDLOOP.

    CALL METHOD go_alv2_108->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_on_button_click.

    DATA: tl_texto        TYPE catsxt_longtext_itab,
          wl_texto        TYPE LINE OF catsxt_longtext_itab,
          it_texto        TYPE STANDARD TABLE OF tline,
          wa_texto        TYPE tline,
          vl_display_mode TYPE xfeld,
          wl_field        TYPE lvc_s_col,
          wl_name         TYPE thead-tdname,
          t_texto         TYPE TABLE OF tline,
          w_texto         TYPE tline.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          ref1        TYPE REF TO cl_gui_alv_grid,
          is_table    TYPE lvc_s_stbl.

    REFRESH: tl_texto, tg_texto.
    CLEAR:wl_texto.

*-CS2025000249-04.09.2025-#189749-JT-inicio
    READ TABLE gt_dados_alv1 INTO DATA(gw_dados_alv1) INDEX es_row_no-row_id.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING gw_dados_alv1 TO wa_saida.
    ELSE.
      READ TABLE t_saida INTO wa_saida INDEX es_row_no-row_id.
    ENDIF.
*-CS2025000249-04.09.2025-#189749-JT-fim

    REFRESH: t_textos_aux.

    LOOP AT t_0082 INTO  wa_0082
       WHERE vbeln EQ wa_saida-vbeln
         AND posnr EQ wa_saida-posnr
         AND nro_sol EQ wa_saida-nro_sol
         AND qte_sol IS NOT INITIAL.

      MOVE: wa_0082-nro_sol TO wa_textos_aux-nro_sol,
            wa_0082-seq     TO wa_textos_aux-seq.

      IF es_col_id EQ 'OBSERVACAO'.
        MOVE es_col_id(4) TO wa_textos_aux-id.
      ELSEIF es_col_id EQ 'ROTEIRO'.
        MOVE es_col_id(4) TO wa_textos_aux-id.
      ENDIF.

      APPEND wa_textos_aux  TO t_textos_aux.
    ENDLOOP.

    wg_index = 1.
    READ TABLE t_textos_aux INTO wa_textos_aux INDEX wg_index.
    CLEAR: wl_name.
    CONCATENATE wa_textos_aux-nro_sol wa_textos_aux-seq INTO wl_name.

    "FF #178787 - inicio
    DATA  lv_im_title TYPE sytitle VALUE 'TESTE'.

    IF es_col_id = 'DESC_ROT'.
      REFRESH: it_texto, tl_texto.
      CLEAR: wl_texto.

      vl_display_mode = abap_true.
      wl_name         = wa_saida-nr_rot.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZROT'
          language  = sy-langu
          name      = wl_name
          object    = 'ZSDROTEIRO'
        TABLES
          lines     = it_texto
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.

      IF sy-subrc IS INITIAL.
        LOOP AT it_texto INTO wa_texto.
          MOVE: wa_texto-tdline TO wl_texto.
          APPEND wl_texto TO tl_texto.
          CLEAR: wl_texto.
        ENDLOOP.
      ENDIF.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title        = 'Roteiro'
          im_display_mode = vl_display_mode
        CHANGING
          ch_text         = tl_texto.

      RETURN.
    ENDIF.

    IF es_col_id = 'OBSERVACAO_VE'.

      lv_im_title = 'Observação Venda Especial'.

      zcl_util_sd=>identifica_venda_especial(
        EXPORTING
          i_ordem_vendas = wa_saida-vbeln
          i_setor_ativ   = wa_saida-spart
        IMPORTING
          et_texto       = tg_texto[]
*         e_venda_especial =
      ).
      "FF #178787 - fim

    ELSE.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = wa_textos_aux-id
          language                = sy-langu
          name                    = wl_name
          object                  = 'ZTEXTO'
        TABLES
          lines                   = tg_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

    ENDIF.

    LOOP AT tg_texto INTO w_texto.
      MOVE: w_texto-tdline TO wl_texto.
      APPEND wl_texto TO tl_texto.
      CLEAR: wl_texto.
    ENDLOOP.


    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = lv_im_title "FF #178787
        im_display_mode = abap_true
      CHANGING
        ch_text         = tl_texto.

*    CALL SCREEN 104 STARTING AT 10 4 ENDING AT 80 14.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = is_table.

    CALL METHOD cl_gui_cfw=>dispatch.
    CALL METHOD cl_gui_cfw=>flush.
  ENDMETHOD.                    "HANDLE_ON_BUTTON_CLICK.

  METHOD handle_on_button_click2.

    DATA: w_saida_dados1 TYPE zsds093,
          w_saida_dados2 TYPE ty_dados_alv2,
          w_zsds093      TYPE zsds093,
          w_zsds094      TYPE zsds094.

    READ TABLE gt_dados_alv1 INTO w_saida_dados1 INDEX 1.
    READ TABLE gt_dados_alv2 INTO w_saida_dados2 INDEX es_row_no-row_id.

    CASE es_col_id.
      WHEN 'HIST'.
        MOVE-CORRESPONDING: w_saida_dados1 TO w_zsds093,
                            w_saida_dados2 TO w_zsds094.

        CALL FUNCTION 'ZSD_DISTRIBUICAO_HISTORICO'
          EXPORTING
            i_zsds093 = w_zsds093
            i_zsds094 = w_zsds094.

      WHEN 'ITINERARIO'.
        PERFORM f_criar_itinerario USING w_saida_dados2.
    ENDCASE.

  ENDMETHOD.

  METHOD on_toolbar_108.

    DATA : mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).

      CASE <fs_tollbar>-function.
        WHEN OTHERS.
          IF <fs_tollbar>-function EQ '&REFRESH'.
            <fs_tollbar>-function = 'REFRESH_GRID'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
            <fs_tollbar>-function = 'DELETE_ROW'.
          ELSEIF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
            <fs_tollbar>-function = 'INSERT_ROW'.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_f4.

    DATA: lt_return  TYPE TABLE OF ddshretval,
          ls_return  TYPE ddshretval,
          lv_lifnr   TYPE lifnr,
          lv_werks   TYPE werks_d,
          lv_ebeln   TYPE ebeln,
          lv_lifn2   TYPE lifnr,
          lv_armazem TYPE zsdt0132-armazem.

    TYPES: BEGIN OF ty_f4_roteiro,
             nr_rot            TYPE zsdt0132-nr_rot,
             rot_desc          TYPE zsdt0132-rot_desc,
             city1             TYPE zsdt0132-city1,
             uf                TYPE zsdt0132-uf,
             zone1             TYPE tzone-zone1,
             zlatitude         TYPE tzone-zlatitude,
             zlongitude        TYPE tzone-zlongitude,
             z_url_localizacao TYPE tzone-z_url_localizacao,
           END OF ty_f4_roteiro.

    TYPES: BEGIN OF ty_f4_flex,
             cod   TYPE char01,
             descr TYPE char50,
           END OF ty_f4_flex.

    DATA: lt_f4_data TYPE STANDARD TABLE OF ty_f4_roteiro.
    DATA: lt_lat_lon TYPE STANDARD TABLE OF ty_f4_roteiro.
    DATA: lt_f4_flex TYPE STANDARD TABLE OF ty_f4_flex.
    DATA: lv_ok    TYPE char01,
          lv_route TYPE route_vl.

    DATA: t_dd07v    TYPE TABLE OF dd07v.

    FREE: lv_lifnr, lv_lifn2, lt_f4_data.

    IF e_fieldname = 'NR_ROT_PC'.
      " Obter a linha atual do ALV
      READ TABLE gt_dados_alv1 INTO DATA(ls_dados_alv1) INDEX 1.
      READ TABLE gt_dados_alv2 INTO DATA(ls_dados)      INDEX es_row_no-row_id.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

*"// wbarbosa Verificar Deposito Origem 28/08/2025 US-169490
      CLEAR lv_armazem.
      IF ls_dados-lifnr_pedido IS INITIAL.
        lv_armazem = abap_true.
      ENDIF.

      IF ls_dados_alv1-kunnr IS NOT INITIAL.

        SELECT zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
               tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
          INTO TABLE @lt_lat_lon
          FROM zsdt0132
          INNER JOIN tzone ON tzone~land1 = 'BR'
                          AND tzone~zone1 = zsdt0132~lzone
          WHERE zsdt0132~status  EQ 'A'
            AND zsdt0132~nr_rot  EQ @ls_dados_alv1-nr_rot.

        READ TABLE lt_lat_lon INTO DATA(ls_lat_lon) WITH KEY nr_rot = ls_dados_alv1-nr_rot.
        IF sy-subrc IS INITIAL.

          CALL METHOD zcl_manutencao_insumos=>chk_latitude_longitude
            EXPORTING
              i_latitude  = ls_lat_lon-zlatitude
              i_longitude = ls_lat_lon-zlongitude
              i_origem    = abap_true
            IMPORTING
              e_msg       = DATA(e_msg).

        ENDIF.

        IF e_msg IS NOT INITIAL.
          MESSAGE |Problema no Roteiro Entrega: { ls_dados_alv1-nr_rot }! Erro: { e_msg } | TYPE 'I'.
          RETURN.
        ENDIF.

      ENDIF.
*"// wbarbosa Verificar Deposito Origem 28/08/2025 US-169490

      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
      IF ls_dados-lifnr_pc IS NOT INITIAL.
        SELECT zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
                tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
           INTO TABLE @lt_f4_data
           FROM zsdt0132
           INNER JOIN tzone ON tzone~land1 = 'BR'
                           AND tzone~zone1 = zsdt0132~lzone
           WHERE zsdt0132~status  EQ 'A'
             AND zsdt0132~lifnr   EQ @ls_dados-lifnr_pc
             AND zsdt0132~armazem EQ @lv_armazem. "// wbarbosa Verificar Deposito Origem 28/08/2025 US-169490
      ENDIF.

*       " 1. Pedido > EKPA
*      IF ls_dados-ebeln IS NOT INITIAL.
*        lv_ebeln = ls_dados-ebeln.
*
*        SELECT SINGLE lifnr INTO lv_lifn2
*          FROM ekko
*         WHERE ebeln = lv_ebeln.
*
*        IF sy-subrc = 0 AND lv_lifn2 IS NOT INITIAL.
*          lv_lifnr = lv_lifn2.
*        ELSE.
*          SELECT SINGLE lifn2 INTO lv_lifn2
*            FROM ekpa
*           WHERE ebeln = lv_ebeln AND parvw = 'LF'.
*
*          IF sy-subrc = 0.
*            lv_lifnr = lv_lifn2.
*          ENDIF.
*        ENDIF.
*
*        IF lv_lifnr IS NOT INITIAL.
*          SELECT zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
*                 tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
*            INTO TABLE @lt_f4_data
*            FROM zsdt0132
*            INNER JOIN tzone ON tzone~land1 = 'BR'
*                            AND tzone~zone1 = zsdt0132~lzone
*            WHERE zsdt0132~status = 'A'
*              AND zsdt0132~lifnr  = @lv_lifnr.
*        ENDIF.
*
*        " 2. Fornecedor direto
**     ELSEIFIF lt_f4_data[] IS INITIAL AND ls_dados-lifnr IS NOT INITIAL.
*      ELSEIF ls_dados-lifnr IS NOT INITIAL.
*        lv_lifnr = ls_dados-lifnr.
*
*        SELECT zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
*               tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
*          INTO TABLE @lt_f4_data
*          FROM zsdt0132
*          INNER JOIN tzone ON tzone~land1 = 'BR'
*                          AND tzone~zone1 = zsdt0132~lzone
*          WHERE zsdt0132~status = 'A'
*            AND zsdt0132~lifnr  = @lv_lifnr.
*
*        " 3. Werks
**     ELSEIF lt_f4_data[] IS INITIAL AND ls_dados-werks IS NOT INITIAL.
*      ELSEIF ls_dados-werks IS NOT INITIAL.
*        lv_werks = ls_dados-werks.
*
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            input  = lv_werks
*          IMPORTING
*            output = lv_lifnr.
*
*        IF strlen( lv_lifnr ) <> 10.
*          CLEAR lv_lifnr.
*        ENDIF.
*
*        " Buscar dados
*        IF lv_lifnr IS NOT INITIAL.
*          SELECT zsdt0132~nr_rot, zsdt0132~rot_desc, zsdt0132~city1, zsdt0132~uf,
*                 tzone~zone1, tzone~zlatitude, tzone~zlongitude, tzone~z_url_localizacao
*            INTO TABLE @lt_f4_data
*            FROM zsdt0132
*            INNER JOIN tzone ON tzone~land1 = 'BR'
*                            AND tzone~zone1 = zsdt0132~lzone
*            WHERE zsdt0132~status = 'A'
*              AND zsdt0132~lifnr  = @lv_lifnr.
*        ENDIF.
*      ENDIF.
      "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----



      IF lt_f4_data[] IS INITIAL.
        MESSAGE 'Nenhum roteiro encontrado.' TYPE 'I'.
        RETURN.
      ENDIF.

      CLEAR: lv_ok, lv_route.

      " Exibir F4
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield     = 'NR_ROT'
          value_org    = 'S'
          window_title = 'Selecionar Roteiro'
        TABLES
          value_tab    = lt_f4_data
          return_tab   = lt_return.

      " Tratar retorno
      READ TABLE lt_return INTO ls_return INDEX 1.
      IF sy-subrc = 0 AND ls_dados-nao_editar = abap_false.

        "// wbarbosa Verificar Deposito Origem 28/08/2025 US-169490
        READ TABLE lt_f4_data INTO DATA(ls_f4_data) WITH KEY nr_rot = ls_return-fieldval.
        IF sy-subrc IS INITIAL.

          CLEAR e_msg.
          CALL METHOD zcl_manutencao_insumos=>chk_latitude_longitude
            EXPORTING
              i_latitude  = ls_f4_data-zlatitude
              i_longitude = ls_f4_data-zlongitude
            IMPORTING
              e_msg       = e_msg.

        ENDIF.
        IF e_msg IS NOT INITIAL.
          MESSAGE |Problema no Roteiro Ponto Coleta: { ls_return-fieldval }! Erro: { e_msg } | TYPE 'I'.
          RETURN.
        ENDIF.
        "// wbarbosa Verificar Deposito Origem 28/08/2025 US-169490

        READ TABLE gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_row>) INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          <fs_row>-nr_rot_pc = ls_return-fieldval.
          zcl_manutencao_insumos=>verificar_itinerario_82( EXPORTING i_rota_lr     = ls_dados_alv1-nr_rot
                                                                     i_rota_pc     = <fs_row>-nr_rot_pc
                                                                     is_background = abap_true
                                                           IMPORTING is_ok         = lv_ok
                                                                     e_route       = lv_route ).
          <fs_row>-route     = lv_route.

          CALL METHOD go_alv2_108->refresh_table_display
            EXPORTING
              is_stable = w_stable.
        ENDIF.

      ENDIF.

      " Somente após tratar o valor, marca como handled
      er_event_data->m_event_handled = abap_true.

    ELSEIF e_fieldname = 'FLEXIBILIDADE'.
      " Obter a linha atual do ALV
      READ TABLE gt_dados_alv2 INTO ls_dados INDEX es_row_no-row_id.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      FREE: lt_f4_flex.

      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname   = 'ZD_FLEXIBILIDADE'
          text      = 'X'
          langu     = sy-langu
        TABLES
          dd07v_tab = t_dd07v.

      LOOP AT t_dd07v INTO DATA(ls_dd07v).
        APPEND VALUE #( cod = ls_dd07v-domvalue_l descr = ls_dd07v-ddtext ) TO lt_f4_flex.
      ENDLOOP.

      " Exibir F4
      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield     = 'COD'
          value_org    = 'S'
          window_title = 'Selecionar Flexibilidade'
        TABLES
          value_tab    = lt_f4_flex
          return_tab   = lt_return.

      " Tratar retorno
      READ TABLE lt_return INTO ls_return INDEX 1.
      IF sy-subrc = 0 AND ls_dados-nao_editar = abap_false.
        READ TABLE gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_row2>) INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          <fs_row2>-flexibilidade = ls_return-fieldval.
          CALL METHOD go_alv2_108->refresh_table_display
            EXPORTING
              is_stable = w_stable.
        ENDIF.
      ENDIF.
      " Somente após tratar o valor, marca como handled
      er_event_data->m_event_handled = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS zcl_event IMPLEMENTATION.

  METHOD constructor.

    CREATE OBJECT c_alv_tm
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.

  METHOD on_dt_cnd.

  ENDMETHOD.

  METHOD on_dt_cnd_f.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULÁRIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_vkorg  FOR wa_0082-vkorg OBLIGATORY NO INTERVALS, "FF #171337
                  s_spart  FOR wa_0082-spart NO INTERVALS,  "FF #171337
                  s_vkbur  FOR wa_0082-vkbur,
                  s_safra  FOR wa_zsdt0040-safra,
                  s_cult   FOR wa_zsdt0040-cultura,
                  s_vkgrp  FOR wa_0082-vkgrp,
                  s_auart  FOR wa_0082-auart,
                  s_nrosol FOR wa_0082-nro_sol,
                  s_dt_sol FOR wa_0082-dt_sol OBLIGATORY,  "*-#187270-07.08.2025-JT-inicio
                  s_ordem  FOR wa_0082-vbeln,
                  s_matnr  FOR wa_vbap-matnr,
                  s_werks  FOR wa_0082-werks OBLIGATORY NO INTERVALS. "FF #171337
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_pend   RADIOBUTTON GROUP a1,                "FF #145609
              p_aberto RADIOBUTTON GROUP a1,
              p_workf  RADIOBUTTON GROUP a1,  "*-CS2025000249-16.06.2025-#182039-JT
              p_proces RADIOBUTTON GROUP a1,  "*-CS2025000249-16.06.2025-#182039-JT
              p_todas  RADIOBUTTON GROUP a1.

SELECTION-SCREEN: END OF BLOCK b2.

*----------------------------------------------------------------------*
* INITIALIZATION.
*----------------------------------------------------------------------*

INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT lc_distribuicao_insumos.  "*-CS2025000249-16.06.2025-#182039-JT
  CREATE OBJECT lc_mm_util.               "*-CS2025000249-04.09.2025-#189901-JT-inicio

  DATA(lv_titulo_pend) = 'Distribuição de Embarque - INSUMOS (Pendentes)'. "FF #145609
  SET TITLEBAR '0500' WITH lv_titulo_pend.                  "FF #145609

  PERFORM authority_check.
  CHECK gv_continue EQ abap_true.
  PERFORM selecionar_dados.
  PERFORM organizar_dados.
  PERFORM iniciar_variaveis.
  PERFORM imprimir_dados.
  PERFORM unlock_table.                                     "FF #171337


*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  REFRESH: t_0082, t_vbak, t_vbap, t_mara, t_vbfa, t_kna1, t_aux, t_saida,
           gt_dados_alv1, gt_dados_alv2. "*-CS2025000249-04.09.2025-#189749-JT-inicio

  CALL FUNCTION 'GET_DOMAIN_VALUES'  "*-#187270-07.08.2025-JT-inicio
    EXPORTING
      domname         = 'ZSDD_TPOPR'
      text            = 'X'
    TABLES
      values_tab      = it_tp_oper
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.

  "FF #145609 - inicio
  DATA: lt_conditions TYPE string.

  " Construção dinâmica da condição WHERE
  lt_conditions = |vkorg IN @s_vkorg AND spart IN @s_spart AND vkbur  IN @s_vkbur AND | &&
                  |vkgrp IN @s_vkgrp AND auart IN @s_auart AND dt_sol IN @s_dt_sol AND nro_sol IN @s_nrosol AND | &&
                  |vbeln IN @s_ordem AND werks IN @s_werks AND status NE 3|.

  IF p_pend IS NOT INITIAL.
    CONCATENATE lt_conditions 'AND desmembrar IS NOT INITIAL' INTO lt_conditions SEPARATED BY space.
  ENDIF.

*-CS2025000249-16.06.2025-#182039-JT-inicio
  IF p_proces = abap_true.
    CONCATENATE lt_conditions 'AND bloqueio = @abap_true' INTO lt_conditions SEPARATED BY space.
  ENDIF.
*-CS2025000249-16.06.2025-#182039-JT-fim

  SELECT nro_sol, seq, seq_lib, vbeln, posnr, vkorg, spart, vkgrp, vkbur,
         auart, dt_sol, qte_sol, usuario_sol, dt_liber, usuario_lib, qte_lib,
         dt_entrega, status, dt_canc, user_canc, nr_rot, werks, desmembrar, route_ov, route_iti, tp_oper,
         bloqueio, origem_estoque,                        "*-CS2025000249-16.06.2025-#182039-JT-inicio
         integrou_werks, integrou_vkbur, integrou_kunnr,  "*-CS2025000249-01.09.2025-#189440-JT-inicio
         integrou_nr_rot, integrou_nr_rot_pc
    FROM zsdt0082
    WHERE (lt_conditions)
    INTO TABLE @t_0082.

*"// WBARBOSA BUG-160708 11/12/2024
  IF r_werks_del IS NOT INITIAL.
    DELETE t_0082 WHERE vkbur IN r_werks_del.
  ENDIF.
*"// WBARBOSA BUG-160708 11/12/2024

*  SELECT nro_sol seq seq_lib vbeln posnr vkorg spart vkgrp vkbur
*         auart dt_sol qte_sol usuario_sol dt_liber usuario_lib qte_lib dt_entrega status  dt_canc user_canc nr_rot werks
*         desmembrar                                         "FF #145609
*    FROM zsdt0082
*      INTO TABLE t_0082
*        WHERE vkorg  IN s_vkorg
*          AND spart  IN s_spart
*          AND vkbur  IN s_vkbur
*          AND vkgrp  IN s_vkgrp
*          AND auart  IN s_auart
*          AND dt_sol IN s_dt_sol
*          AND vbeln  IN s_ordem
*          AND werks  IN s_werks
*          AND status  NE 3.
  "FF #145609 - fim



  CHECK t_0082[] IS NOT INITIAL.

*-CS2025000249-16.06.2025-#182039-JT-inicio
  SELECT *
    FROM zsdt0411
    INTO TABLE it_zsdt0411
     FOR ALL ENTRIES IN t_0082
   WHERE nro_sol = t_0082-nro_sol.
*-CS2025000249-16.06.2025-#182039-JT-fim

  SELECT vbeln doc_simulacao FROM zsdt0090 INTO CORRESPONDING FIELDS OF TABLE it_zsdt
    FOR ALL ENTRIES IN t_0082
    WHERE vbeln EQ  t_0082-vbeln.

*  MOVE-CORRESPONDING IT_ZSDT0090 TO IT_ZSDT.

  SELECT vbeln doc_simulacao FROM zsdt0041 APPENDING CORRESPONDING FIELDS OF TABLE it_zsdt
    FOR ALL ENTRIES IN t_0082
   WHERE vbeln EQ t_0082-vbeln.

*  MOVE-CORRESPONDING IT_ZSDT0041 TO IT_ZSDT.

  SELECT  * FROM zsdt0040 INTO TABLE it_zsdt0040
    FOR ALL ENTRIES IN it_zsdt
     WHERE   doc_simulacao EQ it_zsdt-doc_simulacao
        AND  safra         IN s_safra
        AND  cultura       IN s_cult.

  IF s_safra IS NOT INITIAL AND s_cult IS NOT INITIAL.
    DELETE it_zsdt0040 WHERE safra NOT IN s_safra AND
                             cultura NOT IN s_cult.
  ELSEIF s_safra IS NOT INITIAL AND s_cult IS  INITIAL.
    DELETE it_zsdt0040 WHERE safra NOT IN s_safra.

  ELSEIF s_safra IS  INITIAL AND s_cult IS NOT INITIAL.
    DELETE it_zsdt0040 WHERE cultura NOT IN s_cult.
  ENDIF.


  SELECT nro_sol seq seq_lib vbeln posnr vkorg spart vkgrp vkbur
         auart dt_sol qte_sol usuario_sol dt_liber usuario_lib qte_lib dt_entrega status  dt_canc user_canc
   FROM zsdt0082
     INTO TABLE t_0082_lib
    FOR ALL ENTRIES IN t_0082
       WHERE nro_sol EQ t_0082-nro_sol
         AND vbeln   EQ t_0082-vbeln
         AND posnr   EQ t_0082-posnr
         AND status  NE 3.

  IF sy-subrc IS INITIAL.


    SELECT vbeln posnr inco1 inco2 FROM vbkd
      INTO TABLE t_vbkd
      FOR ALL ENTRIES IN t_0082
        WHERE vbeln EQ t_0082-vbeln.

    SELECT vbeln kunnr vkbur spart
      FROM vbak
        INTO TABLE t_vbak
        FOR ALL ENTRIES IN t_0082
          WHERE vbeln EQ t_0082-vbeln.

    IF sy-subrc IS INITIAL.


      "FF #145609 - inicio
      SELECT DISTINCT a~vbeln a~lzone
      FROM vbpa AS a
        INTO TABLE t_vbpa
            FOR ALL ENTRIES IN t_0082
              WHERE a~vbeln = t_0082-vbeln
                AND a~parvw = 'PC'.


      IF sy-subrc = 0 AND t_vbpa[] IS NOT INITIAL.

        SELECT azone lzone route
        INTO TABLE t_trolz FROM trolz
        FOR ALL ENTRIES IN t_vbpa
        WHERE azone = t_vbpa-lzone
          AND route <> abap_off.   "*-US190671-15.09.2025-#190671-JT

      ENDIF.
      "FF #145609 - fim

      SELECT a~vbeln, a~vbelv, a~matnr, a~arktx, a~matkl, a~kwmeng, a~vrkme, a~werks, a~netpr, a~waerk, a~posnr, a~meins, a~lgort, a~route
        FROM vbap AS a
        INNER JOIN vbep AS b ON b~vbeln EQ a~vbeln AND b~posnr EQ a~posnr "Ajuste USER STORY 131069 / AOENNING.
        INTO TABLE @t_vbap
         FOR ALL ENTRIES IN @t_vbak
           WHERE  a~vbeln EQ @t_vbak-vbeln
             AND a~matnr IN @s_matnr
             AND b~lifsp <> '12'."Ajuste USER STORY 131069 / AOENNING.


      IF sy-subrc IS INITIAL.
        SELECT matnr wrkst mtart spart  "*-CS2025000249-01.09.2025-#189440-JT
          FROM mara
           INTO TABLE t_mara
           FOR ALL ENTRIES IN t_vbap
             WHERE matnr EQ t_vbap-matnr.

        SELECT vbelv posnv vbtyp_n vbtyp_v vbeln posnn rfmng
          FROM vbfa
            INTO TABLE t_vbfa
            FOR ALL ENTRIES IN t_vbap
              WHERE vbelv EQ t_vbap-vbeln
                AND posnv EQ t_vbap-posnr
                AND vbtyp_n IN ('J', 'H', 'C', 'L' )
                AND vbtyp_v EQ 'C'.

        IF t_vbfa IS NOT INITIAL.
          SELECT vbelv posnv vbtyp_n vbtyp_v vbeln posnn rfmng
            FROM vbfa
              INTO TABLE t_vbfa_h
              FOR ALL ENTRIES IN t_vbfa
                WHERE vbelv EQ t_vbfa-vbeln
                  AND posnv EQ t_vbfa-posnn
                  AND vbtyp_n EQ 'O'
                  AND vbtyp_v EQ 'H'.

          IF NOT t_vbfa_h IS INITIAL.

            SELECT vbelv posnv vbtyp_n vbtyp_v vbeln posnn rfmng
                     FROM vbfa
                       INTO TABLE t_vbfa_h_e
                       FOR ALL ENTRIES IN t_vbfa_h
                         WHERE vbelv EQ t_vbfa_h-vbeln
                           AND posnv EQ t_vbfa_h-posnn
                           AND vbtyp_n EQ 'S'
                           AND vbtyp_v EQ 'O'.
          ENDIF.

        ENDIF.


*        SELECT VBELV POSNV VBTYP_N VBTYP_V RFMNG VBELN POSNN
*          FROM VBFA
*           INTO TABLE T_VBFA
*           FOR ALL ENTRIES IN T_VBAP
*             WHERE VBELV EQ T_VBAP-VBELV
*               AND POSNV EQ T_VBAP-POSNR
*               AND (    VBTYP_N EQ 'M'
*                     OR VBTYP_N EQ 'O' )
*               AND VBTYP_V EQ 'C'.
      ENDIF.

      SELECT kunnr name1 ort01 regio stcd1 stcd2 stcd3 stkzn
        FROM kna1
         INTO TABLE t_kna1
         FOR ALL ENTRIES IN t_vbak
           WHERE kunnr EQ t_vbak-kunnr.



      "Seleção descrição filial de faturamento.
      SELECT * FROM t001w
      INTO TABLE it_t001w
        FOR ALL ENTRIES IN t_vbak
        WHERE werks EQ t_vbak-vkbur.

      "Selecionar a regional. t_0082
      SELECT *
     FROM zsdt0271
       INTO TABLE it_zsdt0271
      FOR ALL ENTRIES IN t_vbak
     WHERE filial EQ t_vbak-vkbur.

      IF it_zsdt0271[] IS NOT INITIAL.
        SELECT *
         FROM zsdt0270
       INTO TABLE it_zsdt0270
         FOR ALL ENTRIES IN it_zsdt0271
       WHERE cod_regional EQ it_zsdt0271-cod_regional.
      ENDIF.
    ENDIF.
  ENDIF.

  "FF #145609 - inicio
*  SELECT nr_rot rot_desc FROM zsdt0132
*    INTO CORRESPONDING FIELDS OF TABLE t_0132
*      FOR ALL ENTRIES IN t_0082
*      WHERE nr_rot EQ t_0082-nr_rot.

  SELECT * FROM zsdt0132
    INTO TABLE t_0132
      FOR ALL ENTRIES IN t_0082
      WHERE nr_rot EQ t_0082-nr_rot.

  "FF #145609  - fim


ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
FORM organizar_dados .
  DATA: wl_tot_fat   TYPE vbfa-rfmng,
        wl_tot_fat_d TYPE vbfa-rfmng,
        wl_qte       TYPE zsdt0082-qte_sol,
        wl_qte_lib   TYPE zsdt0082-qte_lib,
        wl_tot_lib   TYPE zsdt0082-qte_lib,
        wl_tot_sol   TYPE zsdt0082-qte_sol,
        wl_0082      LIKE LINE OF t_0082,
        wl_0082_lib  LIKE LINE OF t_0082,
        wl_name      TYPE thead-tdname.

  SORT: t_0082      BY nro_sol ASCENDING seq DESCENDING,
        t_vbak      BY vbeln,
        t_vbap      BY vbeln posnr,
        t_vbkd      BY vbeln posnr,
        t_mara      BY matnr,
        t_vbfa      BY vbelv posnv,
        t_kna1      BY kunnr,
        it_zsdt     BY vbeln  doc_simulacao,
        it_zsdt0040 BY doc_simulacao .

  CLEAR: wa_saida, wa_vbap, wa_vbak, wa_vbfa, wa_mara, wa_kna1, wl_tot_fat, wl_tot_fat_d, wl_qte, wl_qte_lib, wa_zsdt0040, wa_zsdt.


  LOOP AT t_0082 INTO wa_0082.
    CLEAR: wl_tot_lib, wl_tot_sol.

*-CS2025000249-16.06.2025-#182039-JT-inicio
    lc_distribuicao_insumos->set_avaliar_status_solic( i_nro_sol = wa_0082-nro_sol i_seq = wa_0082-seq i_vbeln = wa_0082-vbeln i_posnr = wa_0082-posnr ).
*-CS2025000249-16.06.2025-#182039-JT-fim

    CLEAR:  wa_saida, wa_vbap, wa_vbkd, wa_vbak, wa_vbfa, wa_mara, wa_kna1, wl_tot_fat, wl_tot_fat_d, wl_qte, wa_zsdt0040, wa_zsdt, wa_0132.

    LOOP AT t_0082_lib INTO wl_0082 WHERE vbeln EQ wa_0082-vbeln
                                      AND posnr EQ wa_0082-posnr
                                      AND nro_sol EQ wa_0082-nro_sol.

      CASE wl_0082-status.
        WHEN 4.
        WHEN OTHERS.
          IF wl_0082-seq EQ 1.
            ADD wl_0082-qte_sol TO wl_tot_sol. "XQTSOL
          ELSE.
            ADD wl_0082-qte_lib TO wl_tot_lib. "XQTLIB
          ENDIF.
      ENDCASE.

      IF wa_saida-roteiro NE icon_display_more.
        CLEAR: wl_name.
        CONCATENATE wl_0082-nro_sol wl_0082-seq INTO wl_name.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'ROTE'
            language                = sy-langu
            name                    = wl_name
            object                  = 'ZTEXTO'
          TABLES
            lines                   = tg_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc IS INITIAL..
          wa_saida-roteiro = icon_display_more.                                 "Roteiro

        ENDIF.

      ENDIF.

      IF wa_saida-observacao NE icon_display_more.
        CLEAR: wl_name.
        CONCATENATE wl_0082-nro_sol wl_0082-seq INTO wl_name.
        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'OBSE'
            language                = sy-langu
            name                    = wl_name
            object                  = 'ZTEXTO'
          TABLES
            lines                   = tg_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.
        IF sy-subrc IS INITIAL..
          wa_saida-observacao = icon_display_more.                                "Observação

        ENDIF.

      ENDIF.



    ENDLOOP.

    IF wa_0082-nr_rot IS INITIAL.
      wa_saida-nr_rot = 9999999999.
    ELSE.
      wa_saida-nr_rot = wa_0082-nr_rot.
    ENDIF.

    IF wa_saida-roteiro IS INITIAL.
      wa_saida-roteiro =  icon_enter_more.
    ENDIF.

    IF wa_saida-observacao IS INITIAL.
      wa_saida-observacao =  icon_enter_more.
    ENDIF.

    "FF #178787 - inicio
    IF wa_saida-observacao_ve IS INITIAL.
      wa_saida-observacao_ve =  icon_enter_more.
    ENDIF.
    "FF #178787 - fim

    wl_qte     = wl_tot_sol - wl_tot_lib.

    READ TABLE t_vbkd INTO wa_vbkd WITH KEY vbeln = wa_0082-vbeln
                                            "POSNR = WA_0082-POSNR
                                            BINARY SEARCH.

    READ TABLE t_vbak INTO wa_vbak WITH KEY vbeln = wa_0082-vbeln
                                            BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      wa_saida-spart = wa_vbak-spart.
      READ TABLE t_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln
                                              posnr = wa_0082-posnr
                                              BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_vbap-matnr
                                                BINARY SEARCH.
        wa_saida-wrkst = wa_mara-wrkst.                           "Marca
        wa_saida-mtart = wa_mara-mtart.
        wa_saida-lgort = wa_vbap-lgort.
        wa_saida-iti_ov = wa_vbap-route. "FF #145609 - Itinerário OV

        CLEAR wl_tot_fat.

        LOOP AT  t_vbfa INTO wa_vbfa
              WHERE vbelv EQ wa_vbap-vbeln
                 AND posnv EQ wa_vbap-posnr.

***       cs1137898-ir149970 - alexandre - 06.09.2023
          SELECT SINGLE vgbel FROM lips
            INTO @DATA(v_vgbel)
            WHERE vbeln = @wa_vbfa-vbeln.

          SELECT SINGLE auart FROM vbak
            INTO @DATA(v_auart)
            WHERE vbeln = @v_vgbel.

          IF v_auart <> 'ZFNT'.
            IF wa_vbfa-vbtyp_n EQ 'J'.
              ADD wa_vbfa-rfmng TO wl_tot_fat.
            ENDIF.
          ENDIF.
***      Fim Alteração - Alexandre - 06.09.2023

          LOOP AT t_vbfa_h INTO wa_vbfa_h WHERE vbelv EQ wa_vbfa-vbeln AND
                                                posnv EQ wa_vbfa-posnn.
            READ TABLE t_vbfa_h_e TRANSPORTING NO FIELDS WITH KEY vbelv = wa_vbfa_h-vbeln
                                                                  posnv = wa_vbfa_h-posnn.
            IF NOT sy-subrc IS INITIAL.
              wa_vbfa_h-rfmng = wa_vbfa_h-rfmng * -1.
              ADD wa_vbfa_h-rfmng TO wl_tot_fat.
            ENDIF.
            CLEAR wa_vbfa_h.
          ENDLOOP.
          CLEAR wa_vbfa.
        ENDLOOP.

*        LOOP AT T_VBFA INTO WA_VBFA WHERE VBELN   EQ WA_VBAP-VBELV
*                                      AND POSNV   EQ WA_VBAP-POSNR
*                                      AND VBTYP_N EQ 'M'
*                                      AND VBTYP_V EQ 'C'.
*
*          ADD WA_VBFA-RFMNG TO WL_TOT_FAT.  "XTOTFAT
*          CLEAR: WA_VBFA.
*        ENDLOOP.
*
*        READ TABLE T_VBFA INTO WA_VBFA WITH KEY VBELV   = WA_VBAP-VBELV
*                                                POSNV   = WA_VBAP-POSNR
*                                                VBTYP_N = 'M'
*                                                VBTYP_V = 'C'
*                                                BINARY SEARCH.
*        IF SY-SUBRC IS INITIAL.
*          LOOP AT T_VBFA INTO WA_VBFA WHERE VBELV   EQ WA_VBFA-VBELN
*                                        AND POSNV   EQ WA_VBFA-POSNN
*                                        AND VBTYP_N EQ 'O'
*                                        AND VBTYP_V EQ 'H'.
*
*            ADD WA_VBFA-RFMNG TO WL_TOT_FAT_D. "XTOTFATD
*            CLEAR: WA_VBFA.
*          ENDLOOP.
*
*        ENDIF.
*         XTOTF            = XTOTFAT    - XTOTFATD
*        WA_SAIDA-QTE_FAT = WL_TOT_FAT - WL_TOT_FAT_D.             "Qte. Faturado
        wa_saida-qte_fat = wl_tot_fat.             "Qte. Faturado
      ELSE.
        CONTINUE.
      ENDIF.

      READ TABLE t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr
                                              BINARY SEARCH.

      wa_saida-nro_sol     = wa_0082-nro_sol.                     "Nro. Solicitação
      wa_saida-seq         = wa_0082-seq.                         "Nro. Solicitação
      wa_saida-dt_sol      = wa_0082-dt_sol.                      "Dta. solicitação - SMC 121425
      wa_saida-kunnr       = wa_vbak-kunnr.                       "Cliente
      wa_saida-name1       = wa_kna1-name1.                       "Nome Cliente
      wa_saida-vkbur       = wa_vbak-vkbur.                       "Escr. Venda
      wa_saida-vbeln       = wa_vbap-vbeln.                       "Ordem de Venda
      wa_saida-posnr       = wa_vbap-posnr.                       "Item
      wa_saida-matnr       = wa_vbap-matnr.                       "Material
      wa_saida-matkl       = wa_vbap-matkl.                       "Grupo Mercadoria "*-#187270-07.08.2025-JT-inicio
      wa_saida-arktx       = wa_vbap-arktx.                       "Desc. Material

*     wa_saida-werks       = wa_vbap-werks.                       "Centro
      wa_saida-werks       = wa_0082-werks.                       "Centro
      wa_saida-inco1       = wa_vbkd-inco1.                       "Incoterns
      wa_saida-meins       = wa_vbap-meins.                       "Unid.
      wa_saida-kwmeng      = wa_vbap-kwmeng.                      "Qte Ordem
      wa_saida-tp_oper     = wa_0082-tp_oper.                     "Tipo operacao

*-CS2025000249-01.09.2025-#189440-JT-inicio
      wa_saida-integrou_werks     = COND #( WHEN wa_0082-integrou_werks     = abap_true THEN icon_okay ELSE icon_led_red ).
      wa_saida-integrou_vkbur     = COND #( WHEN wa_0082-integrou_vkbur     = abap_true THEN icon_okay ELSE icon_led_red ).
      wa_saida-integrou_kunnr     = COND #( WHEN wa_0082-integrou_kunnr     = abap_true THEN icon_okay ELSE icon_led_red ).
      wa_saida-integrou_nr_rot    = COND #( WHEN wa_0082-integrou_nr_rot    = abap_true THEN icon_okay ELSE icon_led_red ).
      wa_saida-integrou_nr_rot_pc = COND #( WHEN wa_0082-integrou_nr_rot_pc = abap_true THEN icon_okay ELSE icon_led_red ).
*-CS2025000249-01.09.2025-#189440-JT-fim

      wa_saida-qte_sol     = wa_0082-qte_sol.                     "Qte. Solicitado
      wa_saida-qte_sdo_lib = wa_0082-qte_sol - wl_tot_lib.        "Saldo a Liberar
      wa_saida-qte_sdo_fat = wa_vbap-kwmeng  - wa_saida-qte_fat.  "Saldo a Faturar
      wa_saida-qte_sdo_sol = wa_vbap-kwmeng  - wl_tot_sol.        "Saldo a Solicitar

      wa_saida-vkorg       = wa_0082-vkorg.
      wa_saida-dt_entrega  = wa_0082-dt_entrega.

*#133297 #133302 - Inclusão de campos  | ITSOUZA
      wa_saida-stcd3 = wa_kna1-stcd3.
      wa_saida-ort01 = wa_kna1-ort01.
      wa_saida-regio = wa_kna1-regio.
      wa_saida-cpf_cnpj = COND #( WHEN wa_kna1-stkzn EQ abap_true THEN wa_kna1-stcd2
                                  ELSE wa_kna1-stcd1 ).

      READ TABLE it_zsdt INTO wa_zsdt WITH KEY vbeln =  wa_0082-vbeln BINARY SEARCH..
      IF sy-subrc = 0.
        CLEAR: wa_saida-safra, wa_saida-cultura.

        READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt-doc_simulacao BINARY SEARCH..
        IF sy-subrc = 0.
          wa_saida-safra          = wa_zsdt0040-safra.
          wa_saida-cultura        = wa_zsdt0040-cultura.
          wa_saida-vkbur_fat   = wa_zsdt0040-vkbur.
        ENDIF.
      ENDIF.

      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_vbak-vkbur.
      IF sy-subrc EQ 0.
        wa_saida-name1_fat   = wa_t001w-name1.
      ENDIF.

      READ TABLE it_zsdt0271 INTO wa_zsdt0271 WITH KEY filial = wa_vbak-vkbur.
      IF sy-subrc EQ 0.
        READ TABLE it_zsdt0270 INTO wa_zsdt0270 WITH KEY cod_regional = wa_zsdt0271-cod_regional.
        IF sy-subrc EQ 0.
          wa_saida-cod_regional = wa_zsdt0270-cod_regional.
          wa_saida-regional = wa_zsdt0270-regional.
        ENDIF.
      ENDIF.

      READ TABLE t_0132 INTO wa_0132 WITH KEY nr_rot = wa_0082-nr_rot.
      IF sy-subrc EQ 0.
        wa_saida-rot_desc = wa_0132-rot_desc.
      ENDIF.

*      "FF #145609 - inicio
      READ TABLE t_vbpa WITH KEY vbeln = wa_0082-vbeln INTO DATA(wa_vbpa).
      IF sy-subrc = 0.
        READ TABLE t_trolz WITH KEY azone = wa_vbpa-lzone
                                    lzone = wa_0132-lzone INTO DATA(wa_trolz).

        IF sy-subrc = 0.
          wa_saida-iti_rote = wa_trolz-route.
        ENDIF.
      ENDIF.

*      wa_saida-iti_rote = wa_0082-route_iti.
*      "FF #145609 - fim

      "FF #178787 - inicio
      zcl_util_sd=>identifica_venda_especial(
        EXPORTING
          i_ordem_vendas   = wa_saida-vbeln
          i_setor_ativ     = wa_saida-spart
        IMPORTING
          et_texto         = tg_texto[]
          e_venda_especial = wa_saida-venda_esp
      ).

      IF wa_saida-venda_esp = abap_true.
        wa_saida-t_color = 'C718'. "Cor Laranja
      ENDIF.

      IF tg_texto[] IS NOT INITIAL.
        wa_saida-observacao_ve = icon_display_more.                                "Venda Especial Observação
      ENDIF.

      "FF #178787 - fim


      CASE 'X'.

          "FF #145609 - inicio
        WHEN p_pend.

          IF  wa_0082-desmembrar IS NOT INITIAL.
            APPEND wa_saida TO t_saida.
          ENDIF.
          "FF #145609 - fim.

*-CS2025000249-16.06.2025-#182039-JT-inicio
        WHEN p_proces.
          IF wa_0082-seq EQ 1.
            APPEND wa_saida TO t_saida.
          ENDIF.

        WHEN p_workf.
          IF wa_0082-seq EQ 1.
            READ TABLE it_zsdt0411 INTO wa_zsdt0411 WITH KEY nro_sol = wa_saida-nro_sol
                                                             status  = abap_off.
            IF sy-subrc = 0.
              APPEND wa_saida TO t_saida.
            ENDIF.
          ENDIF.
*-CS2025000249-16.06.2025-#182039-JT-fim

        WHEN p_aberto.

          IF wa_0082-seq EQ 1 AND wa_0082-status EQ 1 AND wl_qte NE 0
          AND wa_0082-desmembrar IS INITIAL AND wa_0082-bloqueio = abap_off. "FF #145609 "*-CS2025000249-16.06.2025-#182039-JT-inicio
            IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL.
              APPEND wa_saida TO t_saida.
            ENDIF.
          ENDIF.

        WHEN p_todas .
          IF  wa_saida-safra IS NOT INITIAL AND  wa_saida-cultura  IS NOT INITIAL
          AND wa_0082-desmembrar IS INITIAL. "FF #145609.
            APPEND wa_saida TO t_saida.
          ENDIF.
      ENDCASE.
    ENDIF.

  ENDLOOP.

  PERFORM lock_table.                                       "FF #171337

ENDFORM.                    " ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprimir_dados .

  DATA:   lv_grid_title TYPE lvc_title.                     "FF #145609
  DATA: wl_layout TYPE slis_layout_alv.
  PERFORM definir_eventos.
  PERFORM montar_layout.

  ADD 1 TO ls_sort-spos.
  ls_sort-fieldname = 'VBELN'.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
  APPEND ls_sort TO lt_sort.

  ADD 1 TO ls_sort-spos.
  ls_sort-fieldname = 'POSNR'.
  ls_sort-up        = 'X'.
  ls_sort-subtot    = 'X'.
  APPEND ls_sort TO lt_sort.

  wl_layout-colwidth_optimize = 'X'.
  wl_layout-box_fieldname     = 'SELECAO'.  "*-CS2025000249-01.09.2025-#189440-JT-inicio

  wl_layout-info_fieldname = 'T_COLOR'.                     "FF #178787

  "FF #145609 - inicio
  CLEAR lv_grid_title.

  CASE 'X'.

    WHEN p_pend.
      lv_grid_title  = 'Pendente Desmembramento'.

    WHEN  p_aberto.
      lv_grid_title = 'Em Aberto para Distribuição'.

    WHEN p_todas.
      lv_grid_title = 'Todas(Exceto Pend. Desmembr.)'.

*-CS2025000249-16.06.2025-#182039-JT-inicio
    WHEN p_proces.
      lv_grid_title = 'Pendentes de Processamento'.

    WHEN p_workf.
      lv_grid_title = 'Pendentes de Aprovação'.
*-CS2025000249-16.06.2025-#182039-JT-fim

  ENDCASE.
  "FF #145609 - fim.

* PERFORM lock_table.                                       "FF #171337

  "FF teste DEV

  IF sy-uname = 'FFONSECA' AND sy-sysid = 'DEV'.
    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs>).

    APPEND INITIAL LINE TO t_saida ASSIGNING <fs>.
    <fs>-t_color = 'C718'. "Cor Laranja
    <fs>-qte_sdo_lib = '80'.
    APPEND INITIAL LINE TO t_saida ASSIGNING <fs>.
    APPEND INITIAL LINE TO t_saida ASSIGNING <fs>.
    APPEND INITIAL LINE TO t_saida ASSIGNING <fs>.
    APPEND INITIAL LINE TO t_saida ASSIGNING <fs>.
  ENDIF.

  "FF teste DEV

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = v_report
      i_grid_title       = lv_grid_title "FF #145609
      it_fieldcat        = estrutura[]
      is_layout          = wl_layout
      i_save             = 'A'
      it_events          = events
      is_print           = t_print
      it_sort            = lt_sort
    TABLES
      t_outtab           = t_saida.


ENDFORM.                    "imprimir_dados

FORM lock_table.

  FREE: it_bloqueio.

*-CS2025000249-04.09.2025-#189901-JT-inicio
*  IF sy-uname = 'JTASSONI' OR sy-uname = 'WPPEREIRA'.
*    RETURN.
*  ENDIF.
*
**FF #171337 - inicio
*  DATA(lv_count) = lines( t_saida ).
*
*  CASE lv_count.
*    WHEN 1.
*
*      READ TABLE t_saida INDEX 1 INTO DATA(wa).
*
*      CALL FUNCTION 'ENQUEUE_EZSDR0038'
*        EXPORTING
*          vkorg          = wa-vkorg
*          spart          = wa-spart
*          werks          = wa-werks
*          matkl          = wa-matkl
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.
*
*      IF sy-subrc <> 0.
*        DATA(l_user_lock) = sy-msgv1.
*
*        DELETE t_saida WHERE vkorg = wa-vkorg
*                         AND spart = wa-spart
*                         AND werks = wa-werks
*                         AND matkl = wa-matkl.
*
*        DATA(l_msg) = |O usuário { l_user_lock } já está executando dados para o filtro Organização de vendas + SetorAtividade + Centro + Grp.Merc.|.
*        MESSAGE l_msg TYPE 'S' DISPLAY LIKE 'E'.
*        EXIT.
*
*      ENDIF.
*
*    WHEN OTHERS.
*
*      LOOP AT t_saida INTO DATA(wa_saida).
*
*        APPEND INITIAL LINE TO it_bloqueio ASSIGNING FIELD-SYMBOL(<fs_bloq>).
*
*        <fs_bloq>-vkorg = wa_saida-vkorg.
*        <fs_bloq>-spart = wa_saida-spart.
*        <fs_bloq>-werks = wa_saida-werks.
*        <fs_bloq>-matkl = wa_saida-matkl.
*      ENDLOOP.
*
*      SORT it_bloqueio BY vkorg spart werks matkl.
*      DELETE ADJACENT DUPLICATES FROM it_bloqueio.
*
**     LOOP AT it_bloqueio ASSIGNING FIELD-SYMBOL(<fs>).
*      LOOP AT it_bloqueio INTO DATA(_bloq).
*        DATA(l_tabix) = sy-tabix.
*
*        CALL FUNCTION 'ENQUEUE_EZSDR0038'
*          EXPORTING
*            vkorg          = _bloq-vkorg
*            spart          = _bloq-spart
*            werks          = _bloq-werks
*            matkl          = _bloq-matkl
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*
*        l_user_lock = sy-msgv1.
*
*        IF sy-subrc <> 0.
*          _bloq-lock_failed = abap_true.
*          MODIFY it_bloqueio FROM _bloq INDEX l_tabix.
*        ENDIF.
*      ENDLOOP.
*
**     LOOP AT it_bloqueio INTO <fs> WHERE lock_failed IS NOT INITIAL.
*      LOOP AT it_bloqueio INTO _bloq WHERE lock_failed IS NOT INITIAL.
*        DELETE t_saida WHERE vkorg = _bloq-vkorg
*                         AND spart = _bloq-spart
*                         AND werks = _bloq-werks
*                         AND matkl = _bloq-matkl.
*
*        MESSAGE s024(sd) WITH 'Nem todos os registros puderam ser exibidos por'
*                              ' estar em processamento por outro(s) usuário(s):'  l_user_lock  DISPLAY LIKE 'E'.
*      ENDLOOP.
*  ENDCASE.
**FF #171337 - fim
*-CS2025000249-04.09.2025-#189901-JT-fim

ENDFORM.

FORM unlock_table.

*-CS2025000249-04.09.2025-#189901-JT-inicio
*FF #171337 - inicio
*  DATA(lv_count) = lines( t_saida ).
*
*  CASE lv_count.
*    WHEN 1.
*
*      READ TABLE t_saida INDEX 1 INTO DATA(wa).
*
*      CALL FUNCTION 'DEQUEUE_EZSDR0038'
*        EXPORTING
*          vkorg          = wa-vkorg
*          spart          = wa-spart
*          werks          = wa-werks
*          matkl          = wa-matkl
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.
*
*    WHEN OTHERS.
*
**     LOOP AT it_bloqueio ASSIGNING FIELD-SYMBOL(<fs>) WHERE lock_failed IS INITIAL.
*      LOOP AT it_bloqueio INTO DATA(_bloq) WHERE lock_failed IS INITIAL.
*
*        CALL FUNCTION 'DEQUEUE_EZSDR0038'
*          EXPORTING
*            vkorg          = _bloq-vkorg
*            spart          = _bloq-spart
*            werks          = _bloq-werks
*            matkl          = _bloq-matkl
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*
*      ENDLOOP.
*  ENDCASE.
*
*FF #171337 - fim
*-CS2025000249-04.09.2025-#189901-JT-fim

ENDFORM.

*"FF #178787 - inicio
*FORM f_busca_observ_venda_espec.
*
*  DATA: lt_cds_result TYPE TABLE OF zi_sd_dados_compl_ov_info,
*        lv_simulador  TYPE zsdt0397-simulador,
*        lv_setor_ativ TYPE zsdt0397-setor_ativ,
*        ls_0397       TYPE zsdt0397.
*
*  CLEAR tg_texto[].
*
*  "Obter dados do CDS para a OV
*  SELECT * FROM zi_sd_dados_compl_ov_info
*    INTO TABLE @lt_cds_result
*    WHERE vbeln = @wa_saida-vbeln.
*
*  IF sy-subrc = 0.
*
*    READ TABLE lt_cds_result INTO DATA(ls_cds_result) INDEX 1.
*    IF sy-subrc = 0.
*
*      lv_simulador  = ls_cds_result-nro_sol.
*      lv_setor_ativ = wa_saida-spart.
*
*      "Buscar na ZSDT0397
*      SELECT SINGLE * FROM zsdt0397 INTO ls_0397
*        WHERE simulador = lv_simulador
*          AND setor_ativ = lv_setor_ativ.
*
*      IF sy-subrc = 0 AND ls_0397-observacao IS NOT INITIAL.
*
*        "Preencher tg_texto com o conteúdo da observação
*        DATA: lv_texto_completo TYPE string,
*              lv_linha_parte    TYPE string,
*              lv_offset         TYPE i,
*              lv_length         TYPE i VALUE 72.
*
*        CLEAR tg_texto[].
*
*        lv_texto_completo = ls_0397-observacao.
*        lv_offset = 0.
*
*        WHILE lv_offset < strlen( lv_texto_completo ).
*
*          DATA(lv_remaining) = strlen( lv_texto_completo ) - lv_offset.
*
*          IF lv_remaining >= lv_length.
*            lv_linha_parte = lv_texto_completo+lv_offset(lv_length).
*          ELSE.
*            lv_linha_parte = lv_texto_completo+lv_offset(lv_remaining).
*          ENDIF.
*
*          tg_texto-tdformat = '*'.
*          tg_texto-tdline   = lv_linha_parte.
*          APPEND tg_texto.
*
*          lv_offset = lv_offset + lv_length.
*
*        ENDWHILE.
*
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*"FF #178787 - fim

*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.

  IF wg_ucomm IS INITIAL.
    PERFORM f_carregar_eventos USING:
                                   slis_ev_user_command  'XUSER_COMMAND', "para tira duplo click
                                   slis_ev_pf_status_set 'XPF_STATUS_SET2',
                                   slis_ev_top_of_page   'XTOP_OF_PAGE'.

  ELSEIF wg_ucomm EQ '&IC1'.
    PERFORM f_carregar_eventos USING:
                                   slis_ev_user_command  'XUSER_COMMAND_POPUP', "para tira duplo click
                                   slis_ev_pf_status_set 'XPF_STATUS_SET'.
*                                   SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE_POPUP'.

  ELSEIF wg_ucomm EQ '&HIST_SOL'.
    PERFORM f_carregar_eventos USING:
                                   slis_ev_user_command  'XUSER_COMMAND_POPUP', "para tira duplo click
                                   slis_ev_pf_status_set 'XPF_STATUS_SET3'.
**                                   SLIS_EV_TOP_OF_PAGE   'XTOP_OF_PAGE_POPUP'.
  ENDIF.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.

  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.


ENDFORM.                    " F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  IF wg_ucomm IS INITIAL.
    PERFORM montar_estrutura USING:
          1  'ZSDT0082'   'INTEGROU_WERKS'   'T_SAIDA' 'INTEGROU_WERKS'   'Centr Int'             ' ' ' ',      "
          2  'ZSDT0082'   'INTEGROU_VKBUR'   'T_SAIDA' 'INTEGROU_VKBUR'   'EscrV Int'             ' ' ' ',      "
          3  'ZSDT0082'   'INTEGROU_KUNNR'   'T_SAIDA' 'INTEGROU_KUNNR'   'Clien Int'             ' ' ' ',      "
          3  'ZSDT0082'   'INTEGROU_NR_ROT'  'T_SAIDA' 'INTEGROU_NR_ROT'  'Rot Int'               ' ' ' ',      "
*         3  'ZSDT0082'   'INTEGROU_NR_ROT_PC' 'T_SAIDA' 'INTEGROU_NR_ROT_PC'  'RotPC Int'        ' ' ' ',      "
          4  'ZSDT0082'   'DT_SOL'           'T_HIST' 'DT_SOL'            'Dt. Solicitada'        ' ' ' ',      "   Dt. Solicitação SMC #121425
          5  'ZSDT0082'   'DT_ENTREGA'       'T_AUX'  'DT_ENTREGA'        'Data Entrega'          ' ' ' ',      "   Dt. Entrega SMC #122112
          6  'NRO_SOL'    'NRO_SOL'          'T_SAIDA' 'NRO_SOL'          'Nro. Solicitação'      ' ' ' ',      "   Nro Sol
          7  'KNA1'       'STCD1'            'T_SAIDA' 'CPF_CNPJ'         'CPF/CNPJ'              ' ' ' ',      "   CPF/CNPJ
          8  'KNA1'       'STCD3'            'T_SAIDA' 'STCD3'            'IE'                    ' ' ' ',      "   Inscrição Estadual
          9  'VBAK'       'KUNNR'            'T_SAIDA' 'KUNNR'            'Cliente'               ' ' ' ',      "   Cliente
         10  'KNA1'       'NAME1'            'T_SAIDA' 'NAME1'            'Nome do Cliente'       ' ' ' ',      "   Nome do Cliente
         11  'KNA1'       'ORT01'            'T_SAIDA' 'ORT01'            'Municipio'             ' ' ' ',      "   Municipio
         12  'KNA1'       'REGIO'            'T_SAIDA' 'REGIO'            'UF'                    ' ' ' ',      "   UF
         13  'VBAK'       'VKBUR'            'T_SAIDA' 'VKBUR'            ' '                     ' ' ' ',      "   Escr. Vendas
         14  'ZSDT0040'   'SAFRA'            'T_SAIDA' 'SAFRA'            'Safra'                 ' ' ' ',      "
         15  'ZSDT0040'   'CULTURA'          'T_SAIDA' 'CULTURA'          'Cultura'               ' ' ' ',      "
         16  'VBAP'       'VBELN'            'T_SAIDA' 'VBELN'            'Ordem de Venda'        ' ' ' ',      "   Contrato
         17  'VBAP'       'POSNR'            'T_SAIDA' 'POSNR'            'Item'                  ' ' ' ',      "   Item
         18  'VBAP'       'MATNR'            'T_SAIDA' 'MATNR'            ' '                     ' ' ' ',      "   Material
         19  'VBAP'       'ARKTX'            'T_SAIDA' 'ARKTX'            'Descrição Material'    ' ' ' ',      "   Descrição Material
         20  'MARA'       'WRKST'            'T_SAIDA' 'WRKST'            'Marca'                 ' ' ' ',      "   Marca
         21  'VBAP'       'WERKS'            'T_SAIDA' 'WERKS'            'Centro'                ' ' ' ',      "   Centro
         22  'VBKD'       'INCO1'            'T_SAIDA' 'INCO1'            ''                      ' ' ' ',      "   Incoterns*****
         23  'VBAP'       'MEINS'            'T_SAIDA' 'MEINS'            'Unid.'                 ' ' ' ',      "   Unid.
         24  'VBAP'       'KWMENG'           'T_SAIDA' 'KWMENG'           'Qte. Ordem'            ' ' ' ',      "   Qte. Ordem****
         25  'VBAP'       'KWMENG'           'T_SAIDA' 'QTE_FAT'          'Qte. Faturado'         ' ' ' ',      "   Qte. Faturado****
         25  'VBAP'       'KWMENG'           'T_SAIDA' 'QTE_SOL'          'Qte. Solicitado'       ' ' ' ',      "   Qte. Solicitado****
         26  'VBAP'       'KWMENG'           'T_SAIDA' 'QTE_SDO_LIB'      'Sdo.a Lib.'            ' ' ' ',      "   Saldo a liberar****
         27  'VBAP'       'KWMENG'           'T_SAIDA' 'QTE_SDO_FAT'      'Sdo.a Fat.'            ' ' ' ',      "   Saldo a Faturar****
*         13  'VBAP'       'KWMENG'           'T_SAIDA' 'QTE_SDO_SOL'      'Sdo.a Sol.'            ' ' ' ',      "   Saldo a Solicitar****
         28  ' '          ' '                'T_SAIDA' 'OBSERVACAO'       'Observação'            ' ' ' ',      "   Observação
         "16  ' '          ' '                'T_SAIDA' 'ROTEIRO'          'Roteiro'               ' ' ' ',      "   Roteiro
         29  'ZSDT0082'   'NR_ROT'           'T_SAIDA' 'NR_ROT'           'N. Roteiro'            ' ' ' ',      "   Roteiro
         30  'ZSDT0132'   'ROT_DESC'         'T_SAIDA' 'ROT_DESC'         'Desc. Roteiro'         ' ' ' ',      "   Descrição do Roteiro

"USER STORY 60440 / ABAP - Anderson Oenning
*         22  'ZSDT0040'   'VKBUR'           'T_SAIDA' 'VKBUR_FAT'        'Centro faturamento'     ' ' ' ',      " Centro faturamento
         31  'T001W   '   'NAME1'           'T_SAIDA' 'NAME1_FAT'        'Desc.esc.venda'            ' ' ' ',    " Descrição escritorio venda filial
         32  'VBAP    '   'LGORT'           'T_SAIDA' 'LGORT'            'Deposito'               ' ' ' ',      " Deposito
         33  'ZSDT0270'   'COD_REGIONAL'    'T_SAIDA' 'COD_REGIONAL'     'Cod.Regional'               ' ' ' ',  " Codigo Regional
         34  'ZSDT0270'   'REGIONAL'        'T_SAIDA' 'REGIONAL'         'Regional'               ' ' ' ',      " Regional
         35  'MARA    '   'MTART   '        'T_SAIDA' 'MTART'            'Tipo material'          ' ' ' ',      " Tipo material
    "USER STORY 60440 / ABAP - Anderson Oenning
         36  'VBAP    '   'ROUTE'           'T_SAIDA' 'ITI_OV'           'Itinerário OV'         ' ' ' ',      " Itinerário OV "FF #145609
         37  '        '   '     '           'T_SAIDA' 'ITI_rote'         'Itinerário base Roteiro'         ' ' ' ',      " Itinerário base Roteiro "FF #145609
         38  ' '          ' '               'T_SAIDA' 'OBSERVACAO_VE'    'Observação Venda Especial'            ' ' ' '.      "Observação Venda Especial FF #178787

  ELSEIF wg_ucomm EQ '&IC1'.
    PERFORM montar_estrutura USING:
          1  'VBAK'             'KUNNR'         'T_AUX' 'KUNNR'          'Cliente'               ' ' ' ',
          2  'KNA1'             'NAME1'         'T_AUX' 'NAME1'          'Nome do Cliente'       ' ' ' ',
          3  'VBAK'             'VKBUR'         'T_AUX' 'VKBUR'          ' '                     ' ' ' ',
          4  'VBAP'             'VBELN'         'T_AUX' 'VBELN'          'Contrato'              ' ' ' ',
          5  'VBAP'             'POSNR'         'T_AUX' 'POSNR'          ' '                     ' ' ' ',
          6  'VBAP'             'MATNR'         'T_AUX' 'MATNR'          ' '                     ' ' ' ',
          7  'VBAP'             'ARKTX'         'T_AUX' 'ARKTX'          'Descrição Material'    ' ' ' ',
          8  'MARA'             'WRKST'         'T_AUX' 'WRKST'          'Marca'                 ' ' ' ',
          9  'VBAP'             'MEINS'         'T_AUX' 'MEINS'          'Unid.'                 ' ' ' ',
         10  'VBAP'             'KWMENG'        'T_AUX' 'QTE_SOL'        'Sdo.a Lib.'            ' ' 'X',
         11  'ZSDT0082'         'DT_ENTREGA'    'T_AUX' 'DT_ENTREGA'     'Data Entrega'          ' ' 'X'.

  ELSEIF wg_ucomm EQ '&HIST_SOL'.
    PERFORM montar_estrutura USING:
          1  'ZSDT0082'       'NRO_SOL'         'T_HIST' 'NRO_SOL'          'Nro. Solicitação'   ' '   ' ',
          2  'ZSDT0082'       'SEQ'             'T_HIST' 'SEQ'              'Seq.'               ' '   ' ',
          3  'ZSDT0082'       'VBELN'           'T_HIST' 'VBELN'            'Nro. OV.'           ' '   ' ',
          4  'ZSDT0082'       'POSNR'           'T_HIST' 'POSNR'            'Item'               ' '   ' ',
          5  'ZSDT0082'       'AUART'           'T_HIST' 'AUART'            'Tp.Ordem '          ' '   ' ',
          6  'ZSDT0082'       'DT_SOL'          'T_HIST' 'DT_SOL'           'Dt. Solicitada'     ' '   ' ',
          7  'ZSDT0082'       'QTE_SOL'         'T_HIST' 'QTE_SOL'          'Qte. Solicitada'    ' '   ' ',
          8  'ZSDT0082'       'USUARIO_SOL'     'T_HIST' 'USUARIO_SOL'      'Usuário Sol.'       ' '   ' ',
          9  'ZSDT0082'       'DT_LIBER'        'T_HIST' 'DT_LIBER'         'Dt. Liberação'      ' '   ' ',
         10  'ZSDT0082'       'QTE_LIB'         'T_HIST' 'QTE_LIB'          'Qte. Liberada'      ' '   ' ',
         11  'ZSDT0082'       'USUARIO_LIB'     'T_HIST' 'USUARIO_LIB'      'Usuário Lib.'       ' '   ' ',
         12  ''               ''                'T_HIST' 'DESC_STATUS'      'Situação'           ' '   ' ',
         13  'ZSDT0082'       'DT_CANC'         'T_HIST' 'DT_CANC'          'Dt. Canc.'          ' '   ' ',
         14  'ZSDT0082'       'USER_CANC'       'T_HIST' 'USER_CANC'        'Usuário Canc.'      ' '   ' '.
  ENDIF.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit).

  CLEAR: wa_estrutura.

  IF p_field EQ 'QTE_SDO_LIB'
    AND wg_ucomm IS INITIAL.
    wa_estrutura-hotspot = 'X'.
  ELSEIF p_field EQ 'NR_ROT'
    AND wg_ucomm IS INITIAL.
    wa_estrutura-hotspot = 'X'.
* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio
  ELSEIF p_field EQ 'MATNR'
  AND wg_ucomm IS INITIAL.
    wa_estrutura-hotspot = 'X'.
* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim
  ELSE.
    wa_estrutura-edit        = p_edit.
  ENDIF.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.


  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

*-CS2025000249-01.09.2025-#189440-JT-inicio
  IF p_field EQ 'INTEGROU_WERKS' OR
     p_field EQ 'INTEGROU_VKBUR' OR
     p_field EQ 'INTEGROU_KUNNR'.
    wa_estrutura-icon = abap_true.
  ELSE.
    wa_estrutura-icon = abap_false.
  ENDIF.
*-CS2025000249-01.09.2025-#189440-JT-fim

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  "FF #145609 - inicio
  IF p_field = 'QTE_SDO_LIB' "Saldo a liberar
  AND p_pend = abap_true.

*    wa_estrutura-no_out = 'X'.


    "FF #145609 - fim
  ELSE.

    APPEND wa_estrutura TO estrutura.

  ENDIF.

ENDFORM.                    " MONTAR_ESTRUTURA

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.

  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.


ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaveis.

  v_report = sy-repid.

  PERFORM f_construir_cabecalho USING 'H' TEXT-003.

ENDFORM.                    " INICIAR_VARIAVES
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xpf_status_set2 USING ucomm TYPE kkblo_t_extab.        "#EC CALLED

  DATA: gr_events       TYPE REF TO lcl_event_receiver,
        ls_sel_hide     TYPE slis_sel_hide_alv,
        ref1            TYPE REF TO cl_gui_alv_grid,
        it_fieldcatalog TYPE lvc_t_fcat,
        wa_fieldcatalog TYPE lvc_s_fcat,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = ref1.

  CALL METHOD ref1->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  LOOP AT it_fieldcatalog INTO wa_fieldcatalog
    WHERE fieldname EQ 'OBSERVACAO'
       OR fieldname EQ 'ROTEIRO'
       OR fieldname = 'OBSERVACAO_VE'                       "FF #178787
       OR fieldname = 'DESC_ROT'.                           "FF #178787

    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
    MODIFY it_fieldcatalog FROM wa_fieldcatalog.
  ENDLOOP.

  CALL METHOD ref1->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcatalog.


  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD ref1->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  IF init IS INITIAL.
    CREATE OBJECT gr_events.
    SET HANDLER gr_events->handle_on_button_click FOR ref1.
    init = 'X'.
  ENDIF.

  "FF #145609 - inicio
  IF p_pend = abap_true.
    DATA lt_fcode TYPE TABLE OF syucomm.

    APPEND 'CHANGE_PLN' TO lt_fcode.

  ENDIF.

  IF p_aberto = abap_true OR p_todas = abap_true.

    APPEND 'DESMEMBRAR' TO lt_fcode.

  ENDIF.



*  SET PF-STATUS 'STANDARD_FULLSCREEN'." EXCLUDING TL_FCODE.
  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING lt_fcode.
  "FF #145609 - fim

ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xpf_status_set USING ucomm TYPE kkblo_t_extab.         "#EC CALLED
  DATA: tl_fcode TYPE TABLE OF sy-ucomm.
  LOOP AT t_aux TRANSPORTING NO FIELDS WHERE qte_sol IS NOT INITIAL.
  ENDLOOP.
  IF sy-subrc IS NOT INITIAL.
    APPEND '&LIB_EMBAR' TO tl_fcode.
  ENDIF.

*  TL_FCODE = VALUE #(
*                      ( 'CANCELAR' )
*                      ( 'EXCEL' )
*                    ).

  APPEND 'CANCELAR' TO tl_fcode.
  APPEND 'EXCEL'   TO tl_fcode.

  SET PF-STATUS 'STDPOPUP_FULLSCREEN' EXCLUDING tl_fcode.

ENDFORM. "XPF_STATUS_SET
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xpf_status_set3 USING ucomm TYPE kkblo_t_extab.        "#EC CALLED
  DATA: tl_fcode TYPE TABLE OF sy-ucomm.

  LOOP AT t_aux TRANSPORTING NO FIELDS WHERE werks EQ '0175' AND mtart EQ 'ZFER'.
    APPEND 'CANCELAR' TO tl_fcode.
  ENDLOOP.


*  TL_FCODE = VALUE #(
*                      ( '&LIB_EMBAR' )
*                      ( '&HIS_SOL' )
*                      ( 'EXCEL' )
*                    ).

  APPEND '&LIB_EMBAR' TO tl_fcode.
  APPEND '&HIS_SOL'   TO tl_fcode.
  APPEND 'EXCEL'      TO tl_fcode.
  APPEND '&ENV_EMAIL' TO tl_fcode.



  SET PF-STATUS 'STDPOPUP_FULLSCREEN' EXCLUDING tl_fcode.

ENDFORM. "XPF_STATUS_SET

FORM status_01 USING ucomm TYPE kkblo_t_extab.              "#EC CALLED
  DATA: tl_fcode TYPE TABLE OF sy-ucomm.

  SET PF-STATUS 'STATUS_01' EXCLUDING tl_fcode.

ENDFORM. "XPF_STATUS_SET

*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xuser_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield..     "#EC CALLED

  DATA: it_sel_rows TYPE lvc_t_row,
        wa_sel_rows TYPE lvc_s_row.

  DATA: wl_qte       TYPE zsdt0082-qte_sol,
        wl_index(4),
        i_grid_title TYPE  lvc_title,
        wl_layout    TYPE slis_layout_alv,
        wl_tot_lib   TYPE zsdt0082-qte_lib,
        wl_tot_sol   TYPE zsdt0082-qte_sol,
        wl_0082      LIKE LINE OF t_0082.

  "----

  DATA: tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab,
        wl_field TYPE lvc_s_col,
        wl_name  TYPE thead-tdname,
        t_texto  TYPE TABLE OF tline,
        w_texto  TYPE tline.

  DATA: ls_sel_hide TYPE slis_sel_hide_alv,
        ref1        TYPE REF TO cl_gui_alv_grid,
        is_table    TYPE lvc_s_stbl.

  DATA: zcl_obj_manutencao_insumos TYPE REF TO zcl_manutencao_insumos.
  CREATE OBJECT zcl_obj_manutencao_insumos.
  DATA: vg_check_erro TYPE char01.

  DATA: ls_manutencao TYPE zde_manutencao.

  FREE fcat.

  PERFORM montar_estrutura USING:
  16 '' '' 'T_SAIDA' 'DESROTEIRO' 'Roteiro' '' ''.

  fcat = estrutura[].

  REFRESH: t_top_aux,
           events,
           estrutura,
           t_aux.

  wg_ucomm = ucomm.

  CASE ucomm.
    WHEN 'HIS_CG_QTD'.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR' "<-- Gets the reference of ALV grid object from REUSE functions
        IMPORTING
          e_grid = ref1.

      CHECK ref1 IS NOT INITIAL.

      CALL METHOD ref1->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE  'Selecione uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows[] ) > 1.
        MESSAGE  'Selecione somente uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

      READ TABLE t_saida INTO wa_saida INDEX wa_sel_rows-index.

      CHECK sy-subrc EQ 0.

      SELECT *
        FROM zsdt0150 INTO TABLE @DATA(it_zsdt0150)
       WHERE nro_sol EQ @wa_saida-nro_sol
         AND vbeln   EQ @wa_saida-vbeln
         AND posnr   EQ @wa_saida-posnr.

      CHECK it_zsdt0150[] IS NOT INITIAL.

      SORT it_zsdt0150 BY dt_registro hr_registro.

      PERFORM f_montar_layout_log_qtde.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          it_fieldcat           = estrutura[]
          i_save                = 'A'
          i_screen_start_column = 3
          i_screen_start_line   = 3
          i_screen_end_column   = 100
          i_screen_end_line     = 13
        TABLES
          t_outtab              = it_zsdt0150.


    WHEN 'CHANGE_QTD'.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR' "<-- Gets the reference of ALV grid object from REUSE functions
        IMPORTING
          e_grid = ref1.

      CHECK ref1 IS NOT INITIAL.

      CALL METHOD ref1->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE  'Selecione uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows[] ) > 1.
        MESSAGE  'Selecione somente uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

      READ TABLE t_saida INTO wa_saida INDEX wa_sel_rows-index.

      CHECK sy-subrc EQ 0.

*-US191034-17.09.2025-#191034-JT-inicio
*---------------------------------------------
*---- check se solicitacao desta OV esta bloqueada/pendente na distribuicao (zsdt0081)
*---------------------------------------------
      SELECT SINGLE *
        FROM zsdt0082 INTO @DATA(_0082)
       WHERE nro_sol = @wa_saida-nro_sol
         AND vbeln   = @wa_saida-vbeln
         AND posnr   = @wa_saida-posnr
         AND seq     = '001'.

      IF sy-subrc = 0 AND _0082-bloqueio = abap_true.
        MESSAGE s024(sd) WITH 'Solicitação está Pendente na Distribuicao!!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
*-US191034-17.09.2025-#191034-JT-inicio

      CLEAR: wg_qtde_sol_new.

      wg_qtde_sol_new = wa_saida-qte_sol.

      CALL SCREEN 0105 STARTING AT 30 05 ENDING AT 68 1.

      PERFORM selecionar_dados.
      PERFORM organizar_dados.
      CALL METHOD ref1->refresh_table_display
        EXPORTING
          is_stable = w_stable.

    WHEN '&IC1'. "Double click "Duplo clique

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio
      DATA: lv_error_sisdev TYPE boolean,
            lv_error_indea  TYPE boolean.
* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim

      CHECK p_pend IS INITIAL. "FF #168675 - inicio - Não mostrar popup se estiver na visão "Pendente de DESMEMBRAR"

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio
      IF selfield-fieldname EQ 'MATNR'.

        READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
        IF sy-subrc IS INITIAL.


          SET PARAMETER ID 'MXX' FIELD 'K'.
          SET PARAMETER ID 'MAT' FIELD wa_saida-matnr. "<- set material number
          CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

        ENDIF.


* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim
      ELSEIF selfield-fieldname NE 'NR_ROT'.

        CLEAR:wg_index,wa_saida_aux.

        READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
        IF sy-subrc IS INITIAL.

          LOOP AT t_saida INTO wa_saida_aux WHERE vbeln EQ wa_saida-vbeln AND
                                                  posnr EQ wa_saida-posnr AND
                                                  nro_sol EQ wa_saida-nro_sol AND
                                                  seq     EQ wa_saida-seq.

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio
            IF wa_saida-spart EQ '03' OR wa_saida-spart EQ '04'.
*              perform valida_sisdev using wa_saida_aux
*                                    changing lv_error_sisdev.

              zcl_obj_manutencao_insumos->check_cadastro_forn_sisdev(
                EXPORTING
                  i_werks = wa_saida-werks " Centro
                  i_kunnr = wa_saida-kunnr " Nº cliente
                IMPORTING
                  e_erro  = vg_check_erro                 " Campo de texto do comprimento 1
              ).

              IF  vg_check_erro IS NOT INITIAL.
                lv_error_sisdev = vg_check_erro.
              ENDIF.
            ENDIF.

            IF lv_error_sisdev = abap_false.

**==================================================Retirado a validação / AOENNING / 28-05-2025
*              PERFORM valida_clas_indea USING wa_saida_aux
*                                  CHANGING lv_error_indea.
**==================================================Retirado a validação / AOENNING / 28-05-2025
              IF lv_error_indea = abap_false.

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim
                READ TABLE it_tp_oper INTO wa_tp_oper WITH KEY domvalue_l = wa_saida_aux-tp_oper
                                                               ddlanguage = sy-langu.

                MOVE: wa_saida_aux-kunnr       TO wa_aux-kunnr,
                      wa_saida_aux-name1       TO wa_aux-name1,
                      wa_saida_aux-vkbur       TO wa_aux-vkbur,
                      wa_saida_aux-vbeln       TO wa_aux-vbeln,
                      wa_saida_aux-posnr       TO wa_aux-posnr,
                      wa_saida_aux-matnr       TO wa_aux-matnr,
                      wa_saida_aux-arktx       TO wa_aux-arktx,
                      wa_saida_aux-wrkst       TO wa_aux-wrkst,
                      wa_saida_aux-werks       TO wa_aux-werks,
                      wa_saida_aux-meins       TO wa_aux-meins,
                      wa_saida_aux-vkorg       TO wa_aux-vkorg,
                      wa_saida_aux-nro_sol     TO wa_aux-nro_sol,
                      wa_saida_aux-seq         TO wa_aux-seq,
                      wa_saida_aux-dt_entrega  TO wa_aux-dt_entrega,
                      wa_saida_aux-qte_sdo_lib TO wa_aux-qte_sol,
                      wa_saida_aux-qte_sdo_lib TO wa_aux-qte_sol2,
                      wa_saida_aux-mtart       TO wa_aux-mtart,
                      wa_saida_aux-nr_rot      TO wa_aux-nr_rot,
                      wa_saida_aux-lgort       TO wa_aux-lgort,
                      wa_saida_aux-inco1       TO wa_aux-inco1,
                      wa_saida_aux-tp_oper     TO wa_aux-tp_oper,    "*-#187270-07.08.2025-JT-inicio
                      wa_tp_oper-ddtext        TO wa_aux-tp_oper_desc,    "*-#187270-07.08.2025-JT-inicio
                "FF #169501 - inicio
                      wa_saida_aux-wrkst       TO wa_aux-wrkst,
                      wa_saida_aux-qte_sdo_lib TO wa_aux-qte_sdo_lib,
                      wa_saida_aux-qte_sol     TO wa_aux-qte_sol,
                      wa_saida_aux-ort01       TO wa_aux-ort01,
                      wa_saida_aux-regio       TO wa_aux-regio,
                      wa_saida_aux-spart       TO wa_aux-spart.  "*-US191954-30.09.2025-#191954-JT
                "FF #169501 - fim

                "FF #178787 - inicio
                zcl_util_sd=>identifica_venda_especial(
                  EXPORTING
                    i_ordem_vendas   = wa_saida_aux-vbeln
                    i_setor_ativ     = wa_saida_aux-spart
                  IMPORTING
                    e_venda_especial = wa_aux-venda_esp
                ).

                IF wa_aux-observacao_ve IS INITIAL.
                  wa_aux-observacao_ve = icon_enter_more.
                ENDIF.

                IF wa_aux-venda_esp = abap_true.
                  wa_aux-observacao_ve = icon_display_more.
                ENDIF.

                wa_aux-desc_rot = icon_display_more.

                "FF #178787 - fim

                "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
                SELECT SINGLE lifnr
                  FROM vbpa INTO wa_aux-lifnr_pc
                 WHERE vbeln EQ wa_saida_aux-vbeln
                   AND parvw EQ 'PC'.
                "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

                APPEND wa_aux TO t_aux.

              ENDIF.

            ENDIF.
          ENDLOOP.



* "CS2025000249 - Projetos insumos 25 - 14-05-25 - inicio
          IF lv_error_sisdev = abap_true.
            MESSAGE 'Cliente sem cadastro SISDEV, verificar' TYPE 'S' DISPLAY LIKE 'I'.
            RETURN.
**==================================================Retirado a validação / AOENNING / 28-05-2025
*          ELSEIF lv_error_indea = abap_true.
*            MESSAGE 'Id cultivar indea não esta vinculado na caracteristica do material' TYPE 'S' DISPLAY LIKE 'I'.
*            RETURN.
**==================================================Retirado a validação / AOENNING / 28-05-2025
          ENDIF.

* "CS2025000249 - Projetos insumos 25 - 14-05-25 - Fim

*-CS2025000249-16.06.2025-#182039-JT-inicio
*         SELECT COUNT(*)
*            FROM mara
*            WHERE matnr = wa_saida-matnr
*               AND spart = '04'.  "sementes
*         IF sy-subrc = 0 AND selfield-fieldname = 'QTE_SDO_LIB'.
          IF wa_saida-spart = '04' AND selfield-fieldname = 'QTE_SDO_LIB'.    "sementes
            IF wa_saida-qte_sdo_lib >= 0.
*             PERFORM f_dados_alv1_108.
*             PERFORM f_dados_alv2_108.
*-CS2025000249-16.06.2025-#182039-JT-fim

*-CS2025000249-04.09.2025-#189901-JT-inicio
              PERFORM f_lock_cultivar      USING wa_saida-vkorg wa_saida-matnr
                                        CHANGING lv_erro.
              IF lv_erro = abap_false.
                PERFORM f_dados_alv1_108.
                PERFORM f_dados_alv2_108 USING abap_off.
                CALL SCREEN 0108.
                PERFORM f_unlock_cultivar  USING wa_saida-vkorg wa_saida-matnr.
              ELSE.
                EXIT.
              ENDIF.
*-CS2025000249-04.09.2025-#189901-JT-fim

*-CS2025000249-16.06.2025-#182039-JT-inicio
              PERFORM selecionar_dados.
              PERFORM organizar_dados.
            ENDIF.
*-CS2025000249-16.06.2025-#182039-JT-fim
          ELSE.

*-US191954-30.09.2025-#191954-JT-inicio
            IF wa_saida-spart = '03' AND selfield-fieldname = 'QTE_SDO_LIB'.    "defensivos
              IF wa_saida-qte_sdo_lib >= 0.
                PERFORM f_dados_alv1_108.
                PERFORM f_dados_alv2_108    USING abap_off.
                PERFORM f_tratar_defensivos USING abap_true.
                CALL SCREEN 0108.
                PERFORM f_debloqueio_defensivos.
              ENDIF.
*-US191954-30.09.2025-#191954-JT-fim
            ELSE.
              PERFORM definir_eventos.
              PERFORM montar_layout.

*     Remove a Edição dos campos quando QTE_SOL for ZERO
              IF wa_aux-qte_sol EQ 0.
                LOOP AT estrutura ASSIGNING FIELD-SYMBOL(<estrutura>) WHERE edit EQ abap_true.
                  MOVE abap_false TO <estrutura>-edit.
                ENDLOOP.
              ENDIF.

              wl_layout-box_fieldname     = 'MARK'.
              wl_layout-box_tabname       = 'T_AUX'.
              wl_layout-colwidth_optimize = 'X'.
              CLEAR: wg_ucomm.

              DATA _variant TYPE disvariant.
              _variant-report = 'X'.

              CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
                EXPORTING
                  i_callback_program    = v_report
                  it_fieldcat           = estrutura[]
                  is_layout             = wl_layout
                  is_variant            = _variant
                  i_save                = 'A'
                  it_events             = events
                  is_print              = t_print
                  i_screen_start_column = 2
                  i_screen_start_line   = 2
                  i_screen_end_column   = 140
                  i_screen_end_line     = 10
                  i_grid_title          = 'Distribuição de Embarque'
                TABLES
                  t_outtab              = t_aux.

              PERFORM selecionar_dados.
              PERFORM organizar_dados.

            ENDIF.
*-US191954-30.09.2025-#191954-JT-fim
          ENDIF.
        ENDIF.

        selfield-refresh = 'X'.
        selfield-row_stable = 'X'.
        selfield-col_stable = 'X'.

      ELSE.

        REFRESH: tl_texto, tg_texto.
        CLEAR:wl_texto, wa_textos_aux.

        READ TABLE t_saida INTO wa_saida INDEX selfield-tabindex.
        REFRESH: t_textos_aux.

        LOOP AT t_0082 INTO  wa_0082
           WHERE vbeln EQ wa_saida-vbeln
             AND posnr EQ wa_saida-posnr
             AND nro_sol EQ wa_saida-nro_sol
             AND qte_sol IS NOT INITIAL.

          MOVE: wa_0082-nro_sol TO wa_textos_aux-nro_sol,
                wa_0082-seq     TO wa_textos_aux-seq,
                'ROTE' TO wa_textos_aux-id.

          APPEND wa_textos_aux  TO t_textos_aux.
        ENDLOOP.

        READ TABLE t_textos_aux INTO wa_textos_aux INDEX wg_index.
        CLEAR: wl_name.
        CONCATENATE wa_textos_aux-nro_sol wa_textos_aux-seq INTO wl_name.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = wa_textos_aux-id
            language                = sy-langu
            name                    = wl_name
            object                  = 'ZTEXTO'
          TABLES
            lines                   = tg_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        IF tg_texto[] IS INITIAL.

          PERFORM read_text_roteiro USING selfield-value.

          LOOP AT tg_texto INTO w_texto.
            MOVE: w_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.

        ELSE.

          LOOP AT tg_texto INTO w_texto.
            MOVE: w_texto-tdline TO wl_texto.
            APPEND wl_texto TO tl_texto.
            CLEAR: wl_texto.
          ENDLOOP.

        ENDIF.

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Roteiro'
            im_display_mode = abap_true
          CHANGING
            ch_text         = tl_texto.

      ENDIF.

    WHEN 'EXCEL'.
      PERFORM gera_excel.

      "FF #145609 - inicio
    WHEN 'DESMEMBRAR'.
      "FF #172796 - inicio
      DATA: lv_new_ov         TYPE vbeln,
            lv_solic_emb      TYPE zde_nro_sol,
            lv_tb_saida_vazia.
      "FF #172796 - fim
*      DATA: ls_smesg TYPE smesg.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = ref1.

      CHECK ref1 IS NOT INITIAL.

      CALL METHOD ref1->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE  'Selecione uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows[] ) > 1.
        MESSAGE  'Selecione somente uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      _saida = t_saida[ it_sel_rows[ 1 ]-index ].

* "// Metodo de Desmenbramento da ZSDT0087 WBARBOSA INICIO

*      CALL METHOD ZCL_MANUTENCAO_INSUMOS=>GET_ROTA
*        EXPORTING
*          I_PC      = |{ _SAIDA-WERKS ALPHA = IN }|
*        IMPORTING
*          E_ROTEIRO = DATA(E_ROTEIRO).

      ls_manutencao =
      VALUE #(
                vbeln_old = VALUE #(
                                      vbeln = _saida-vbeln
                                      posnr = _saida-posnr
                                   )
                vbeln_new = VALUE #(
                                      werks = _saida-werks
                                      matnr = _saida-matnr
                                      lgort = _saida-lgort
                                      quantidade = _saida-qte_sol
*                                      NR_ROT_LR = _SAIDA-NR_ROT
*                                      NR_ROT_PC = E_ROTEIRO
                                      itinerario = _saida-iti_rote
                                      kunnr = _saida-kunnr
                                   )
             ).

      CALL METHOD zcl_manutencao_insumos=>run
        EXPORTING
          i_manutencao  = ls_manutencao
          is_background = abap_true
        IMPORTING
          e_vbeln       = lv_new_ov
          e_posnr       = DATA(e_posnr)
          r_return      = te_return.

* "// Metodo de Desmenbramento da ZSDT0087 WBARBOSA FIM

* "// Comentado para incluir Metodo de Desmenbramento da ZSDT0087 WBARBOSA INICIO

*      EXPORT _SAIDA-QTE_SOL     FROM _SAIDA-QTE_SOL     TO MEMORY ID 'memory_qte_sol'. "Import será feito na transação ZSDT0087
*      EXPORT _SAIDA-KUNNR       FROM _SAIDA-KUNNR       TO MEMORY ID 'memory_kunnr'. "Import será feito na transação ZSDT0087
*      EXPORT _SAIDA-ITI_OV      FROM _SAIDA-ITI_OV      TO MEMORY ID 'memory_iti_ov'. "Import será feito na transação ZSDT0087
*      EXPORT _SAIDA-ITI_ROTE    FROM _SAIDA-ITI_ROTE    TO MEMORY ID 'memory_iti_rote'. "Import será feito na transação ZSDT0087/ZSDT0079
*
*      EXPORT _SAIDA-NR_ROT      FROM _SAIDA-NR_ROT      TO MEMORY ID 'memory_roteiro'. "Import será feito na transação ZSDT0079
*      EXPORT _SAIDA-DT_ENTREGA  FROM _SAIDA-DT_ENTREGA  TO MEMORY ID 'memory_dt_entrega'. "Import será feito na transação ZSDT0079
*      EXPORT _SAIDA-QTE_SOL     FROM _SAIDA-QTE_SOL     TO MEMORY ID 'memory_qte_sol'. "Import será feito na transação ZSDT0079
*      EXPORT _SAIDA-QTE_SDO_LIB FROM _SAIDA-QTE_SDO_LIB TO MEMORY ID 'memory_qte_sdo_lib'. "Import será feito na transação ZSDT0079
*
*      PERFORM F_MONTA_SHDB.
*      PERFORM F_CALL_ZSDT0087. "Desmembrar/Criar Ordem de vendas

      "FF #172796 - inicio
*      IMPORT I_VBELN  TO LV_NEW_OV FROM MEMORY ID 'memory_i_vbeln'. "Export feito no programa ZSDR0042  "FF #145609
*      IMPORT LV_TB_SAIDA_VAZIA TO LV_TB_SAIDA_VAZIA FROM MEMORY ID 'memory_tb_saida_vazia'. "Export feito na transação ZSDT0087, programa ZSDR0042
*
*      IF LV_TB_SAIDA_VAZIA IS NOT INITIAL. "Se lv_tb_saida_vazia estiver com X, quer dizer que a tabela it_saida do programa ZSDR0042 está vazia.
*
*
*        DATA(LV_MSG1) = |{ 'Ov: ' }| && |{ _SAIDA-VBELN ALPHA = OUT } | && |{ 'não está disponível para o usuário realizar a ação.' }|.
*
*        IF LV_NEW_OV IS INITIAL.
*          DATA(LV_MSG2) = 'Verificar direto na transação ZSDT0087. Ov não criada'.
*        ENDIF.
*
*        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
*          EXPORTING
*            TITEL          = 'Mensagem'
*            TEXTLINE1      = LV_MSG1
*            TEXTLINE2      = LV_MSG2
*          EXCEPTIONS
*            TEXT_NOT_FOUND = 1
*            OTHERS         = 2.
*
*        EXIT.
*
*      ENDIF.
      "FF #172796 - fim

*FF #171337 - inicio
      "A tabela te_return virá com o campo "type" = E caso tenha dado erro na criação da OV e a tabela TL_SAIDA_EXEC tem a mensagem de erro gerada.
*      IMPORT TL_SAIDA_EXEC TO TL_SAIDA_EXEC FROM MEMORY ID 'memory_tl_saida_exec'. "export feito na função ZSDMF001_GERA_OV_COCKPIT (Esta função é chamada no ZSDR0042)
*      IMPORT TE_RETURN TO TE_RETURN FROM MEMORY ID 'memory_te_return'. "export feito na função ZSDMF001_GERA_OV_COCKPIT (Esta função é chamada no ZSDR0042)
* "// Comentado para incluir Metodo de Desmenbramento da ZSDT0087 WBARBOSA FIM

      READ TABLE te_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.

      IF sy-subrc = 0.

        TYPES: BEGIN OF ty_itab ,
                 erro_msg(255) TYPE c,
               END OF ty_itab.

        DATA itab_msg TYPE TABLE OF ty_itab.

        DATA: lv_title TYPE char80.

        lv_title = 'Erros encontrados'.

        " Preencher a tabela lt_text com as mensagens de erro
        LOOP AT te_return INTO DATA(ls_error).
          APPEND ls_error-message TO itab_msg.
        ENDLOOP.

        " Chamar o popup para exibir as mensagens
        CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
          EXPORTING
            endpos_col   = 60
            endpos_row   = 10
            startpos_col = 10
            startpos_row = 5
            titletext    = lv_title
          TABLES
            valuetab     = itab_msg
          EXCEPTIONS
            break_off    = 1
            OTHERS       = 2.

      ELSE.

        CHECK lv_new_ov IS NOT INITIAL.                     "FF #172796
*FF #171337 - fim

        " Chama o método "cancelar_ordem_vendas" diretamente com a instância criada por NEW (Mesmo cancelar da ZSDT0079)
        DATA(lv_msg) = NEW zcl_util_sd( )->cancelar_ordem_vendas(
          EXPORTING
            i_nro_sol = _saida-nro_sol
            i_vbeln     = _saida-vbeln
            i_posnr     = _saida-posnr
        ).

        PERFORM f_submit_zsdr0037 USING lv_new_ov _saida-nr_rot. "Solicitação de Embarque. Transação --> ZSDT0079

        IMPORT p_number TO lv_solic_emb FROM MEMORY ID 'memory_solic_emb'. "Export feito no programa ZSDR0037  "FF #145609

        DATA(lv_line1) = |{ 'Nova Ordem de Vendas foi criada com número: ' }| && |{ lv_new_ov }|.
        DATA(lv_line2) = |{ 'A Solicitação de embarque foi criada com número: ' }| && |{ lv_solic_emb }|.

        IF lv_solic_emb IS INITIAL.
          lv_line2 = 'Erro ao criar Solicitação de Embarque'.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
          EXPORTING
            titel          = 'Mensagem'
            textline1      = lv_line1
            textline2      = lv_line2
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

      ENDIF.

      "Refresh do ALV....
      PERFORM selecionar_dados.
      PERFORM organizar_dados.

      selfield-refresh = 'X'.
      selfield-row_stable = 'X'.
      selfield-col_stable = 'X'.


      "FF #145609 - fim

    WHEN 'CHANGE_PLN'.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = ref1.

      CHECK ref1 IS NOT INITIAL.

      CALL METHOD ref1->get_selected_rows
        IMPORTING
          et_index_rows = it_sel_rows.

      IF it_sel_rows[] IS INITIAL.
        MESSAGE  'Selecione uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      IF lines( it_sel_rows[] ) > 1.
        MESSAGE  'Selecione somente uma linha!' TYPE 'S'.
        EXIT.
      ENDIF.

      _saida = t_saida[ it_sel_rows[ 1 ]-index ].

      PERFORM altera_qtd_pln.

      PERFORM selecionar_dados.
      PERFORM organizar_dados.

      selfield-refresh = 'X'.
      selfield-row_stable = 'X'.
      selfield-col_stable = 'X'.

*-CS2025000249-01.09.2025-#189440-JT-inicio
    WHEN 'REFRESH'.
      PERFORM selecionar_dados.
      PERFORM organizar_dados.
      selfield-refresh    = 'X'.
      selfield-row_stable = 'X'.
      selfield-col_stable = 'X'.

    WHEN 'ENVIAR_SAFRA'.
      PERFORM f_enviar_safra.
      selfield-refresh    = 'X'.
      selfield-row_stable = 'X'.
      selfield-col_stable = 'X'.
*-CS2025000249-01.09.2025-#189440-JT-fim

      " 25.09.2025 -- RAMON -->
    WHEN 'CANC_SOLI'.
      PERFORM f_cancela_solicitacao.

    WHEN 'CALL_87'.
      PERFORM f_call_zsdt0087.
      " 25.09.2025 -- RAMON --<

  ENDCASE.

ENDFORM. "XUSER_COMMAND


FORM user_command_01.
  BREAK-POINT.
ENDFORM.
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND_POPUP                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xuser_command_popup USING ucomm    LIKE sy-ucomm
                               selfield TYPE kkblo_selfield.. "#EC CALLED

  DATA: i_grid_title TYPE  lvc_title,
        wl_layout    TYPE slis_layout_alv,
        tl_vbep      TYPE TABLE OF vbep WITH HEADER LINE,
        tl_0082      TYPE TABLE OF zsdt0082,
        wl_0082      TYPE zsdt0082,
        wb_0082      TYPE zsdt0082,
        wl_index(4),
        wl_hist      TYPE ty_hist,
        v_seq        TYPE zsdt0082-seq,
        v_seq_lib    TYPE zsdt0082-seq_lib,
        valida       TYPE char1,
        f_headinx    LIKE bapisdh1x,
        tl_return    TYPE TABLE OF bapiret2   WITH HEADER LINE.

  DATA: BEGIN OF i_order_item_in OCCURS 0.
          INCLUDE STRUCTURE bapisditm.
  DATA: END   OF i_order_item_in.

  DATA: BEGIN OF i_order_item_inx OCCURS 0.
          INCLUDE STRUCTURE bapisditmx.
  DATA: END   OF i_order_item_inx.

  DATA: BEGIN OF i_sched OCCURS 10.
          INCLUDE STRUCTURE bapischdl.
  DATA: END OF i_sched.

  DATA: BEGIN OF i_schedx OCCURS 10.
          INCLUDE STRUCTURE bapischdlx.
  DATA: END OF i_schedx.

  DATA: lc_dados                 TYPE zmme_dados_safra,
        lo_order_itens           TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
        l_msg_error              TYPE string,
        ls_retorno_consulta_item TYPE zsds392,
        ls_order_itens           TYPE zde_safra_control_ordem_itens,
        lc_insumos               TYPE REF TO zcl_distribuicao_insumos.

  REFRESH: tl_0082.
  CLEAR: wl_0082, wl_index.

  SORT t_0082_lib BY seq.

  FREE:  lc_insumos, lo_order_itens.         "*-CS2025000249-04.07.2025-#181842-JT
  CREATE OBJECT: lc_insumos, lo_order_itens. "*-CS2025000249-04.07.2025-#181842-JT

  wg_ucomm = ucomm.
  CASE ucomm.
    WHEN '&LIB_EMBAR'.
      wg_ucomm = '&LIB_EMB'.
      BREAK abap.

      SORT t_aux BY nro_sol vbeln posnr.

      CLEAR wa_aux.

      LOOP AT t_aux INTO wa_aux  WHERE mark IS NOT INITIAL.
        CLEAR: v_seq, v_seq_lib.

* "// wbarbosa 18/07/2025 US-168932
        CALL METHOD zcl_manutencao_insumos=>chk_id_propriedade
          EXPORTING
            i_vbeln = wa_aux-vbeln
            i_rota  = wa_aux-nr_rot
          IMPORTING
            is_ok   = DATA(is_ok).

        IF is_ok IS INITIAL.
          MESSAGE i000(z01) WITH 'Rota sem De/Para de IdPropriedade!'.
          CONTINUE.
        ENDIF.
* "// wbarbosa 18/07/2025 US-168932

        IF ( wa_aux-qte_sol > wa_aux-qte_sol2 ).
          MESSAGE i000(z01) WITH 'Quantidade superior ao Saldo!'.
        ELSE.

          IF wa_aux-dt_entrega < sy-datum AND wa_saida-spart EQ 04.
            MESSAGE i000(z01) WITH 'Data de Entraga não pode ser no Passado!'.
          ELSE.

            " Pego a ultima sequencia de liberação
            LOOP AT t_0082_lib INTO wa_0082_lib
              WHERE nro_sol EQ wa_aux-nro_sol AND
                      vkbur EQ wa_aux-vkbur   AND
                      vbeln EQ wa_aux-vbeln   AND
                      posnr EQ wa_aux-posnr.

              v_seq     = wa_0082_lib-seq.
              v_seq_lib = wa_0082_lib-seq_lib.

            ENDLOOP.

            READ TABLE t_0082 INTO wa_0082
             WITH KEY nro_sol = wa_aux-nro_sol
                      vkbur   = wa_aux-vkbur
                      vbeln   = wa_aux-vbeln
                      posnr   = wa_aux-posnr.

            IF v_seq = 0 .
              v_seq = wa_0082-seq. " Pega a sequencia da solicitação.
            ENDIF.

            MOVE: wa_aux-nro_sol       TO wl_0082-nro_sol,
                  wa_aux-vbeln         TO wl_0082-vbeln,
                  wa_aux-posnr         TO wl_0082-posnr,
                  wa_aux-vkorg         TO wl_0082-vkorg,
                  wa_0082-spart        TO wl_0082-spart,
                  wa_0082-vkgrp        TO wl_0082-vkgrp,
                  wa_0082-vkbur        TO wl_0082-vkbur,
                  wa_0082-auart        TO wl_0082-auart,

                  sy-datum             TO wl_0082-dt_liber,
                  wa_aux-qte_sol       TO wl_0082-qte_lib,
                  sy-uname             TO wl_0082-usuario_lib,
                  2                    TO wl_0082-status,
                  wa_aux-nr_rot        TO wl_0082-nr_rot,
                  wa_aux-werks        TO wl_0082-werks,
                  wa_aux-dt_entrega    TO wl_0082-dt_entrega.

            wl_0082-seq      = v_seq + 1.
            wl_0082-seq_lib  = v_seq_lib + 1.

            APPEND wl_0082 TO tl_0082.
            CLEAR: wl_0082.
          ENDIF.
        ENDIF.

      ENDLOOP.

      IF tl_0082[] IS NOT INITIAL.

        MODIFY zsdt0082 FROM  TABLE tl_0082.
        COMMIT WORK.

**        CS2018001228
        LOOP AT tl_0082 INTO wb_0082.

          SELECT * FROM vbep
              INTO TABLE tl_vbep
            WHERE vbeln EQ wb_0082-vbeln
              AND posnr EQ wb_0082-posnr
              AND lifsp NE '12'.

          REFRESH: tl_return,i_order_item_in,i_order_item_inx,i_sched,i_schedx.
          CLEAR: f_headinx, valida.
          f_headinx-updateflag = 'U'.

          LOOP AT tl_vbep.
            i_sched-itm_number = wb_0082-posnr.
            i_sched-sched_line = tl_vbep-etenr.
            IF tl_vbep-lifsp = '10'.
              i_sched-req_dlv_bl  = ''.
              valida = 'X'.
            ENDIF.
            i_schedx-req_dlv_bl  = 'X'.
            i_schedx-itm_number = wb_0082-posnr.
            i_schedx-sched_line = tl_vbep-etenr.
            i_schedx-updateflag  = 'U'.
            APPEND: i_sched, i_schedx.
          ENDLOOP.


          IF valida EQ 'X'.
            CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
              EXPORTING
                salesdocument    = wb_0082-vbeln
                order_header_inx = f_headinx
              TABLES
                return           = tl_return
                order_item_in    = i_order_item_in
                order_item_inx   = i_order_item_inx
                schedule_lines   = i_sched
                schedule_linesx  = i_schedx.

            READ TABLE tl_return WITH KEY type = 'E'.
            IF sy-subrc NE 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDIF.

*-CS2020001303 - 11.10.2021 - JT - inicio
          PERFORM f_envia_carguero USING wb_0082-vbeln
                                         wb_0082-posnr
                                         abap_false.
*-CS2020001303 - 11.10.2021 - JT - fim

        ENDLOOP.
**        CS2018001228

        " Chama a tela de email
        REFRESH tg_editor.
*        call screen 103 starting at 10 4 ending at 75 18. "FF #168675 - DEL

        PERFORM selecionar_dados.
        PERFORM organizar_dados.

        LEAVE TO SCREEN 0.

      ENDIF.

    WHEN '&HIS_SOL'.

      REFRESH: t_hist.

      SORT: t_0082 BY nro_sol ASCENDING seq ASCENDING.

      REFRESH :events,
               estrutura.

      IF ucomm = '&ENV_EMAIL'.

        REFRESH tg_editor.
        CALL SCREEN 103 STARTING AT 10 4 ENDING AT 75 18.

      ELSE.

        LOOP AT t_aux INTO wa_aux
          WHERE mark IS NOT INITIAL.
          LOOP AT t_0082_lib INTO wa_0082_lib
            WHERE nro_sol EQ wa_aux-nro_sol
              AND vbeln   EQ wa_aux-vbeln
              AND posnr   EQ wa_aux-posnr.

*          IF WA_AUX-MARK IS NOT INITIAL.
            MOVE-CORRESPONDING wa_0082_lib TO wl_hist.

            MOVE: wa_0082_lib-nro_sol     TO wl_hist-nro_sol,
                  wa_0082_lib-seq         TO wl_hist-seq,
                  wa_0082_lib-vbeln       TO wl_hist-vbeln,
                  wa_0082_lib-posnr       TO wl_hist-posnr,
                  wa_0082_lib-seq_lib     TO wl_hist-seq_lib,
                  wa_0082_lib-dt_sol      TO wl_hist-dt_sol,
                  wa_0082_lib-qte_sol     TO wl_hist-qte_sol,
                  wa_0082_lib-usuario_sol TO wl_hist-usuario_sol,
                  wa_0082_lib-dt_liber    TO wl_hist-dt_liber,
                  wa_0082_lib-qte_lib     TO wl_hist-qte_lib,
                  wa_0082_lib-usuario_lib TO wl_hist-usuario_lib,
                  wa_0082_lib-status      TO wl_hist-status,
                  wa_0082_lib-dt_canc     TO wl_hist-dt_canc,
                  wa_0082_lib-user_canc   TO wl_hist-user_canc,
                  wa_0082_lib-auart       TO wl_hist-auart.

            CASE wa_0082_lib-status.
              WHEN 1.
                MOVE TEXT-005 TO  wl_hist-desc_status.
              WHEN 2.
                MOVE TEXT-006 TO  wl_hist-desc_status.
              WHEN 3.
                MOVE TEXT-007 TO  wl_hist-desc_status.
              WHEN 4.
                MOVE TEXT-008 TO  wl_hist-desc_status.
              WHEN 5.
                MOVE TEXT-009 TO  wl_hist-desc_status.
            ENDCASE.

            APPEND wl_hist TO t_hist.
            CLEAR: wl_hist.
*          ENDIF.
          ENDLOOP.

        ENDLOOP.

        wl_layout-colwidth_optimize = 'X'.

        wg_ucomm = '&HIST_SOL'.

        PERFORM montar_layout.
        wl_layout-box_fieldname     = 'MARK'.
        wl_layout-box_tabname       = 'T_HIST'.
        wl_layout-colwidth_optimize = 'X'.
        CLEAR: wg_ucomm.

        PERFORM f_carregar_eventos USING:
                                   slis_ev_user_command  'XUSER_COMMAND_POPUP', "para tira duplo click
                                   slis_ev_pf_status_set 'XPF_STATUS_SET3'.

        DATA _variant TYPE disvariant.
        _variant-report = 'X'.



        CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
          EXPORTING
            i_callback_program      = v_report
            is_variant              = _variant
            i_callback_user_command = 'USER_COMMAND' "sem 2º click
            it_fieldcat             = estrutura[]
            is_layout               = wl_layout
            i_save                  = 'A'
            it_events               = events
            is_print                = t_print
            i_screen_start_column   = '10'
            i_screen_start_line     = '7'
            i_screen_end_column     = '160'
            i_screen_end_line       = '17'
            i_grid_title            = 'Histórico das Solicitações'
          TABLES
            t_outtab                = t_hist.

      ENDIF.

    WHEN '&ENV_EMAIL'.
      REFRESH tg_editor.
      CALL SCREEN 103 STARTING AT 10 4 ENDING AT 75 18.

    WHEN 'CANCELAR'.

      READ TABLE t_hist INTO wl_hist WITH KEY mark = 'X'.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE TEXT-013 TYPE 'E'.
        EXIT.
      ENDIF.

      LOOP AT t_hist INTO wl_hist WHERE mark IS NOT INITIAL.

        DATA(l_tabix) = sy-tabix.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
        DATA(_msg_erro_canc) = zcl_distribuicao_insumos=>cancelar_distribuicao(
          EXPORTING
            i_nro_sol   = wl_hist-nro_sol
            i_seq       = wl_hist-seq
            i_vbeln     = wl_hist-vbeln
            i_posnr     = wl_hist-posnr ).

        IF _msg_erro_canc IS NOT INITIAL.
          MESSAGE _msg_erro_canc TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

*        IF wl_hist-seq EQ 1.
*          MESSAGE TEXT-010 TYPE 'E'.
*        ELSE.
*          CASE wl_hist-status.
*            WHEN 4.
*              MESSAGE TEXT-011 TYPE 'E'.
*            WHEN 5.
*              MESSAGE TEXT-012 TYPE 'E'.
*            WHEN 2.
*
*              TRY .
*                  CALL METHOD lo_order_itens->set_metodo_http
*                    EXPORTING
*                      i_metodo = 'GET'.
*
*                  ls_order_itens-externalid = wl_hist-nro_sol && wl_hist-seq.
*
*                  CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
*                    EXPORTING
*                      i_info_request  = ls_order_itens
*                    IMPORTING
*                      e_id_integracao = DATA(lv_id_integracao)
*                      e_integracao    = DATA(lwa_integracao).
*
*                  /ui2/cl_json=>deserialize( EXPORTING json        = lwa_integracao-ds_data_retorno
*                                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
*                                             CHANGING  data        = ls_retorno_consulta_item ).
*
*                  IF ls_retorno_consulta_item IS NOT INITIAL.
*                    wl_hist-integra_safra = abap_true.              "*-CS2025000249-04.07.2025-#181842-JT
*                    wl_hist-status_back   = wl_hist-status.
*                    MODIFY t_hist FROM wl_hist INDEX l_tabix.
*                  ENDIF.
*                CATCH zcx_integracao INTO DATA(lo_integracao).
*                CATCH zcx_error INTO DATA(lo_error).
*              ENDTRY.
*
*              UPDATE zsdt0082 SET status    = 4
*                                  user_canc = sy-uname
*                                  dt_canc   = sy-datum
*                              WHERE nro_sol EQ wl_hist-nro_sol
*                                AND vbeln   EQ wl_hist-vbeln
*                                AND posnr   EQ wl_hist-posnr
*                                AND seq     EQ wl_hist-seq.
*              COMMIT WORK AND WAIT .
*
*              "*-CS2025000249-04.07.2025-#181842-JT-inicio
*              IF wl_hist-integra_safra = abap_true.
*                lc_dados-nro_sol = wl_hist-nro_sol.
*                lc_dados-seq     = wl_hist-seq.
*                lc_dados-vbeln   = wl_hist-vbeln.
*                lc_dados-posnr   = wl_hist-posnr.
*                lc_insumos->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = l_msg_error ).
*
*                IF l_msg_error IS NOT INITIAL.
*                  MESSAGE s024(sd) WITH l_msg_error DISPLAY LIKE 'E'.
*
*                  UPDATE zsdt0082 SET status    = wl_hist-status_back
*                                      user_canc = sy-uname
*                                      dt_canc   = sy-datum
*                                  WHERE nro_sol EQ wl_hist-nro_sol
*                                    AND vbeln   EQ wl_hist-vbeln
*                                    AND posnr   EQ wl_hist-posnr
*                                    AND seq     EQ wl_hist-seq.
*                  COMMIT WORK AND WAIT .
*
*                  COMMIT WORK AND WAIT.
*                  RETURN.
*                ENDIF.
*              ENDIF.
*              "*-CS2025000249-04.07.2025-#181842-JT-fim
*
*          ENDCASE.
*        ENDIF.

        "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----

*-CS2020001303 - 11.10.2021 - JT - inicio
        PERFORM f_envia_carguero USING wl_hist-vbeln
                                       wl_hist-posnr
                                       abap_false.
*-CS2020001303 - 11.10.2021 - JT - fim

      ENDLOOP.

      SELECT * FROM zsdt0082 INTO TABLE t_hist
                      WHERE nro_sol EQ wl_hist-nro_sol
                        AND vbeln   EQ wl_hist-vbeln
                        AND posnr   EQ wl_hist-posnr.

      LOOP AT t_hist ASSIGNING FIELD-SYMBOL(<wl_hist>).
        CASE <wl_hist>-status.
          WHEN 1.
            MOVE TEXT-005 TO  <wl_hist>-desc_status. ADD <wl_hist>-qte_sol TO qte_sol1.
          WHEN 2.
            MOVE TEXT-006 TO  <wl_hist>-desc_status. ADD <wl_hist>-qte_lib TO qte_lib1.
          WHEN 3.
            MOVE TEXT-007 TO  <wl_hist>-desc_status.
          WHEN 4.
            MOVE TEXT-008 TO  <wl_hist>-desc_status.
          WHEN 5.
            MOVE TEXT-009 TO  <wl_hist>-desc_status. ADD <wl_hist>-qte_lib TO qte_lib1.
        ENDCASE.

      ENDLOOP.

      SUBTRACT qte_lib1  FROM  qte_sol1.
      READ TABLE t_aux ASSIGNING FIELD-SYMBOL(<wa_aux>) WITH KEY vbeln = <wl_hist>-vbeln
                                                                 posnr = <wl_hist>-posnr
                                                               nro_sol = <wl_hist>-nro_sol.
      IF sy-subrc IS INITIAL.
        MOVE qte_sol1 TO <wa_aux>-qte_sol.
      ENDIF.

  ENDCASE.

  selfield-refresh = 'X'.
  selfield-row_stable = 'X'.
  selfield-col_stable = 'X'.

ENDFORM. "XUSER_COMMAND_POPUP

*&---------------------------------------------------------------------*
*&  envia carguero
*&---------------------------------------------------------------------*
FORM  f_envia_carguero USING p_vbeln
                            p_posnr
                            p_alt_qtde_distrib TYPE c.

  DATA: t_vbap_sel   TYPE TABLE OF vbap,
        l_nome_tvarv TYPE tvarvc-name,
        l_tot_brgew  TYPE vbap-brgew,
        l_lifnr      TYPE vbpa-lifnr,
        l_erro       TYPE char1,
        l_erro_mesg  TYPE bapi_msg.

  RANGES: r_vbpa        FOR vbpa-lifnr,
          r_matkl       FOR vbap-matkl.

  FREE: r_vbpa, r_matkl.

*--------------------
*-item OV
*--------------------
  SELECT a~*
    FROM vbap AS a
    INNER JOIN vbep AS b
    ON b~vbeln EQ a~vbeln
    AND b~posnr EQ a~posnr "Ajuste USER STORY 131069 / AOENNING.
    INTO TABLE @t_vbap_sel
   WHERE a~vbeln = @p_vbeln
     AND a~posnr = @p_posnr
     AND b~lifsp <> '12'. "Ajuste USER STORY 131069 / AOENNING.

  CHECK sy-subrc = 0.

  READ TABLE t_vbap_sel INTO DATA(w_vbap) INDEX 1.

*--------------------
*-parceiro
*--------------------
  SELECT a~*
    FROM vbpa AS a
*    inner join vbep as b on b~vbeln eq a~vbeln "Ajuste USER STORY 131069 / AOENNING.
    INTO @DATA(w_vbpa)
      UP TO 1 ROWS
   WHERE a~vbeln = @p_vbeln
     AND a~parvw = 'PC'.
*    and b~lifsp ne '12'. "Ajuste USER STORY 131069 / AOENNING.
  ENDSELECT.

  CHECK sy-subrc = 0.

*--------------------
*-TVARV grp.mercadoria
*--------------------
  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_grp_mat)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_grp_mat INTO DATA(w_grp_mat).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_grp_mat-low ) TO r_matkl.
  ENDLOOP.

*--------------------
*-TVARV parceiro
*--------------------
  l_nome_tvarv = 'PARCEIRO_PC_CENTRO_' && w_vbap-werks.

  SELECT *
    FROM tvarvc
    INTO TABLE @DATA(t_parceiro)
   WHERE name = @l_nome_tvarv.

  IF sy-subrc = 0.
    LOOP AT t_parceiro INTO DATA(w_parceiro).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = w_parceiro-low
        IMPORTING
          output = l_lifnr.

      APPEND VALUE #( sign = 'I' option = 'EQ' low = l_lifnr ) TO r_vbpa.
    ENDLOOP.
  ENDIF.

  CHECK w_vbpa-lifnr IN r_vbpa[]  AND r_vbpa[]  IS NOT INITIAL.
  CHECK w_vbap-matkl IN r_matkl[] AND r_matkl[] IS NOT INITIAL.

*--------------------
*-tipo de material
*--------------------
  SELECT mtart
    FROM mara
    INTO @DATA(l_mtart)
      UP TO 1 ROWS
   WHERE matnr = @w_vbap-matnr.
  ENDSELECT.

  CHECK ( l_mtart = 'ZHAW' ) OR ( l_mtart = 'ZFER' )." AND p_alt_qtde_distrib = ABAP_TRUE ).

*--------------------
*-zsdt0082
*--------------------
  SELECT vbeln, posnr, qte_lib, nr_rot
    FROM zsdt0082
    INTO TABLE @DATA(t_0082_tab)
   WHERE   vbeln  = @p_vbeln
     AND   posnr  = @p_posnr
     AND ( status = '2'
      OR   status = '5' ).

*--------------------
* peso total
*--------------------
  FREE l_tot_brgew.

  LOOP AT t_0082_tab INTO DATA(w_0082_tab).
    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = w_vbap-matnr
        i_in_me              = w_vbap-gewei
        i_out_me             = 'KG'
        i_menge              = w_0082_tab-qte_lib
      IMPORTING
        e_menge              = w_0082_tab-qte_lib
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    l_tot_brgew = l_tot_brgew + w_0082_tab-qte_lib.
  ENDLOOP.

*--------------------
* ajusta vbap
*--------------------
  w_vbap-brgew         = l_tot_brgew.
  MODIFY t_vbap_sel FROM w_vbap INDEX 1.

*--------------------
* envia carguero
*--------------------
  FREE: l_erro, l_erro_mesg.

  TRY .
      zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_envia_fertilizantes_ov(
        EXPORTING
          i_vbeln     = p_vbeln
          t_vbap      = t_vbap_sel
        IMPORTING
          e_erro      = l_erro
          e_erro_mesg = l_erro_mesg ).

    CATCH zcx_integracao.
    CATCH zcx_error.
  ENDTRY.

  COMMIT WORK AND WAIT.

  IF l_erro = abap_true.
    MESSAGE i024(sd) WITH 'Envio Carguero: ' l_erro_mesg(50)
                                             l_erro_mesg+50(50)
                                             l_erro_mesg+100(100).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.

  "BREAK-POINT.
  SET PF-STATUS 'Z001'.
*  SET TITLEBAR 'xxx'.
  IF obg_textbox IS NOT INITIAL.
    CALL METHOD obg_textbox->free.
  ENDIF.
  IF g_custom_container IS NOT INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  REFRESH: tg_editor.
  FREE:  g_custom_container, obg_textbox.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    IF g_custom_container IS NOT INITIAL.
      CREATE OBJECT obg_textbox
        EXPORTING
          parent            = g_custom_container
          wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = 72
          max_number_chars  = 255.

    ENDIF.
  ENDIF.

  IF obg_textbox IS NOT INITIAL.
    CALL METHOD obg_textbox->set_text_as_r3table
      EXPORTING
        table = tg_editor.
  ENDIF.
ENDMODULE.                 " STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0103 INPUT.
  "BREAK-POINT.

  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'OK'.
      " Pego o valor digitado na tela
      REFRESH: tg_obj_cont.
      CALL METHOD obg_textbox->get_text_as_r3table
        IMPORTING
          table = tg_editor.

      LOOP AT tg_editor INTO wg_editor.

        MOVE wg_editor-line TO wg_obj_cont-line.

        TRANSLATE wg_obj_cont-line TO UPPER CASE.
        APPEND wg_obj_cont TO tg_obj_cont.

      ENDLOOP.


      PERFORM gera_pdf.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0103  INPUT
*&---------------------------------------------------------------------*
*&      Form  FORME
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email_html TABLES lt_obj_cont STRUCTURE solisti1
                      USING p_mail
                            p_assunto TYPE string.
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
  MOVE: 'BIN' TO ls_type,
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
    i_type    = 'BIN'
    i_subject = lv_sub
    i_text    = lt_obj_cont[] ). "lt_txt_cont

  lo_bcs = cl_bcs=>create_persistent( ).
  lo_bcs->set_document( lo_document ).

  lo_recipient = cl_cam_address_bcs=>create_internet_address( wl_email ).

  lo_bcs->set_message_subject( ip_subject = p_assunto ).   "Subject

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
*&      Module  STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0104 OUTPUT.
  SET PF-STATUS 'Z002'.
*  SET TITLEBAR 'xxx'.

  "BREAK-POINT.

  IF obg_textbox IS NOT INITIAL.
    CALL METHOD obg_textbox->free.
  ENDIF.
  IF g_custom_container IS NOT INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.
  REFRESH: tg_editor.
  FREE:  g_custom_container, obg_textbox.
  CALL METHOD cl_gui_cfw=>flush.

*  LOOP AT TG_TEXTO.
*      MOVE: TG_TEXTO-TDLINE TO WL_TEXTO.
*      APPEND WL_TEXTO TO TL_TEXTO.
*      CLEAR: WL_TEXTO.
*  ENDLOOP.
**  LOOP AT TG_TEXTO.
**    MOVE TG_TEXTO-TDLINE TO WG_EDITOR-LINE.
**    APPEND WG_EDITOR TO TG_EDITOR.
**  ENDLOOP.
**  IF G_CUSTOM_CONTAINER IS INITIAL.
*
*
*    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
*      EXPORTING
*        IM_TITLE        = 'TESTE'
*        IM_DISPLAY_MODE = ABAP_TRUE
*      CHANGING
*        CH_TEXT         = TL_TEXTO.

*    CREATE OBJECT G_CUSTOM_CONTAINER
*      EXPORTING
*        CONTAINER_NAME = 'CC_TEXTO'.
*
*    IF G_CUSTOM_CONTAINER IS NOT INITIAL.
*      CREATE OBJECT OBG_TEXTBOX
*        EXPORTING
*          PARENT            = G_CUSTOM_CONTAINER
*          WORDWRAP_MODE     = CL_GUI_TEXTEDIT=>WORDWRAP_AT_FIXED_POSITION
*          WORDWRAP_POSITION = 72
*          MAX_NUMBER_CHARS  = 255.
*
*    ENDIF.
*  ENDIF.

  IF obg_textbox IS NOT INITIAL.
    CALL METHOD obg_textbox->set_text_as_r3table
      EXPORTING
        table = tg_editor.

    CALL METHOD obg_textbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ENDIF.

  REFRESH:tg_texto.
ENDMODULE.                 " STATUS_0104  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0104  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0104 INPUT.
  DATA: wl_tabix TYPE sy-tabix,
        wl_name  TYPE thead-tdname.
  DESCRIBE TABLE t_textos_aux LINES wl_tabix.
  REFRESH: tg_texto.
  CLEAR: tg_texto.
  CASE sy-ucomm.
    WHEN 'OK' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'NEXT_T'.
      ADD 1 TO wg_index.
      IF wg_index GT wl_tabix.
        MOVE 1 TO wg_index.
      ENDIF.
      READ TABLE t_textos_aux INTO wa_textos_aux INDEX wg_index.
      CLEAR: wl_name.
      CONCATENATE wa_textos_aux-nro_sol wa_textos_aux-seq INTO wl_name.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = wa_textos_aux-id
          language                = sy-langu
          name                    = wl_name
          object                  = 'ZTEXTO'
        TABLES
          lines                   = tg_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
    WHEN 'BACK_T'.
      SUBTRACT 1 FROM wg_index.
      IF wg_index IS INITIAL.
        wg_index = wl_tabix.
      ENDIF.
      READ TABLE t_textos_aux INTO wa_textos_aux INDEX wg_index.

      CLEAR: wl_name.
      CONCATENATE wa_textos_aux-nro_sol wa_textos_aux-seq INTO wl_name.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = wa_textos_aux-id
          language                = sy-langu
          name                    = wl_name
          object                  = 'ZTEXTO'
        TABLES
          lines                   = tg_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0104  INPUT,

*&---------------------------------------------------------------------*

*&      Form  user_command

*&---------------------------------------------------------------------*

*   Form for user actions on the ALV

*----------------------------------------------------------------------*

FORM user_command USING r_ucomm LIKE sy-ucomm
                    rs_selfield TYPE slis_selfield.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GERA_PDF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_pdf .

  CONSTANTS: c_x               TYPE c VALUE 'X'.

  DATA: vl_form   TYPE tdsfname,
        vl_name   TYPE rs38l_fnam,
        wg_header TYPE zsdt0051,
        vl_safra  TYPE char4,
        vl_date   TYPE char10,
        wl_name   TYPE thead-tdname,
        it_texto  TYPE TABLE OF tline WITH HEADER LINE.

  vl_form = 'ZSDS0005'.

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

  DATA: it_saida TYPE TABLE OF  zinfzsds0005,
        wa_saida TYPE zinfzsds0005.

  LOOP AT t_aux INTO wa_aux WHERE mark IS NOT INITIAL.

    wa_saida-kunnr     =  wa_aux-kunnr.
    wa_saida-name1     =  wa_aux-name1.
    wa_saida-vkbur     =  wa_aux-vkbur.
    wa_saida-vbeln     =  wa_aux-vbeln.
    wa_saida-posnr     =  wa_aux-posnr.
    wa_saida-matnr     =  wa_aux-matnr.
    wa_saida-arktx     =  wa_aux-arktx.
    wa_saida-wrkst     =  wa_aux-wrkst.
    wa_saida-meins     =  wa_aux-meins.
    wa_saida-qte_sol   =  wa_aux-qte_sol.
    wa_saida-qte_sol2  =  wa_aux-qte_sol2.
    wa_saida-nro_sol   =  wa_aux-nro_sol.
    wa_saida-vkorg     =  wa_aux-vkorg.

    APPEND wa_saida TO it_saida.

  ENDLOOP.

  CONCATENATE sy-datum+6(2) '/' sy-datum+4(2) '/'  sy-datum(4) INTO vl_date .

  wl_name = |{ wa_saida-nro_sol }001|.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'ROTE'
      language                = sy-langu
      name                    = wl_name
      object                  = 'ZTEXTO'
    TABLES
      lines                   = it_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

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
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE,
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
      p_date             = vl_date
    IMPORTING
      job_output_info    = job_output_info
    TABLES
      it_saida           = it_saida     " I_NRO_SOL_OV = WG_HEADER-NRO_SOL_OV
      it_obj_cont        = tg_obj_cont
      it_texto           = it_texto
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.


  IF sy-subrc IS INITIAL.

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
    CLEAR: wa_objhead.

    i_objbin[] = i_record[].
* Create Message Body Title and Description
    i_objtxt = 'Segue anexo!'.
    APPEND i_objtxt.

    DESCRIBE TABLE i_objtxt LINES v_lines_txt.
    READ TABLE i_objtxt INDEX v_lines_txt.
    wa_doc_chng-obj_name = 'smartform'.
    wa_doc_chng-expiry_dat = sy-datum + 10.
    wa_doc_chng-obj_descr = 'Distribuição de Embarque - INSUMOS'.

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
    i_objpack-obj_descr = | Sol. Embarque { wa_aux-nro_sol }|.
    APPEND i_objpack.

    CLEAR i_reclist.
    i_reclist-receiver = wg_dest.
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
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Ocorreu um erro ao enviar o e-mail'.
    ELSE.
      MESSAGE s836(sd) WITH 'E-mail enviado com sucesso'.
    ENDIF.

  ENDIF.

ENDFORM.                    " GERA_PDF
*&---------------------------------------------------------------------*
*&      Form  READ_TEXT_ROTEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SELFIELD_VALUE  text
*----------------------------------------------------------------------*
FORM read_text_roteiro USING p_selfield_value.

  DATA: wl_name_char TYPE char10.
  DATA: wl_name TYPE thead-tdname.

  FREE tg_texto.
  wl_name_char = p_selfield_value.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_name_char
    IMPORTING
      output = wl_name_char.

  wl_name = wl_name_char.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'ZROT'
      language                = sy-langu
      name                    = wl_name
      object                  = 'ZSDROTEIRO'
    TABLES
      lines                   = tg_texto
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

ENDFORM.

MODULE status_0105 OUTPUT.
  SET PF-STATUS 'PF0105'.
  SET TITLEBAR 'T0105'.
ENDMODULE.

MODULE user_command_0105 INPUT.

  DATA: var_answer  TYPE c,
        wl_zsdt0150 TYPE zsdt0150.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF wg_qtde_sol_new IS INITIAL.
        MESSAGE |Alteração de Quantidade NÃO permitida! Deve ser realizado o Estorno pela ZSDT0079! | TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Deseja realmente alterar a quantidade dessa solicitação?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          default_button        = '1'
          display_cancel_button = ''
        IMPORTING
          answer                = var_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK var_answer EQ '1'.

      "Recupera registro da quantidade total da solicitação
      SELECT SINGLE *
        FROM zsdt0082 INTO @DATA(_wl_0082_sol)
       WHERE nro_sol = @wa_saida-nro_sol
         AND vbeln   = @wa_saida-vbeln
         AND posnr   = @wa_saida-posnr
         AND seq     = '001'.

      IF sy-subrc NE 0.
        MESSAGE s024(sd) WITH 'Registro da solicitado não encontrado para atualização' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

*-US191034-17.09.2025-#191034-JT-inicio
*---------------------------------------------
*---- check se solicitacao desta OV esta bloqueada/pendente na distribuicao (zsdt0081)
*---------------------------------------------
      IF _wl_0082_sol-bloqueio = abap_true.
        MESSAGE s024(sd) WITH 'Solicitação está Pendente na Distribuicao!!' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
*-US191034-17.09.2025-#191034-JT-inicio

      SELECT SUM( qte_lib )
        FROM zsdt0082 INTO @DATA(_wl_qtde_lib)
       WHERE nro_sol EQ @wa_saida-nro_sol
         AND vbeln   EQ @wa_saida-vbeln
         AND posnr   EQ @wa_saida-posnr
         AND seq     NE '001'
*         AND STATUS  IN ('2', '3', '5' )
        AND status  NE '4'
         AND qte_sol EQ 0.

      IF ( wg_qtde_sol_new < _wl_qtde_lib ).
        MESSAGE |Quantidade { wg_qtde_sol_new } não pode ser inferior a quantidade já distribuída { _wl_qtde_lib } ! | TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF ( wg_qtde_sol_new >= _wl_0082_sol-qte_sol ).
        MESSAGE |Quantidade { wg_qtde_sol_new } deve ser menor que a quantidade da solicitação { _wl_0082_sol-qte_sol } ! | TYPE 'S'  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      DATA(_qtde_sol_old)  = _wl_0082_sol-qte_sol.
      _wl_0082_sol-qte_sol = wg_qtde_sol_new.
      MODIFY zsdt0082 FROM _wl_0082_sol.
      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao alterar a quantidade da solicitação' TYPE 'S'  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      CLEAR: wl_zsdt0150.

      wl_zsdt0150-direcao        = 'Solicitado'.
      wl_zsdt0150-nro_sol        = wa_saida-nro_sol.
      wl_zsdt0150-seq            = '001'.
      wl_zsdt0150-vbeln          = wa_saida-vbeln.
      wl_zsdt0150-posnr          = wa_saida-posnr.
      wl_zsdt0150-dt_registro    = sy-datum.
      wl_zsdt0150-hr_registro    = sy-uzeit.
      wl_zsdt0150-us_registro    = sy-uname.
      wl_zsdt0150-qtde_old       = _qtde_sol_old.
      wl_zsdt0150-qtde_new       = wg_qtde_sol_new.
      MODIFY zsdt0150 FROM wl_zsdt0150.

      IF sy-subrc NE 0.
        ROLLBACK WORK.
        MESSAGE 'Houve um erro ao alterar a quantidade da solicitação' TYPE 'S'.
        EXIT.
      ENDIF.

      MESSAGE 'Quantidade da solicitação alterada com sucesso!' TYPE 'S'.
      LEAVE TO SCREEN 0.

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.


FORM pf_status_set USING ut_extab TYPE slis_t_extab.        "#EC CALLED

  DATA: tg_fcode TYPE TABLE OF sy-ucomm WITH HEADER LINE.

  REFRESH: tg_fcode.

  SET PF-STATUS '0100' EXCLUDING tg_fcode.

  "SET PF-STATUS 'STANDARD_FULLSCREEN' OF PROGRAM 'SAPLKKBL'.


ENDFORM.

FORM f_montar_layout_log_qtde.
  REFRESH estrutura.
  PERFORM montar_estrutura USING:
     1  ''   ''            'IT_ZSDT0150' 'DIRECAO'      'Ação'         '10' '',
     1  ''   ''            'IT_ZSDT0150' 'SEQ'          'Sequencia'    '03' '',
     1  ''   ''            'IT_ZSDT0150' 'DT_REGISTRO'  'Data'         '10' '',
     1  ''   ''            'IT_ZSDT0150' 'HR_REGISTRO'  'Hora'         '10' '',
     1  ''   ''            'IT_ZSDT0150' 'US_REGISTRO'  'Usuário Reg.' '13' '',
     1  ''   ''            'IT_ZSDT0150' 'QTDE_OLD'     'Qtde.Antiga'  '13' '',
     1  ''   ''            'IT_ZSDT0150' 'QTDE_NEW'     'Qtde.Nova'    '13' ''.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  GERA_EXCEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_excel .

  DATA: str         TYPE REF TO data,
        concat      TYPE c LENGTH 99999,
        r_fieldname TYPE RANGE OF char30.

  DATA: w_excel     TYPE ole2_object,
        w_workbooks TYPE ole2_object,
        w_workbook  TYPE ole2_object,
        h_int       TYPE ole2_object,
        h_f         TYPE ole2_object,
        h_rows      TYPE ole2_object,
        h_font      TYPE ole2_object,
        h_columns   TYPE ole2_object,
        h_entirecol TYPE ole2_object,
        w_cell      TYPE ole2_object.

  DATA: w_line TYPE i.


  r_fieldname = VALUE #( sign = 'I' option = 'EQ' ( low = 'CPF_CNPJ' )
                                                  ( low = 'REGIO' )
                                                  ( low = 'ORT01' )
                                                  ( low = 'STCD3' ) ).

  CREATE OBJECT w_excel 'Excel.Application'.

  CALL METHOD OF w_excel 'Workbooks' = w_workbooks.
  CALL METHOD OF w_workbooks 'Add' = w_workbook.

  SET PROPERTY OF w_excel 'Visible' = 1.

  LOOP AT fcat ASSIGNING FIELD-SYMBOL(<table>).

    CASE <table>-fieldname.
      WHEN 'VKBUR'.
        <table>-seltext_l = 'Escritório de vendas'.
      WHEN 'MATNR'.
        <table>-seltext_l = 'Material'.
      WHEN 'INCO1'.
        <table>-seltext_l = 'Incoterms'.
    ENDCASE.

    DATA(tabix) = sy-tabix.
    CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = 1 #2 = tabix.
    SET PROPERTY OF w_cell 'Value' = <table>-seltext_l.
    GET PROPERTY OF w_cell 'Interior'   = h_int.
    GET PROPERTY OF w_cell 'Font'    = h_f.
    SET PROPERTY OF w_cell 'HorizontalAlignment' = -4108 .

  ENDLOOP.

  CALL METHOD OF w_excel 'Rows' = h_rows
    EXPORTING
      #1 = '1:1'.
  GET PROPERTY OF h_rows 'Font' = h_font.
  SET PROPERTY OF h_font 'Bold' = 1.

  FIELD-SYMBOLS: <fs_campo> TYPE any.

  w_line = 2.
  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<excel>).
    LOOP AT fcat ASSIGNING <table>.

      tabix = sy-tabix.
      ASSIGN COMPONENT <table>-fieldname OF STRUCTURE <excel> TO <fs_campo>.

      CASE <table>-fieldname.

        WHEN 'NR_ROT'.

          wl_name = <fs_campo>.

          IF wl_name EQ '9999999999'.

            wl_name = |{ <excel>-nro_sol }001|.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ROTE'
                language                = sy-langu
                name                    = wl_name
                object                  = 'ZTEXTO'
              TABLES
                lines                   = tg_texto
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

          ELSE.

            CALL FUNCTION 'READ_TEXT'
              EXPORTING
                id                      = 'ZROT'
                language                = sy-langu
                name                    = wl_name
                object                  = 'ZSDROTEIRO'
              TABLES
                lines                   = tg_texto
              EXCEPTIONS
                id                      = 1
                language                = 2
                name                    = 3
                not_found               = 4
                object                  = 5
                reference_check         = 6
                wrong_access_to_archive = 7
                OTHERS                  = 8.

          ENDIF.

          CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
          SET PROPERTY OF w_cell 'Value' = <fs_campo>.

        WHEN 'DESROTEIRO'.

          IF NOT tg_texto[] IS INITIAL.

            LOOP AT tg_texto.
              concat = |{ concat } { tg_texto-tdline } |.
            ENDLOOP.

            CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
            SET PROPERTY OF w_cell 'Formula' = concat.
            SET PROPERTY OF w_cell 'Value' = concat.

            FREE tg_texto.
            CLEAR concat.

          ENDIF.

        WHEN OTHERS.

          CALL METHOD OF w_excel 'Cells' = w_cell EXPORTING #1 = w_line #2 = tabix.
          IF <table>-fieldname IN r_fieldname.
            SET PROPERTY OF w_cell 'NumberFormat' = '@'.
          ENDIF.
          SET PROPERTY OF w_cell 'Value' = <fs_campo>.

          CASE <table>-fieldname.
            WHEN 'KWMENG' OR 'QTE_FAT' OR 'QTE_SOL' OR 'QTE_SDO_LIB' OR 'QTE_SDO_FAT'.
              SET PROPERTY OF w_cell 'NumberFormat' = '#,###'.
          ENDCASE.

      ENDCASE.
    ENDLOOP.
    ADD 1 TO w_line.
  ENDLOOP.

  CALL METHOD OF w_excel 'Columns' = h_columns
    EXPORTING
      #1 = 'A:Q'.
  GET PROPERTY OF h_columns 'EntireColumn' = h_entirecol.
  SET PROPERTY OF h_entirecol 'Autofit' = 1.

  FREE OBJECT: w_excel,
               w_workbooks,
               w_workbook,
               w_cell.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALTERA_QTD_PLN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM altera_qtd_pln.

*  BREAK-POINT.

  SELECT *
    FROM zsdt0082
    INTO CORRESPONDING FIELDS OF TABLE it_alt_qtd_pln
    WHERE nro_sol EQ _saida-nro_sol
       AND status IN ('2', '5').

  IF sy-subrc IS INITIAL.

    LOOP AT it_alt_qtd_pln ASSIGNING FIELD-SYMBOL(<f_pln>).

      CASE <f_pln>-spart.
        WHEN 2.

          SELECT *
            FROM zsdt0137
            INTO TABLE @DATA(it_0137)
              WHERE nro_sol EQ @<f_pln>-nro_sol
              AND seq EQ @<f_pln>-seq
              AND status NE @abap_true.

          <f_pln>-qtd_vinc = REDUCE #( INIT x = 0 FOR ls1 IN it_0137 NEXT x = x + ls1-qtd_vinc ).

        WHEN 3.

          SELECT *
            FROM zsdt0140
            INTO TABLE @DATA(it_0140)
              WHERE nro_sol EQ @<f_pln>-nro_sol
              AND seq EQ @<f_pln>-seq
              AND status NE @abap_true.

          IF sy-subrc IS INITIAL.

            SELECT *
              FROM zsdt0134
              INTO TABLE @DATA(it_0134)
              FOR ALL ENTRIES IN @it_0140
                WHERE vbeln EQ @it_0140-vbeln
                AND posnr EQ @it_0140-posnr
                AND nro_cg EQ @it_0140-nro_cgd
              AND status NE @abap_true.

            <f_pln>-qtd_vinc = REDUCE #( INIT x = 0 FOR ls2 IN it_0134 NEXT x = x + ls2-lfimg ).

          ENDIF.

        WHEN 4.

          SELECT *
            FROM zsdt0130
            INTO TABLE @DATA(it_0130)
              WHERE nro_sol EQ @<f_pln>-nro_sol
              AND seq EQ @<f_pln>-seq
              AND status NE @abap_true.

          IF sy-subrc IS INITIAL.

            SELECT *
              FROM zsdt0131
              INTO TABLE @DATA(it_0131)
              FOR ALL ENTRIES IN @it_0130
                WHERE nro_lote EQ @it_0130-nro_lote
                AND nro_sol EQ @it_0130-nro_sol
                AND seq EQ @it_0130-seq.

            <f_pln>-qtd_vinc = REDUCE #( INIT x = 0 FOR ls IN it_0131 NEXT x = x + ls-qtd_vinc ).

          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDIF.

  CALL SCREEN 0106 STARTING AT 030 6
                   ENDING   AT 130 15.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0106 OUTPUT.

  SET PF-STATUS 'PF0106'.
  SET TITLEBAR 'TI0106'.

  fcat_alt_qtd_pln = VALUE #(
                              ( col_pos = 1 fieldname = 'NRO_SOL'  scrtext_l = 'Nro Solicitação' )
                              ( col_pos = 2 fieldname = 'SEQ'      scrtext_l = 'Sequência'       )
                              ( col_pos = 3 fieldname = 'VBELN'    scrtext_l = 'Vbeln'           )
                              ( col_pos = 4 fieldname = 'POSNR'    scrtext_l = 'Posnr'           )
                              ( col_pos = 5 fieldname = 'SEQ_LIB'  scrtext_l = 'Seq Liberação'   )
                              ( col_pos = 6 fieldname = 'QTE_LIB'  scrtext_l = 'Qtd. Distribuida'  )
                              ( col_pos = 7 fieldname = 'QTD_VINC' scrtext_l = 'Qtd. Vinculada'   )
                              ( col_pos = 8 fieldname = 'QTD_NEW'  scrtext_l = 'Qtd. Nova'
                                ref_table = 'VBAP'  ref_field = 'KWMENG' edit = abap_true no_zero = abap_true ) "*-CS2025000249-04.07.2025-#181842-JT
                            ).

  IF cont_alt_qtd_pln IS INITIAL.

    CREATE OBJECT cont_alt_qtd_pln
      EXPORTING
        container_name = 'CC_ALT_QTD_PLN'.

    CREATE OBJECT alv_alt_qtd_pln
      EXPORTING
        i_shellstyle    = 0
        i_parent        = cont_alt_qtd_pln
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    CREATE OBJECT o_event
      EXPORTING
        io_alv_grid = alv_alt_qtd_pln.

    SET HANDLER: o_event->on_dt_cnd   FOR alv_alt_qtd_pln,
                 o_event->on_dt_cnd_f FOR alv_alt_qtd_pln.

    _function = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).

    _save = abap_true.
    _variant-report = sy-repid.

    _layout = VALUE #(
                       zebra      = abap_true
                       no_toolbar = abap_false
                       cwidth_opt = abap_true
                       sel_mode   = 'C'
                     ).

    CALL METHOD alv_alt_qtd_pln->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding          = _function
*       IS_VARIANT                    = _VARIANT
        is_layout                     = _layout
        i_save                        = _save
        i_default                     = abap_true
      CHANGING
        it_outtab                     = it_alt_qtd_pln
        it_fieldcatalog               = fcat_alt_qtd_pln
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    CALL METHOD alv_alt_qtd_pln->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD alv_alt_qtd_pln->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD alv_alt_qtd_pln->refresh_table_display
      EXPORTING
        is_stable = w_stable.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0106  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0106 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'OK'.
      PERFORM atualiza_qtd_vinc.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_QTD_VINC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_qtd_vinc .

*-CS2025000249-04.07.2025-#181842-JT-inicio
  DATA: lo_order_itens           TYPE REF TO zcl_int_ob_safra_crt_ord_itens,
        lc_insumos               TYPE REF TO zcl_distribuicao_insumos,
        ls_order_itens           TYPE zde_safra_control_ordem_itens,
        ls_retorno_consulta_item TYPE zsds392,
        lc_dados                 TYPE zmme_dados_safra,
        l_msg_error              TYPE string,
        lv_qtd_lib               TYPE vbap-kwmeng.
*-CS2025000249-04.07.2025-#181842-JT-fim

  FREE:  lc_insumos, lo_order_itens.         "*-CS2025000249-04.07.2025-#181842-JT
  CREATE OBJECT: lc_insumos, lo_order_itens. "*-CS2025000249-04.07.2025-#181842-JT

  LOOP AT it_alt_qtd_pln INTO DATA(wa).

    DATA(lv_tabix) = sy-tabix.

    CHECK wa-qtd_new <> 0.  "*-CS2025000249-04.07.2025-#181842-JT

    IF NOT wa-qtd_new BETWEEN wa-qtd_vinc AND wa-qte_lib.
      MESSAGE |Quantidade fora do Permitido! Linha { lv_tabix }| TYPE 'S' DISPLAY LIKE 'E'.
      DATA(_check) = abap_true.
      RETURN.
    ENDIF.

    IF  wa-qtd_new IS INITIAL.
      MESSAGE |Quantidade não pode ser ZERO! Linha { lv_tabix }| TYPE 'S' DISPLAY LIKE 'E'.
      _check = abap_true.
      RETURN.
    ENDIF.

*-CS2025000249-04.07.2025-#181842-JT-inicio
    TRY .
        CALL METHOD lo_order_itens->set_metodo_http
          EXPORTING
            i_metodo = 'GET'.

        ls_order_itens-externalid = wa-nro_sol && wa-seq.

        CALL METHOD lo_order_itens->zif_integracao_outbound~execute_request
          EXPORTING
            i_info_request  = ls_order_itens
          IMPORTING
            e_id_integracao = DATA(lv_id_integracao)
            e_integracao    = DATA(lwa_integracao).

        /ui2/cl_json=>deserialize( EXPORTING json        = lwa_integracao-ds_data_retorno
                                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                   CHANGING  data        = ls_retorno_consulta_item ).

        IF ls_retorno_consulta_item IS NOT INITIAL.
*          lv_qtd_lib = ls_retorno_consulta_item-quantity.         "// CS2025000249 13/09/2025 #181842 WBARBOSA Aplicar Campo de Quantiodade em Carga
          lv_qtd_lib = ls_retorno_consulta_item-quantity_in_cargo. "// CS2025000249 13/09/2025 #181842 WBARBOSA Aplicar Campo de Quantiodade em Carga

          IF wa-qtd_new < lv_qtd_lib.
            MESSAGE |Quantidade não pode ser inferior ao Liberado no SAFRA! Linha { lv_tabix }| TYPE 'S' DISPLAY LIKE 'E'.
            _check = abap_true.
            RETURN.
          ENDIF.

          wa-integra_safra         = abap_true.              "*-CS2025000249-04.07.2025-#181842-JT
          wa-qte_lib_back          = wa-qte_lib.             "*-CS2025000249-04.07.2025-#181842-JT
          MODIFY it_alt_qtd_pln FROM wa INDEX lv_tabix.      "*-CS2025000249-04.07.2025-#181842-JT
        ENDIF.

      CATCH zcx_integracao INTO DATA(lo_integracao).
      CATCH zcx_error INTO DATA(lo_error).
    ENDTRY.
*-CS2025000249-04.07.2025-#181842-JT-fim

  ENDLOOP.

  CHECK _check IS INITIAL.

  LOOP AT it_alt_qtd_pln INTO wa WHERE qtd_new <> 0.  "*-CS2025000249-04.07.2025-#181842-JT

    UPDATE zsdt0082 SET qte_lib  = wa-qtd_new
                  WHERE nro_sol EQ wa-nro_sol
                    AND seq     EQ wa-seq.

    COMMIT WORK AND WAIT.

    "*-CS2025000249-04.07.2025-#181842-JT-inicio
    IF wa-integra_safra = abap_true.
      lc_dados-nro_sol = wa-nro_sol.
      lc_dados-seq     = wa-seq.
      lc_dados-vbeln   = wa-vbeln.
      lc_dados-posnr   = wa-posnr.
      lc_insumos->set_integrar_safra_control( EXPORTING i_dados = lc_dados IMPORTING e_msg_erro = l_msg_error ).

      IF l_msg_error IS NOT INITIAL.
        MESSAGE s024(sd) WITH l_msg_error DISPLAY LIKE 'E'.

        UPDATE zsdt0082 SET qte_lib  = wa-qte_lib_back
                      WHERE nro_sol EQ wa-nro_sol
                        AND seq     EQ wa-seq.

        COMMIT WORK AND WAIT.
        RETURN.
      ENDIF.
    ENDIF.
    "*-CS2025000249-04.07.2025-#181842-JT-fim

    wl_zsdt0150 = VALUE #(
                            direcao        = 'Distribuído'
                            nro_sol        = wa-nro_sol
                            seq            = wa-seq
                            vbeln          = wa-vbeln
                            posnr          = wa-posnr
                            dt_registro    = sy-datum
                            hr_registro    = sy-uzeit
                            us_registro    = sy-uname
                            qtde_old       = wa-qte_lib
                            qtde_new       = wa-qtd_new
                         ).

    MODIFY zsdt0150 FROM wl_zsdt0150.

*-CS2020001303 - 11.10.2021 - JT - inicio
    PERFORM f_envia_carguero USING wa-vbeln
                                   wa-posnr
                                   abap_true.
*-CS2020001303 - 11.10.2021 - JT - fim

  ENDLOOP.



  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form authority_check
*&---------------------------------------------------------------------*
FORM authority_check .

  "FF #171337 - inicio
*  IF s_werks[] IS INITIAL AND s_vkbur[] IS INITIAL.
*    MESSAGE i024(sd) WITH 'Informe o escritório de venda ou o centro' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
  "FF #171337 - fim

  LOOP AT s_werks ASSIGNING FIELD-SYMBOL(<fs_cent>).
    <fs_cent>-low  = |{ <fs_cent>-low ALPHA = IN }|.
    <fs_cent>-high = |{ <fs_cent>-high ALPHA = IN }|.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_cent>-low ) TO rg_werks.
  ENDLOOP.

  LOOP AT s_vkbur INTO DATA(ws_vkbur).
*    "// BUG-164703 WBARBOSA 27/01/2025
    IF ws_vkbur-option EQ 'BT'.
      APPEND VALUE #( sign = ws_vkbur-sign option = ws_vkbur-option low = ws_vkbur-low high = ws_vkbur-high ) TO rg_werks.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = |{ ws_vkbur-low ALPHA = IN }| ) TO rg_werks.
    ENDIF.
*    "// BUG-164703 WBARBOSA 27/01/2025
  ENDLOOP.

  SELECT werks, name1, vkorg INTO TABLE @DATA(lt_werks) FROM t001w
    WHERE werks IN @rg_werks
      AND vkorg IN @s_vkorg.

  LOOP AT lt_werks INTO DATA(wa_werks).
    AUTHORITY-CHECK OBJECT 'ZMM00132'
    ID 'WERKS' FIELD wa_werks-werks
    ID 'VKORG' FIELD wa_werks-vkorg.

    IF sy-subrc EQ 0.
      r_werks = VALUE #( BASE r_werks ( sign = 'I' option = 'EQ' low = wa_werks-werks ) ).
    ELSE.
      APPEND VALUE #( werks = wa_werks-werks name1 = wa_werks-name1 vkorg = wa_werks-vkorg ) TO gt_popup.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_werks-werks ) TO r_werks_del. "// WBARBOSA BUG-160708 11/12/2024
    ENDIF.
  ENDLOOP.

  IF gt_popup IS NOT INITIAL.
    CALL SCREEN 0107 STARTING AT 20 5.
  ELSE.
    gv_continue = abap_true.
  ENDIF.

  IF s_werks[] IS NOT INITIAL.
    DELETE s_werks WHERE low NOT IN r_werks.
  ENDIF.

  IF s_vkbur IS NOT INITIAL.
    DELETE s_vkbur WHERE low NOT IN r_werks.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0107  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0107 INPUT.
  CASE sy-ucomm.
    WHEN 'NO' OR 'ECAN'.
      gv_continue = abap_false.
    WHEN 'YES'.
      gv_continue = abap_true.
  ENDCASE.
  SET SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT OUTPUT
*&---------------------------------------------------------------------*
MODULE init OUTPUT.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR 'CENTROS'.
  PERFORM init_container.
  PERFORM init_alv.
  PERFORM build_fcat.
  PERFORM show_popup_data.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INIT_CONTAINER
*&---------------------------------------------------------------------*
FORM init_container .
  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING
*       parent                      =
        container_name              = 'CC_CENTROS'
*       style                       =
*       lifetime                    = lifetime_default
*       repid                       =
*       dynnr                       =
*       no_autodef_progid_dynnr     =
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_ALV
*&---------------------------------------------------------------------*
FORM init_alv .
  IF go_alv_popup IS NOT BOUND.
    CREATE OBJECT go_alv_popup
      EXPORTING
        i_parent          = go_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_POPUP_DATA
*&---------------------------------------------------------------------*
FORM show_popup_data .

  CALL METHOD go_alv_popup->set_table_for_first_display
    CHANGING
      it_outtab                     = gt_popup
      it_fieldcatalog               = gt_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUILD_FCAT
*&---------------------------------------------------------------------*
FORM build_fcat .
  IF gt_fieldcat IS INITIAL.
    gs_fcat-fieldname   = 'WERKS'.
    gs_fcat-ref_table   = 'GT_POPUP'.
    gs_fcat-coltext     = 'Centro'.
    APPEND gs_fcat TO gt_fieldcat.
    CLEAR gs_fcat.
    gs_fcat-col_opt     = abap_true.
    gs_fcat-fieldname   = 'NAME1'.
    gs_fcat-ref_table   = 'GT_POPUP'.
    gs_fcat-coltext     = 'Descrição'.
    APPEND gs_fcat TO gt_fieldcat.
    CLEAR gs_fcat.
    gs_fcat-col_opt     = abap_true.
    gs_fcat-fieldname   = 'VKORG'.
    gs_fcat-ref_table   = 'GT_POPUP'.
    gs_fcat-coltext     = 'Empresa'.
    APPEND gs_fcat TO gt_fieldcat.
    CLEAR gs_fcat.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE sy-ucomm.
    WHEN 'NO' OR 'ECAN'.
      gv_continue = abap_false.
    WHEN 'YES'.
      gv_continue = abap_true.
  ENDCASE.
  SET SCREEN 0.

ENDMODULE.

"FF #145609 - inicio
FORM f_monta_shdb.

  CLEAR: it_bdcdata[].

  PERFORM f_preenche_bdc USING:

          'X'    'ZSDR0042'       '1000',
          ' '    'BDC_CURSOR'     'S_VBELN-LOW',
          ' '    'BDC_OKCODE'     '=ONLI',
          ' '    'S_VKORG-LOW'    _saida-vkorg,
          ' '    'S_VKBUR-LOW'    _saida-vkbur,
          ' '    'S_VBELN-LOW'    _saida-vbeln,
          ' '    'S_MATNR-LOW'    _saida-matnr.

*          ' '    'P_EXTCAL'         'X'. "Usarei o export memory [ID 'memory_qte_sol'] para identificar se a chamada foi feita por call transaction...

*  PERFORM f_preenche_bdc USING:
*          'X'    'ZSDR0042'       '1000',
*          ' '    'BDC_OKCODE'     '=DESMEMBRAR'.
*
*  PERFORM f_preenche_bdc USING:
*          'X'    'ZSDR0042'       '0300',
*          ' '    'BDC_OKCODE'     '=CONF', "Chamei o perform CONF direto no PBO da tela 300 do programa ZSDR0042 (transação ZSDT0087)
*
*          ' '    'WG_DESMEM-KUNNR' _saida-kunnr.




ENDFORM.

FORM f_call_zsdt0087.

  DATA lo_insumos TYPE REF TO zcl_simulador_insumos.
  DATA lv_vbeln TYPE vbeln_va.

  DATA ls_layout TYPE lvc_s_layo.
  DATA is_table        TYPE lvc_s_stbl.
  DATA lo_grid TYPE REF TO  cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CHECK lo_grid IS NOT INITIAL.

  CALL METHOD lo_grid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_rows).

  IF lines( lt_rows ) > 2.
    MESSAGE  'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF lt_rows IS NOT INITIAL.

    lv_vbeln = t_saida[ lt_rows[ 1 ]-index ]-vbeln.

  ENDIF.

  lo_insumos = NEW zcl_simulador_insumos( iv_ov = lv_vbeln ).

  CALL METHOD lo_insumos->call_transaction_zsdt0087
    EXCEPTIONS
      sem_dados = 1
      OTHERS    = 2.

  IF sy-subrc <> 0.
    MESSAGE  'Sem dados para a seleção' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_preenche_bdc USING dynbegin
                           name
                           value.

  IF dynbegin = 'X'.
    MOVE: name      TO st_bdcdata-program,
          value     TO st_bdcdata-dynpro,
          dynbegin  TO st_bdcdata-dynbegin.
    APPEND st_bdcdata TO it_bdcdata.
  ELSE.

    MOVE: name  TO st_bdcdata-fnam,
          value TO st_bdcdata-fval.
    APPEND st_bdcdata TO it_bdcdata.

  ENDIF.

  CLEAR st_bdcdata.

ENDFORM.

FORM f_submit_zsdr0037 USING p_ov TYPE vbeln p_rota TYPE zde_nro_sol.


  DATA: r_vkorg TYPE RANGE OF vkorg,
        r_vkbur TYPE RANGE OF vkbur,
        r_auart TYPE RANGE OF auart,
        r_erdat TYPE RANGE OF erdat,
        r_matnr TYPE RANGE OF matnr,
        r_ordem TYPE RANGE OF vbeln.

*  DATA: LV_NEW_OV TYPE VBELN.

*  IMPORT I_VBELN TO LV_NEW_OV FROM MEMORY ID 'memory_i_vbeln'. "Export feito no programa ZSDR0042  "FF #145609
  _saida-vbeln = p_ov.


  SELECT SINGLE auart, erdat
  FROM vbak
  INTO @DATA(ls_vbak)
  WHERE vbeln = @_saida-vbeln.
  IF sy-subrc <> 0.
    CLEAR ls_vbak.
  ENDIF.

  " Preenche o intervalo de valores para o SELECT-OPTION
  APPEND VALUE #( sign = 'I' option = 'EQ' low = _saida-vkorg  ) TO r_vkorg.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = _saida-vkbur  ) TO r_vkbur.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_vbak-erdat ) TO r_erdat.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_vbak-auart ) TO r_auart.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = _saida-matnr  ) TO r_matnr.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = _saida-vbeln  ) TO r_ordem.

  SUBMIT zsdr0037 WITH s_vkorg IN r_vkorg
                  WITH s_vkbur IN r_vkbur
                  WITH s_auart IN r_auart
                  WITH s_erdat IN r_erdat
                  WITH s_matnr IN r_matnr
                  WITH s_ordem IN r_ordem
                  WITH p_rota  EQ p_rota
                  WITH p_dt_en EQ _saida-dt_entrega
                  WITH p_qte_1 EQ _saida-qte_sol
                  WITH p_qte_2 EQ _saida-qte_sdo_lib
                  WITH p_tpope EQ _saida-tp_oper
                  WITH p_extcal = 'X' AND RETURN. "Indica que o programa será chamado via submit.

ENDFORM.


"FF #145609 - fim
*&---------------------------------------------------------------------*
*& Form valida_sisdev
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_SAIDA_AUX
*&---------------------------------------------------------------------*
FORM valida_sisdev  USING  p_saida TYPE ty_saida
                    CHANGING c_error TYPE boolean.

  TABLES: kna1.

  "Verificar Região do centro fornecedor.
  SELECT SINGLE regio FROM  t001w
  INTO @DATA(vg_regio) WHERE werks EQ @wa_saida-werks.

  SELECT SINGLE * FROM kna1
    WHERE stcd1 EQ p_saida-cpf_cnpj
       OR stcd2 EQ p_saida-cpf_cnpj.
  IF sy-subrc IS INITIAL AND kna1-regio EQ 'MT' AND vg_regio EQ 'MT'.
    SELECT COUNT(*)
      FROM zsdt0205
      WHERE cpfcnpj = @p_saida-cpf_cnpj.

    IF sy-subrc <> 0.
      IF sy-subrc NE 0.
        SELECT COUNT(*)
        FROM zsdt0206
        WHERE cnpj EQ @p_saida-cpf_cnpj.
        IF sy-subrc NE 0.
          c_error = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form valida_clas_indea
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> WA_SAIDA_AUX
*&      <-- LV_ERROR_INDEA
*&---------------------------------------------------------------------*
FORM valida_clas_indea  USING  p_saida TYPE ty_saida
                    CHANGING c_error TYPE boolean.

  DATA: lv_idea  TYPE zsdt0210-id_matnr_idea,
        lv_matnr TYPE matnr18.

  lv_matnr = p_saida-matnr(18).

  NEW zcl_mm_util( )->get_codigo_indea(
    EXPORTING
      i_material = lv_matnr
    IMPORTING
      id_indea   = lv_idea
  ).

  IF lv_idea IS INITIAL.
    c_error = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0108 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0108 OUTPUT.

  "*-CS2025000249-16.06.2025-#182039-JT-inicio
  DATA: w_fcode TYPE slis_extab,
        t_fcode TYPE slis_t_extab.
  "*-CS2025000249-16.06.2025-#182039-JT-fim

  FREE: t_fcode.  ""*-CS2025000249-16.06.2025-#182039-JT

  READ TABLE t_aux INTO DATA(_aux) INDEX 1.

  "*-CS2025000249-16.06.2025-#182039-JT-inicio
  IF lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = _aux-nro_sol i_seq = _aux-seq i_vbeln = _aux-vbeln
                                            i_posnr   = _aux-posnr )-bloqueio = abap_true.
    APPEND VALUE #( fcode = 'OK'  )          TO t_fcode[].
  ELSE.
    APPEND VALUE #( fcode = 'REPROCESSAR'  ) TO t_fcode[].
  ENDIF.
  APPEND VALUE #(   fcode = 'ITINERARIO'  )  TO t_fcode[].
  "*-CS2025000249-16.06.2025-#182039-JT-fim

*-US191954-30.09.2025-#191954-JT-inicio
  IF      _aux-spart = '04'. "sementes
    APPEND VALUE #( fcode = 'OUTROS_CENTROS'  )  TO t_fcode[].
    APPEND VALUE #( fcode = 'MESMOS_CENTROS'  )  TO t_fcode[].
  ELSEIF  _aux-spart = '03'. "defensivos
    READ TABLE gt_dados_alv1 INTO DATA(_dados1) INDEX 1.
    LOOP AT gt_dados_alv2 INTO DATA(_dados2) WHERE werks = _dados1-werks.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      APPEND VALUE #( fcode = 'MESMOS_CENTROS'  )  TO t_fcode[].
    ELSE.
      APPEND VALUE #( fcode = 'OUTROS_CENTROS'  )  TO t_fcode[].
    ENDIF.
  ENDIF.

  LOOP AT gt_dados_alv2 INTO _dados2 WHERE vfdat < sy-datum.
    EXIT.
  ENDLOOP.
  IF sy-subrc = 0.
    APPEND VALUE #( fcode = 'LOTES_VENCIDOS'  )  TO t_fcode[].
  ELSE.
    APPEND VALUE #( fcode = 'LOTES_VALIDOS'  )   TO t_fcode[].
  ENDIF.
*-US191954-30.09.2025-#191954-JT-fim

  SET PF-STATUS 'PF0108' EXCLUDING t_fcode.  "*-CS2025000249-16.06.2025-#182039-JT
  SET TITLEBAR 'T0108'.

*==== ALV1 ====
* PERFORM f_dados_alv1_108.

  IF go_cont_alv1_108 IS INITIAL.

*   PERFORM f_dados_alv1_108.

    PERFORM f_fieldcat_108.

    CREATE OBJECT go_cont_alv1_108
      EXPORTING
        container_name = 'CONTAINER_ALV1'.

    CREATE OBJECT go_alv1_108
      EXPORTING
        i_shellstyle    = 0
        i_parent        = go_cont_alv1_108
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    "FF #178787 - inicio aqui
    DATA: gr_events_108       TYPE REF TO lcl_event_receiver.
    CREATE OBJECT gr_events_108.

    SET HANDLER: gr_events_108->handle_on_button_click FOR go_alv1_108.
    "FF #178787 - fim


    gv_function_alv1 = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).
    gv_save_alv1 = abap_true.
    gv_variant_alv1-report = sy-repid.
    gv_layout_alv1 = VALUE #(
                       zebra      = abap_true
                       no_toolbar = abap_false
                       cwidth_opt = abap_false  "abap_true
                       sel_mode   = 'C'
                     ).

    gv_layout_alv1-grid_title = 'Solicitação Origem'.

    CALL METHOD go_alv1_108->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding          = gv_function_alv1
        is_layout                     = gv_layout_alv1
        i_save                        = gv_save_alv1
        i_default                     = abap_true
      CHANGING
        it_outtab                     = gt_dados_alv1
        it_fieldcatalog               = gt_alv1_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.
    CALL METHOD go_alv1_108->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.


*==== ALV2 ====
* PERFORM f_dados_alv2_108.

  IF go_cont_alv2_108 IS INITIAL.

    FREE: it_exclude_fcode.

*   PERFORM f_dados_alv2_108.

    PERFORM f_fieldcat_alv2_108.

    CREATE OBJECT go_cont_alv2_108
      EXPORTING
        container_name = 'CONTAINER_ALV2'.

    CREATE OBJECT go_alv2_108
      EXPORTING
        "i_shellstyle    = 0
        i_parent = go_cont_alv2_108
        "i_appl_events   = abap_false
        "i_fcat_complete = abap_false
      .

    APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_move_row      TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.

*"FF #178787 - inicio
    "    DATA: gr_events       TYPE REF TO lcl_event_receiver.
    CREATE OBJECT gr_events.

    CALL METHOD go_alv2_108->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD go_alv2_108->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: gr_events->on_toolbar_108   FOR go_alv2_108,
                 gr_events->on_f4            FOR go_alv2_108,
                 gr_events->handle_on_button_click2 FOR go_alv2_108,
                 lcl_event_handler=>on_data_changed FOR go_alv2_108.
*"FF #178787 - fim

    gv_function_alv2 = VALUE #( ( cl_gui_alv_grid=>mc_fc_excl_all ) ).
    gv_save_alv2 = abap_true.
    gv_variant_alv2-report = sy-repid.
    gv_layout_alv2 = VALUE #(
                       zebra      = abap_true
                       cwidth_opt = abap_false  "abap_true
                       sel_mode   = 'C'
                       no_toolbar = abap_false " abap_true
*                      edit       = abap_true
                       ctab_fname = 'COLOR'
                       stylefname = 'STYLE'
                     ).

    gv_layout_alv2-grid_title = 'Estoques Disponíveis'.

    SORT gt_dados_alv2 BY werks lifnr lgort.                "FF #169501

    CALL METHOD go_alv2_108->set_table_for_first_display
      EXPORTING
        "it_toolbar_excluding          = gv_function_alv2
        is_layout                     = gv_layout_alv2
        i_save                        = gv_save_alv2
        i_default                     = abap_true
        it_toolbar_excluding          = it_exclude_fcode
      CHANGING
        it_outtab                     = gt_dados_alv2  "gt_dados_alv2
        it_fieldcatalog               = gt_alv2_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    "FF #178787 - inicio
    DATA lt_f4 TYPE lvc_t_f4.
    DATA ls_f4 TYPE lvc_s_f4.

    FREE: lt_f4.
    ls_f4-fieldname   = 'FLEXIBILIDADE'.
    ls_f4-register    = abap_true.  " Ativa o F4
    ls_f4-getbefore   = abap_false.
    ls_f4-chngeafter  = abap_false.
    ls_f4-internal    = abap_false.
    APPEND ls_f4 TO lt_f4.

    ls_f4-fieldname   = 'NR_ROT_PC'.
    ls_f4-register    = abap_true.  " Ativa o F4
    ls_f4-getbefore   = abap_false.
    ls_f4-chngeafter  = abap_false.
    ls_f4-internal    = abap_false.
    APPEND ls_f4 TO lt_f4.

    CALL METHOD go_alv2_108->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4.

    "FF #178787 - fim


  ELSE.
    CALL METHOD go_alv2_108->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0108  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0108 INPUT.

  DATA: lv_sair_tela TYPE char01.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

*-CS2025000249-16.06.2025-#182039-JT-inicio
    WHEN 'OK'.
      go_alv2_108->check_changed_data( ).

      PERFORM f_efetuar_distribuicao    USING abap_off
                                     CHANGING lv_sair_tela.
      IF lv_sair_tela = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
**    PERFORM atualiza_qtd_vinc.

    WHEN 'REPROCESSAR'.
      go_alv2_108->check_changed_data( ).

      PERFORM f_efetuar_distribuicao    USING abap_on
                                     CHANGING lv_sair_tela.
      IF lv_sair_tela = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'VISUALIZAR'.
      go_alv2_108->check_changed_data( ).

      PERFORM f_visualizar_historico CHANGING lv_sair_tela.
      IF lv_sair_tela = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'ITINERARIO'.
      go_alv2_108->check_changed_data( ).

*     PERFORM f_criar_itinerario     CHANGING lv_sair_tela.
*-CS2025000249-16.06.2025-#182039-JT-fim

*-US191954-30.09.2025-#191954-JT-inicio
    WHEN 'MESMOS_CENTROS'.
      go_alv2_108->check_changed_data( ).
      PERFORM f_selecionar_centros USING abap_true.

    WHEN 'OUTROS_CENTROS'.
      go_alv2_108->check_changed_data( ).
      PERFORM f_selecionar_centros USING abap_false.

    WHEN 'LOTES_VENCIDOS'.
      go_alv2_108->check_changed_data( ).
      PERFORM f_exibir_lotes       USING abap_true.

    WHEN 'LOTES_VALIDOS'.
      go_alv2_108->check_changed_data( ).
      PERFORM f_exibir_lotes       USING abap_false.
*-US191954-30.09.2025-#191954-JT-fim

    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'MARCA'.


*      IF gv_agrupado = abap_false.
*        " Salva a tabela original antes de agrupar
*        gt_dados_alv2_original = gt_dados_alv2.
*        PERFORM agrupar_marca.
*        gv_agrupado = abap_true.
*
*      ELSE.
*        " Restaura a tabela original
*        gt_dados_alv2 = gt_dados_alv2_original.
*        gv_agrupado = abap_false.
*      ENDIF.
*
*      " Atualiza a tela do ALV
*      go_alv2_108->refresh_table_display( ).



      PERFORM agrupar_marca.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Form f_fieldcat_108
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_fieldcat_108 .


  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  gt_alv1_fieldcat = VALUE lit_fieldcat_aux(
( fieldname ='KUNNR'      coltext = 'Cliente'              ref_table = 'VBAK'     ref_field = 'KUNNR'        no_zero = '' outputlen = 8 )
( fieldname ='NAME1'      coltext = 'Nome do Cliente'      ref_table = 'KNA1'     ref_field = 'NAME1'        no_zero = '' outputlen = 10 )
"FF #169501 - inicio
( fieldname ='ORT01'      coltext = 'Cidade'               ref_table = 'KNA1'     ref_field = 'ORT01'        no_zero = '' outputlen = 10 )
( fieldname ='REGIO'      coltext = 'Estado'               ref_table = 'KNA1'     ref_field = 'REGIO'        no_zero = '' outputlen = 3 )
"FF #169501 - fim
( fieldname ='VKBUR'      coltext = 'Escritório de vendas' ref_table = 'VBAK'     ref_field = 'VKBUR'        no_zero = '' outputlen = 4 )
( fieldname ='NRO_SOL'    coltext = 'Solicitação'          ref_table = 'ZSDT0082' ref_field = 'NRO_SOL'      no_zero = '' outputlen = 10 )
( fieldname ='VBELN'      coltext = 'Contrato'             ref_table = 'VBAP'     ref_field = 'VBELN'        no_zero = '' outputlen = 10 )
( fieldname ='POSNR'      coltext = 'Item'                 ref_table = 'VBAP'     ref_field = 'POSNR'        no_zero = '' outputlen = 3 )
( fieldname ='LIFNR_PC'   coltext = 'Ponto Coleta'         ref_table = 'LFA1'     ref_field = 'LIFNR'        no_zero = '' outputlen = 10 )   "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP
( fieldname ='WERKS'      coltext = 'Centro'               ref_table = 'VBAP'     ref_field = 'WERKS'        no_zero = '' outputlen = 5 )
( fieldname ='LGORT'      coltext = 'Deposito'             ref_table = 'VBAP'     ref_field = 'LGORT'        no_zero = '' outputlen = 5 )
( fieldname ='INCO1'      coltext = 'Incoterms'            ref_table = 'VBKD'     ref_field = 'INCO1'        no_zero = '' outputlen = 5 )

"FF #169501 - inicio
( fieldname ='MATNR'      coltext = 'Material'             ref_table = 'VBAP'     ref_field = 'MATNR'        no_zero = '' outputlen = 7 )
( fieldname ='ARKTX'      coltext = 'Descrição Material'   ref_table = 'VBAP'     ref_field = 'ARKTX'        no_zero = '' outputlen = 10 )
( fieldname ='WRKST'      coltext = 'Marca'                ref_table = 'MARA'     ref_field = 'WRKST'        no_zero = '' outputlen = 07 )
( fieldname ='QTE_SOL'    coltext = 'Qte. Solicitado'      ref_table = 'VBAP'     ref_field = 'KWMENG'       no_zero = '' outputlen = 08 )
( fieldname ='QTE_SDO_LIB' coltext = 'Sdo.a Lib.'          ref_table = 'VBAP'     ref_field = 'KWMENG'       no_zero = '' outputlen = 08 )
"FF #169501 - fim
( fieldname ='VENDA_ESP'  coltext = 'Venda Especial'       ref_table = 'ZSDS093'     ref_field = 'VENDA_ESP'  no_zero = '' checkbox = abap_true outputlen = 10 ) "FF #178787
( fieldname ='OBSERVACAO_VE'  coltext = 'Observação Venda Especial'       ref_table = 'ZSDS093'     ref_field = 'OBSERVACAO_VE'
                              no_zero = '' style = cl_gui_alv_grid=>mc_style_button outputlen = 10 ) "FF #178787
( fieldname ='NR_ROT'     coltext = 'Nro.Roteiro'          ref_table = 'ZSDT0082' ref_field = 'NR_ROT'       no_zero = '' outputlen = 08 )
( fieldname ='DESC_ROT'   coltext = 'Desc.Roteiro'         ref_table = '' ref_field = ''
                              no_zero = '' style = cl_gui_alv_grid=>mc_style_button  outputlen = 08 )
( fieldname ='DT_ENTREGA' coltext = 'Dt.Entrega'           ref_table = 'ZSDT0082' ref_field = 'DT_ENTREGA'   no_zero = '' outputlen = 10 )
( fieldname ='TP_OPER_DESC' coltext = 'Tp.Oper'            ref_table = ''         ref_field = ''             no_zero = '' outputlen = 07 )
).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_dados_alv1_108
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_dados_alv1_108 .

  FREE: gt_dados_alv1, t_centros_defens.

  LOOP AT t_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).
    APPEND INITIAL LINE TO gt_dados_alv1 ASSIGNING FIELD-SYMBOL(<fs_alv1>).
    MOVE-CORRESPONDING <fs_aux> TO <fs_alv1>.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_dados_alv2_108
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_dados_alv2_108 USING p_lotes_vencidos.

  DATA: w_dados_alv2_dado TYPE zsds094.

  FREE: gt_dados_alv2, gt_dados_alv2_dado.

  LOOP AT t_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).
    APPEND INITIAL LINE TO gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_alv2>).
*   <fs_aux>-flexibilidade        = 1.
*   <fs_aux>-carga_auto           = 'X'.
    MOVE-CORRESPONDING <fs_aux>  TO <fs_alv2>.
    MOVE-CORRESPONDING <fs_aux>  TO w_dados_alv2_dado.
    APPEND w_dados_alv2_dado     TO gt_dados_alv2_dado.
  ENDLOOP.

  DATA(lo_busca) = NEW zcl_sd_distribuir_saldo( ).

  lo_busca->busca_dados( EXPORTING cg_dados_cabec = gt_dados_alv1
                         CHANGING  cg_dados       = gt_dados_alv2_dado ).

  "FF #178787 - inicio
  FREE: t_color.
  w_color-fname     = 'FLEXIBILIDADE'.
  w_color-color-col = 3.
  w_color-color-int = 1.
  w_color-color-inv = 0.
  APPEND w_color   TO t_color.
  w_color-fname     = 'NR_ROT_PC'.
  w_color-color-col = 3.
  w_color-color-int = 1.
  w_color-color-inv = 0.
  APPEND w_color   TO t_color.

  SELECT SINGLE usuario
    INTO @DATA(_usuario)
    FROM zsdt0398
   WHERE usuario = @sy-uname.

  DATA(lv_edit_priorid) = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_off ).

  FREE: gt_dados_alv2.

  LOOP AT gt_dados_alv2_dado ASSIGNING FIELD-SYMBOL(<fs_dado>).
    FREE: t_style, w_dados_alv2.
*   <fs_dado>-flexibilidade = 1.
*   <fs_dado>-carga_auto    = 'X'.
    MOVE-CORRESPONDING <fs_dado> TO w_dados_alv2.
    w_dados_alv2-hist       = icon_history.
    w_dados_alv2-itinerario = icon_car.
    w_dados_alv2-color      = t_color[].

    IF <fs_dado>-ebeln IS NOT INITIAL.
      wa_style-fieldname = 'MARCA'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND wa_style   TO t_style.
    ELSE.
      wa_style-fieldname = 'MARCA'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_style   TO t_style.
    ENDIF.

    wa_style-fieldname = 'PRIORIDADE'.
    wa_style-style     = COND #( WHEN lv_edit_priorid = abap_true THEN cl_gui_alv_grid=>mc_style_enabled ELSE cl_gui_alv_grid=>mc_style_disabled ).
    APPEND wa_style   TO t_style.

    IF <fs_dado>-nao_editar = abap_true.
      wa_style-fieldname = 'KWMENG'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
      APPEND wa_style   TO t_style.
*     wa_style-fieldname = 'DT_ENTREGA'.
*     wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*     APPEND wa_style   TO t_style.
*     wa_style-fieldname = 'NR_ROT_PC'.
*     wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*     APPEND wa_style   TO t_style.
*     wa_style-fieldname = 'PRIORIDADE'.
*     wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*     APPEND wa_style   TO t_style.
*     wa_style-fieldname = 'FLEXIBILIDADE'.
*     wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*     APPEND wa_style   TO t_style.
*     wa_style-fieldname = 'CARGA_AUTO'.
*     wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*     APPEND wa_style   TO t_style.
*     wa_style-fieldname = 'TRANSF_NO_FORNECEDOR'.
*     wa_style-style     = cl_gui_alv_grid=>mc_style_disabled.
*     APPEND wa_style   TO t_style.
    ELSE.
*     wa_style-fieldname = 'KWMENG'.   "*-CS2025000249-04.09.2025-#189804-JT-inicio
*     wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
*     APPEND wa_style   TO t_style.
      wa_style-fieldname = 'DT_ENTREGA'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND wa_style   TO t_style.
      wa_style-fieldname = 'NR_ROT_PC'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND wa_style   TO t_style.
*      wa_style-fieldname = 'PRIORIDADE'.
*      wa_style-style     = COND #( WHEN lv_edit_priorid = abap_true THEN cl_gui_alv_grid=>mc_style_enabled ELSE cl_gui_alv_grid=>mc_style_disabled ).
*      APPEND wa_style   TO t_style.
      wa_style-fieldname = 'FLEXIBILIDADE'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND wa_style   TO t_style.
      wa_style-fieldname = 'CARGA_AUTO'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND wa_style   TO t_style.
      wa_style-fieldname = 'TRANSF_NO_FORNECEDOR'.
      wa_style-style     = cl_gui_alv_grid=>mc_style_enabled.
      APPEND wa_style   TO t_style.
    ENDIF.
    w_dados_alv2-style[]    = t_style[].
    APPEND w_dados_alv2    TO gt_dados_alv2.
  ENDLOOP.

  IF p_lotes_vencidos = abap_false.
    DELETE gt_dados_alv2 WHERE vfdat < sy-datum.
  ENDIF.

  "F #178787 - fim
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fieldcat_alv2_108
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        textho
*&---------------------------------------------------------------------*
FORM f_fieldcat_alv2_108 .

  "FF #178787 - inicio

  SELECT SINGLE usuario
  INTO @DATA(lv_0398)
  FROM zsdt0398
  WHERE usuario = @sy-uname.

  IF sy-subrc = 0.
    DATA(lv_editar) = abap_true.
  ELSE.
    lv_editar = abap_false.
  ENDIF.
  "FF #178787 - FIM

*-CS2025000249-16.06.2025-#182039-JT-inicio
  READ TABLE gt_dados_alv2 INTO DATA(_alv2) INDEX 1.
  IF lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = _alv2-nro_sol i_seq   = _alv2-seq
                                            i_vbeln   = _alv2-vbeln   i_posnr = _alv2-posnr )-bloqueio = abap_true.
    lv_editar2 = abap_false.
    lv_editar  = abap_false.
  ELSE.
    lv_editar2 = abap_true.
  ENDIF.
*-CS2025000249-16.06.2025-#182039-JT-inicio

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.
  gt_alv2_fieldcat = VALUE lit_fieldcat_aux(
( fieldname ='HIST'   coltext = ''                   ref_table = ''         ref_field = ''      no_zero = '' outputlen = 4
                      style = cl_gui_alv_grid=>mc_style_button fix_column = abap_true )
( fieldname ='WERKS'  coltext = 'Centro'             ref_table = 'VBAP'     ref_field = 'WERKS' no_zero = '' outputlen = 4 fix_column = abap_true )
( fieldname ='LIFNR'  coltext = 'Fornecedor Armz.'   ref_table = 'LFA1'     ref_field = 'LIFNR' no_zero = '' outputlen = 8 fix_column = abap_true )
( fieldname ='MUNICIPIO_FORN'  coltext = 'Municipio' ref_table = 'LFA1'     ref_field = 'ORT01' no_zero = '' outputlen = 6 fix_column = abap_true )
( fieldname ='LGORT'  coltext = 'Deposito'           ref_table = 'VBAP'     ref_field = 'LGORT' no_zero = '' outputlen = 4 fix_column = abap_true )
( fieldname ='EBELN'  coltext = 'Pedido'             ref_table = 'EKKO'     ref_field = 'EBELN' no_zero = '' outputlen = 10 fix_column = abap_true )
( fieldname ='EBELP'  coltext = 'Item'               ref_table = 'EKKO'     ref_field = 'EBELP' no_zero = '' outputlen = 5 fix_column = abap_true )
( fieldname ='LIFNR_PEDIDO'  coltext = 'Forn.Pedido' ref_table = 'LFA1'     ref_field = 'LIFNR' no_zero = '' outputlen = 8 fix_column = abap_true )
( fieldname ='RAZAO_SOCIAL_FORN_PED'  coltext = 'Razão Social' ref_table = 'LFA1'     ref_field = 'NAME1' no_zero = '' outputlen = 6 fix_column = abap_true )
( fieldname ='LIFNR_PC'  coltext = 'Ponto Coleta'    ref_table = 'LFA1'     ref_field = 'LIFNR' no_zero = '' outputlen = 10 fix_column = abap_true )

( fieldname ='MATNR'  coltext = 'Material'           ref_table = 'VBAP'     ref_field = 'MATNR' no_zero = '' outputlen = 6 fix_column = abap_true )
( fieldname ='ARKTX'  coltext = 'Descrição Material' ref_table = 'VBAP'     ref_field = 'ARKTX' no_zero = '' outputlen = 10 fix_column = abap_true )
( fieldname ='MEINS'  coltext = 'Unid.'              ref_table = 'VBAP'     ref_field = 'MEINS' no_zero = '' outputlen = 4 fix_column = abap_true )
( fieldname ='MTART'  coltext = 'Tipo material'      ref_table = 'MARA'     ref_field = 'MTART' no_zero = '' outputlen = 5 fix_column = abap_true )
( fieldname ='MARCA'  coltext = 'Marca'              ref_table = 'ZSDS094'  ref_field = 'MARCA' no_zero = '' outputlen = 08 fix_column = abap_true )
( fieldname ='CHARG'  coltext = 'Lote'               ref_table = 'MCHB'     ref_field = 'CHARG' no_zero = '' outputlen = 10 fix_column = abap_true )
( fieldname ='GERMINACAO'     coltext = 'Germinação'     ref_table = ''     ref_field = '' no_zero = '' outputlen = 5 )
( fieldname ='CATEG_SEMENTE'  coltext = 'Categ.Semente'  ref_table = ''     ref_field = '' no_zero = '' outputlen = 5  )
( fieldname ='LICHA'  coltext = 'Lote Fornecedor'    ref_table = 'MCHA'     ref_field = 'LICHA' no_zero = '' outputlen = 10 )
( fieldname ='VFDAT'  coltext = 'Data Val.'          ref_table = 'MCH1'     ref_field = 'VFDAT' no_zero = '' outputlen = 9 )
( fieldname ='MENGE_PED'   coltext = 'Qtd Pedido'    ref_table = 'EKPO'     ref_field = 'MENGE' no_zero = '' outputlen = 9 )
( fieldname ='MENGE_FAT'   coltext = 'Qtd Faturado'  ref_table = 'EKPO'     ref_field = 'MENGE' no_zero = '' outputlen = 9 )
( fieldname ='MENGE_CONSUMO' coltext = 'Qtde Consum' ref_table = 'VBAP'     ref_field = 'KWMENG' no_zero = abap_true edit = abap_off outputlen = 9 ) "*-CS2025000249-16.06.2025-#182039-JT
( fieldname ='CLABS'   coltext = 'Saldo'             ref_table = 'MCHB'     ref_field = 'CLABS' no_zero = '' outputlen = 9 )
*( fieldname ='KWMENG'  coltext = 'Qtde a Lib.'       ref_table = 'VBAP'     ref_field = 'KWMENG' no_zero = abap_true edit = abap_true outputlen = 9 ) "*-CS2025000249-16.06.2025-#182039-JT
( fieldname ='KWMENG'  coltext = 'Qtde a Lib.'       ref_table = 'VBAP'     ref_field = 'KWMENG' no_zero = abap_true edit = lv_editar2 outputlen = 9 ) "*-CS2025000249-16.06.2025-#182039-JT
( fieldname ='DT_ENTREGA' coltext = 'Data Entrega'   ref_table = 'ZSDT0082' ref_field = 'DT_ENTREGA' no_zero = '' edit = abap_true  outputlen = 10 ) "*-CS2025000249-16.06.2025-#182039-JT
*( fieldname ='KWMENG'  coltext = 'Sdo.a Lib.'        ref_table = 'VBAP'     ref_field = 'KWMENG' no_zero = '' edit = 'X' )
*( fieldname ='DT_ENTREGA' coltext = 'Data Entrega'   ref_table = 'ZSDT0082' ref_field = 'DT_ENTREGA' no_zero = '' edit = 'X'  )
"FF #178787 - inicio

( fieldname ='NR_ROT_PC'             coltext = 'Roteiro Ponto Coleta'         ref_table = 'ZSDT0082'    ref_field = 'NR_ROT_PC'     no_zero = ''  edit = ' ' f4availabl = 'X' outputlen = 10 )
( fieldname ='ROUTE'                 coltext = 'Itinerário'                   ref_table = 'TROLZ'       ref_field = 'ROUTE'   no_zero = ''  edit = ' ' outputlen = 05 )
( fieldname ='ITINERARIO' coltext = 'Criar.Itinerario' ref_table = '' ref_field = '' no_zero = '' outputlen = 6 style = cl_gui_alv_grid=>mc_style_button )
( fieldname ='PRIORIDADE'            coltext = 'Prioridade'                   ref_table = 'ZSDT0082'    ref_field = 'PRIORIDADE'
                                     no_zero = ''  edit = abap_true  checkbox = abap_true outputlen = 10 )
( fieldname ='FLEXIBILIDADE'         coltext = 'Flexibilidade'                ref_table = 'ZSDT0082'    ref_field = 'FLEXIBILIDADE'
                                     no_zero = ''  edit = abap_off f4availabl = 'X' outputlen = 10 )
( fieldname ='CARGA_AUTO'            coltext = 'Carga Automática'  no_zero = ''  edit = abap_true  checkbox  = 'X' outputlen = 10 )
( fieldname ='TRANSF_NO_FORNECEDOR'  coltext = 'Transf.Forn'  ref_table = 'ZSDT0082'    ref_field = 'TRANSF_NO_FORNECEDOR' no_zero = ''  edit = abap_true  checkbox  = 'X' outputlen = 10 )
*( fieldname ='FLEXIBILIDADE'         coltext = 'Flexibilidade'                ref_table = 'ZSDT0082'    ref_field = 'FLEXIBILIDADE' no_zero = ''  edit = 'X' )
*( fieldname ='CARGA_AUTO'            coltext = 'Carga Automática'                                                                   no_zero = ''  edit = 'X' checkbox  = 'X' )
*( fieldname ='TRANSF_NO_FORNECEDOR'  coltext = 'Transferencia no Fornecedor'  ref_table = 'ZSDT0082'    ref_field = 'TRANSF_NO_FORNECEDOR' no_zero = ''  edit = 'X' checkbox  = 'X' )
"FF #178787 - fimm

).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form AGRUPAR_MARCA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM agrupar_marca .

*  "FF #169501 - inicio
**  DATA: lt_dados TYPE zsds094_tt,
**        ls_dados TYPE zsds094.
**
**
**  LOOP AT gt_dados_alv2 ASSIGNING FIELD-SYMBOL(<fs_dados>).
**    MOVE-CORRESPONDING <fs_dados> TO ls_dados.
**    CLEAR: ls_dados-charg.
**    COLLECT ls_dados INTO lt_dados.
**  ENDLOOP.
**
**  gt_dados_alv2 = lt_dados.
*
*
*  TYPES: BEGIN OF ty_dados_agrupados,
*           marca TYPE zsds094,
*           werks TYPE werks_d,
*           lifnr TYPE lifnr,
*           lgort TYPE lgort,
*           matnr TYPE matnr,
*           clabs TYPE mchb-clabs,
*         END OF ty_dados_agrupados.
*
*  DATA: lt_agrupado TYPE SORTED TABLE OF ty_dados_agrupados
*                       WITH UNIQUE KEY marca werks lifnr lgort matnr,
*        ls_agrupado TYPE ty_dados_agrupados.
*
*  CLEAR lt_agrupado.
*
*  LOOP AT gt_dados_alv2 INTO DATA(ls_dado).
*    READ TABLE lt_agrupado INTO ls_agrupado
*      WITH KEY marca = ls_dado-marca
*               werks = ls_dado-werks
*               lifnr = ls_dado-lifnr
*               lgort = ls_dado-lgort
*               matnr = ls_dado-matnr.
*
*    IF sy-subrc = 0.
*      ls_agrupado-clabs = ls_agrupado-clabs + ls_dado-clabs.
*      MODIFY lt_agrupado FROM ls_agrupado
*             TRANSPORTING clabs
*             WHERE marca = ls_agrupado-marca
*               AND werks = ls_agrupado-werks
*               AND lifnr = ls_agrupado-lifnr
*               AND lgort = ls_agrupado-lgort
*               AND matnr = ls_agrupado-matnr.
*    ELSE.
*
*      MOVE-CORRESPONDING ls_dado TO ls_agrupado.
*      ls_agrupado-clabs = ls_dado-clabs.
*      INSERT ls_agrupado INTO TABLE lt_agrupado.
*    ENDIF.
*
*  ENDLOOP.
*
*  " Substitui a tabela do ALV pela agrupada
*  gt_dados_alv2 = lt_agrupado.
*
*
*  "FF #169501 - fim
*
*  go_alv2_108->refresh_table_display( ).

ENDFORM.

********************************************************
* bloqueio por cultivar
********************************************************
FORM f_lock_cultivar    USING p_vkorg
                              p_matnr
                     CHANGING p_erro.

  FREE: p_erro.

  SELECT SINGLE *
    INTO @DATA(_tvarvc)
    FROM tvarvc
   WHERE name = 'ZSDT0081_LOCK'
     AND low = @sy-uname.

  CHECK sy-subrc <> 0.

* IF sy-uname = 'JTASSONI' OR sy-uname = 'WPPEREIRA'.
*   RETURN.
* ENDIF.

  lv_object = p_matnr.

  lc_mm_util->get_caracteristica_material( EXPORTING i_class      = 'SEMENTES_GERAL'
                                                     i_object     = lv_object
                                           IMPORTING t_objectdata = t_objectdata ).

  READ TABLE t_objectdata INTO DATA(_objectdata) WITH KEY atnam = 'NOVA_CULTIVAR'.
  IF sy-subrc = 0.
    lv_obj_bloq = _objectdata-ausp1.
  ELSE.
    lv_obj_bloq = p_matnr.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZSDR0038'
    EXPORTING
      vkorg          = p_vkorg
      matnr          = lv_obj_bloq
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    DATA(l_user_lock) = sy-msgv1.
    MESSAGE s024(sd) WITH 'Solicitacao Bloqueada Cultivar:' lv_obj_bloq 'pelo usuário:' l_user_lock.
    p_erro = abap_true.
  ENDIF.

ENDFORM.

********************************************************
* desbloqueio por cultivar
********************************************************
FORM f_unlock_cultivar USING p_vkorg
                             p_matnr.

  lv_object = p_matnr.

  lc_mm_util->get_caracteristica_material( EXPORTING i_class      = 'SEMENTES_GERAL'
                                                     i_object     = lv_object
                                           IMPORTING t_objectdata = t_objectdata ).

  READ TABLE t_objectdata INTO DATA(_objectdata) WITH KEY atnam = 'NOVA_CULTIVAR'.
  IF sy-subrc = 0.
    lv_obj_bloq = _objectdata-ausp1.
  ELSE.
    lv_obj_bloq = p_matnr.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_EZSDR0038'
    EXPORTING
      vkorg          = p_vkorg
      matnr          = lv_obj_bloq
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.

********************************************************
* bloqueio por centro + principio ativo
********************************************************
FORM f_lock_defensivo   USING p_werks
                              p_matnr
                     CHANGING p_erro
                              p_user_lock.

  DATA: lv_mesg TYPE string.

  FREE: p_erro, p_user_lock.

  SELECT SINGLE *
    INTO @DATA(_tvarvc)
    FROM tvarvc
   WHERE name = 'ZSDT0081_LOCK'
     AND low = @sy-uname.

  CHECK sy-subrc <> 0.

* IF sy-uname = 'JTASSONI' OR sy-uname = 'WPPEREIRA'.
*   RETURN.
* ENDIF.

  lv_object = p_matnr.

  lc_mm_util->get_caracteristica_material( EXPORTING i_class      = 'PRINCICIO_ATIVO'
                                                     i_object     = lv_object
                                           IMPORTING t_objectdata = t_objectdata ).

  READ TABLE t_objectdata INTO DATA(_objectdata) WITH KEY atnam = 'PRINCICIO_ATIVO'.
  IF sy-subrc = 0.
    lv_obj_bloq = _objectdata-ausp1.
  ELSE.
    lv_obj_bloq = p_matnr.
  ENDIF.

  CALL FUNCTION 'ENQUEUE_EZSDR0038'
    EXPORTING
      vkorg          = p_werks
      matnr          = lv_obj_bloq
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    p_user_lock = sy-msgv1.
    lv_mesg = ' Centro: ' && | { p_werks } | && 'Princ.Ativo:' && | { lv_obj_bloq ALPHA = OUT } |.
*   MESSAGE s024(sd) WITH 'Distribuições Bloqueadas:' lv_mesg 'pelo usuário:' l_user_lock.
    p_erro = abap_true.
  ENDIF.

ENDFORM.

********************************************************
* desbloqueio defensivos
********************************************************
FORM f_debloqueio_defensivos.

  DATA: t_dados_alv2_werks TYPE TABLE OF ty_centros_defens.

  t_dados_alv2_werks[] = t_centros_defens[].

  SORT t_dados_alv2_werks BY werks.
  DELETE ADJACENT DUPLICATES FROM t_dados_alv2_werks COMPARING werks.

  LOOP AT t_dados_alv2_werks INTO DATA(_dados_alv2).
    PERFORM f_unlock_defensivo USING _dados_alv2-werks _dados_alv2-matnr.
  ENDLOOP.

  FREE: t_centros_defens.

ENDFORM.

********************************************************
* desbloqueio por centro + principio ativo
********************************************************
FORM f_unlock_defensivo   USING p_werks
                                p_matnr.

  lv_object = p_matnr.

  lc_mm_util->get_caracteristica_material( EXPORTING i_class      = 'PRINCICIO_ATIVO'
                                                     i_object     = lv_object
                                           IMPORTING t_objectdata = t_objectdata ).

  READ TABLE t_objectdata INTO DATA(_objectdata) WITH KEY atnam = 'PRINCICIO_ATIVO'.
  IF sy-subrc = 0.
    lv_obj_bloq = _objectdata-ausp1.
  ELSE.
    lv_obj_bloq = p_matnr.
  ENDIF.

  CALL FUNCTION 'DEQUEUE_EZSDR0038'
    EXPORTING
      vkorg          = p_werks
      matnr          = lv_obj_bloq
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

ENDFORM.

INCLUDE zsdr0038_forms.  "*-CS2025000249-16.06.2025-#182039-JT
*&---------------------------------------------------------------------*
*& Form f_cancela_solicitacao
*&---------------------------------------------------------------------*
FORM f_cancela_solicitacao .

  DATA lv_erro.
  DATA ls_layout TYPE lvc_s_layo.
  DATA is_table        TYPE lvc_s_stbl.
  DATA lo_grid TYPE REF TO  cl_gui_alv_grid.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = lo_grid.

  CHECK lo_grid IS NOT INITIAL.

  CALL METHOD lo_grid->get_selected_rows
    IMPORTING
      et_index_rows = DATA(lt_rows).

  IF lt_rows[] IS INITIAL.
    MESSAGE  'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).

    READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_rows>-index.

    CHECK sy-subrc EQ 0.

    DATA(lv_mess) = zcl_solicitacao_saida_insumos=>cancelar( EXPORTING
                      iv_nro_sol = <fs_saida>-nro_sol
                      iv_vbeln =  <fs_saida>-vbeln
                      iv_posnr =  <fs_saida>-posnr
                      iv_popup_to_confirm = abap_true
                      IMPORTING ev_erro = lv_erro ).

    IF lv_mess IS INITIAL AND lv_erro IS INITIAL.
      DELETE t_saida INDEX <fs_rows>-index.
    ELSE.

      zcl_solicitacao_saida_insumos=>show_message( lv_mess ).
    ENDIF.

  ENDLOOP.

  " caso nao tenha mais linha, retira o total
  IF t_saida[] IS INITIAL.

    lo_grid->get_frontend_layout( IMPORTING es_layout = ls_layout ).

    ls_layout-no_totline = abap_true.

    CALL METHOD lo_grid->set_frontend_layout
      EXPORTING
        is_layout = ls_layout.
  ENDIF.


  CALL METHOD lo_grid->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  CALL METHOD cl_gui_cfw=>dispatch.
  CALL METHOD cl_gui_cfw=>flush.


ENDFORM.
