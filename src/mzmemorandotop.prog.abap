*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDOTOP .
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: zdoc_memo_nf_exp,
        zdoc_memorando,
        zdoc_memo_protoc,
        zdoc_memo_nr,
        j_1bnfdoc,
        t001w,
        j_1bnflin,
        kna1,
        lfa1.

*----------------------------------------------------------------------*
* Bibliotecas
*----------------------------------------------------------------------*
TYPE-POOLS: zmemo, icon.

TYPES: BEGIN OF ty_zmemo_export_acomp.
         INCLUDE STRUCTURE zexport_acomp.
         TYPES: qtde_baixadas TYPE zsdt0276-menge,
         mark          TYPE char1,
         dt_emissao    TYPE zexport_notas-dt_emissao,
       END OF ty_zmemo_export_acomp.

TYPES: BEGIN OF ty_zexport_acomp.
         INCLUDE STRUCTURE zexport_acomp.
         TYPES: qtde_baixadas TYPE zsdt0276-menge,
       END OF ty_zexport_acomp.

TYPES: BEGIN OF ty_zexport_notas.
         INCLUDE STRUCTURE zexport_notas.
         TYPES: qtde_baixadas TYPE zsdt0276-menge,
       END OF ty_zexport_notas.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click
                FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_column_id
                es_row_no.
    METHODS on_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row e_column.
ENDCLASS.                    "lcl_event_handler DEFINITION


*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: c_fcode_save      TYPE sy-ucomm   VALUE 'SAVE',
           c_fcode_csnfms    TYPE sy-ucomm   VALUE 'CSNFMS',
           c_fcode_csnfmm    TYPE sy-ucomm   VALUE 'CSNFMM',
           c_fcode_libmemo   TYPE sy-ucomm   VALUE 'LIBMEMO',
           c_fcode_libresp   TYPE sy-ucomm   VALUE 'LIBRESP',
           c_fcode_nfpropria TYPE sy-ucomm   VALUE 'NFPROPRIA',
           c_legenda         TYPE sy-ucomm   VALUE 'LEGENDA',
           c_a               TYPE c LENGTH 1 VALUE 'A',
           c_c               TYPE c LENGTH 1 VALUE 'C',
           c_e               TYPE c LENGTH 1 VALUE 'E',
           c_f               TYPE c LENGTH 1 VALUE 'F',
           c_i               TYPE c LENGTH 1 VALUE 'I',
           c_j               TYPE c LENGTH 1 VALUE 'J',
           c_m               TYPE c LENGTH 1 VALUE 'M',
           c_n               TYPE c LENGTH 1 VALUE 'N',
           c_p               TYPE c LENGTH 1 VALUE 'P',
           c_r               TYPE c LENGTH 1 VALUE 'R',
           c_s               TYPE c LENGTH 1 VALUE 'S',
           c_t               TYPE c LENGTH 1 VALUE 'T',
           c_v               TYPE c LENGTH 1 VALUE 'V',
           c_x               TYPE c LENGTH 1 VALUE 'X',
           c_0               TYPE c LENGTH 1 VALUE '0',
           c_1               TYPE c LENGTH 1 VALUE '1',
           c_2               TYPE c LENGTH 1 VALUE '2',
           c_5               TYPE c LENGTH 1 VALUE '5',
           c_01              TYPE c LENGTH 2 VALUE '01',
           c_55              TYPE c LENGTH 2 VALUE '55',
           c_zcic            TYPE c LENGTH 4 VALUE 'ZCIC',
           c_zfic            TYPE c LENGTH 4 VALUE 'ZFIC',

           c_tabcd           TYPE c LENGTH 5 VALUE 'TABCD',
           c_tabmm           TYPE c LENGTH 5 VALUE 'TABMM',
           c_tabnx           TYPE c LENGTH 5 VALUE 'TABNX',
           c_tabbl           TYPE c LENGTH 5 VALUE 'TABBL',

           c_csmemo          TYPE c LENGTH 6 VALUE 'CSMEMO',
           c_edmemo          TYPE c LENGTH 6 VALUE 'EDMEMO',
           c_lancme          TYPE c LENGTH 6 VALUE 'LANCME',
           c_lancnv          TYPE c LENGTH 6 VALUE 'LANCNV',
           c_libmemo         TYPE c LENGTH 7 VALUE 'LIBMEMO',
           c_libresp         TYPE c LENGTH 7 VALUE 'LIBRESP',
           c_dlmemo          TYPE c LENGTH 6 VALUE 'DLMEMO',
           c_csproto         TYPE c LENGTH 7 VALUE 'CSPROTO',
           c_csnfmm          TYPE c LENGTH 6 VALUE 'CSNFMM',
           c_nfpropria       TYPE c LENGTH 9 VALUE 'NFPROPRIA',
           c_csnfms          TYPE c LENGTH 6 VALUE 'CSNFMS',
           c_csprint         TYPE c LENGTH 7 VALUE 'CSPRINT',
           c_csnotas         TYPE c LENGTH 7 VALUE 'CSNOTAS',
           c_back            TYPE c LENGTH 4 VALUE 'BACK',
           c_exit            TYPE c LENGTH 4 VALUE 'EXIT',
           c_save            TYPE c LENGTH 4 VALUE 'SAVE',
           c_cancel          TYPE c LENGTH 6 VALUE 'CANCEL',
           c_backv           TYPE c LENGTH 5 VALUE 'BACKV',
           c_exitv           TYPE c LENGTH 5 VALUE 'EXITV',
           c_cancelv         TYPE c LENGTH 7 VALUE 'CANCELV',
           c_cplnf           TYPE c LENGTH 5 VALUE 'CPLNF',
           c_delmemo         TYPE c LENGTH 7 VALUE 'DELMEMO',
           c_dlcancel        TYPE c LENGTH 8 VALUE 'DLCANCEL',
           c_altmemo         TYPE c LENGTH 7 VALUE 'ALTMEMO',
           c_addmemo         TYPE c LENGTH 7 VALUE 'ADDMEMO',
           c_bldelt          TYPE c LENGTH 6 VALUE 'BLDELT',
           c_blnovo          TYPE c LENGTH 6 VALUE 'BLNOVO',
           c_apcons          TYPE c LENGTH 6 VALUE 'APCONS',
           c_btpesq          TYPE c LENGTH 6 VALUE 'BTPESQ',
           c_btvinf          TYPE c LENGTH 6 VALUE 'BTVINF',
           c_btdenf          TYPE c LENGTH 6 VALUE 'BTDENF',
           c_csnfmt          TYPE c LENGTH 6 VALUE 'CSNFMT',
           c_dlproto         TYPE c LENGTH 7 VALUE 'DLPROTO',
           c_cancelar        TYPE c LENGTH 8 VALUE 'CANCELAR',
           "Abas de Filtro
           c_flt01           TYPE c LENGTH 5 VALUE 'FLT01',
           c_flt02           TYPE c LENGTH 5 VALUE 'FLT02',
           c_flt03           TYPE c LENGTH 5 VALUE 'FLT03',
           "Abas de Resultado de Pesquisa
           c_tabmemo         TYPE c LENGTH 7 VALUE 'TABMEMO',
           c_tabnota         TYPE c LENGTH 7 VALUE 'TABNOTA',
           "Status GUI 5050
           c_apcone          TYPE c LENGTH 6 VALUE 'APCONE',
           c_csnfme          TYPE c LENGTH 6 VALUE 'CSNFME',
           c_csnfmo          TYPE c LENGTH 6 VALUE 'CSNFMO',
           c_lancne          TYPE c LENGTH 6 VALUE 'LANCNE',
           c_bcgrd           TYPE c LENGTH 5 VALUE 'BCGRD',
           "Status GUI 8000
           c_prpesq          TYPE c LENGTH 6 VALUE 'PRPESQ',
           c_vcmemo          TYPE c LENGTH 6 VALUE 'VCMEMO',
           c_btmmv           TYPE c LENGTH 6 VALUE 'BTMMV',
           c_btmmd           TYPE c LENGTH 6 VALUE 'BTMMD',
           c_btcan           TYPE c LENGTH 6 VALUE 'BTCAN',

           c_1000            LIKE sy-dynnr   VALUE '1000',
           c_1001            LIKE sy-dynnr   VALUE '1001',
           c_1002            LIKE sy-dynnr   VALUE '1002',
           c_1003            LIKE sy-dynnr   VALUE '1003',
           c_1004            LIKE sy-dynnr   VALUE '1004',
           c_1005            LIKE sy-dynnr   VALUE '1005',
           c_2000            LIKE sy-dynnr   VALUE '2000',
           c_2001            LIKE sy-dynnr   VALUE '2001',
           c_2002            LIKE sy-dynnr   VALUE '2002',
           c_2003            LIKE sy-dynnr   VALUE '2003',
           c_2004            LIKE sy-dynnr   VALUE '2004',
           c_2005            LIKE sy-dynnr   VALUE '2005',
           c_2006            LIKE sy-dynnr   VALUE '2006',
           c_4000            LIKE sy-dynnr   VALUE '4000',
           c_4001            LIKE sy-dynnr   VALUE '4001',
           c_4002            LIKE sy-dynnr   VALUE '4002',
           c_4003            LIKE sy-dynnr   VALUE '4003',
           c_5000            LIKE sy-dynnr   VALUE '5000',
           c_5001            LIKE sy-dynnr   VALUE '5001',
           c_5002            LIKE sy-dynnr   VALUE '5002',
           c_5050            LIKE sy-dynnr   VALUE '5050',
           c_5051            LIKE sy-dynnr   VALUE '5051',
           c_6000            LIKE sy-dynnr   VALUE '6000',
           c_6002            LIKE sy-dynnr   VALUE '6002',
           c_6003            LIKE sy-dynnr   VALUE '6003',
           c_7000            LIKE sy-dynnr   VALUE '7000',
           c_8000            LIKE sy-dynnr   VALUE '8000',
           c_8001            LIKE sy-dynnr   VALUE '8001',
           c_8002            LIKE sy-dynnr   VALUE '8002',
           c_8020            LIKE sy-dynnr   VALUE '8020',
           c_8021            LIKE sy-dynnr   VALUE '8021',
           c_8022            LIKE sy-dynnr   VALUE '8022',
           c_8050            LIKE sy-dynnr   VALUE '8050',
           c_8051            LIKE sy-dynnr   VALUE '8051',
           c_8052            LIKE sy-dynnr   VALUE '8052',
           c_9999            LIKE sy-dynnr   VALUE '9999'.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA: vg_dynnr_000          LIKE sy-dynnr,
      vg_dynnr_n1           LIKE sy-dynnr,
      vg_dynnr_ant          LIKE sy-dynnr,
      vg_dynnr_tab          LIKE sy-dynnr,
      vg_dynnr_cad          LIKE sy-dynnr,
      vg_dynnr_flt          LIKE sy-dynnr,
      vg_dynnr_res          LIKE sy-dynnr,
      vg_4002_6002          LIKE sy-dynnr,
      vg_2004_9999          LIKE sy-dynnr,
      vg_consul_memo        TYPE c LENGTH 1,
      vg_alterou_memorando  TYPE c LENGTH 1,
      vg_alterou_notas      TYPE c LENGTH 1,
      vg_tem_protocolo      TYPE c LENGTH 1,
      vg_primeiro_visual    TYPE c LENGTH 1,
      vg_primeiro_visual_ed TYPE c LENGTH 1,
      sequ                  TYPE i,
      sequ_memorando        TYPE i,
      wa_fcode              TYPE sy-ucomm,
      ok_code               LIKE sy-ucomm,
      g_tab_nf_livres_lines LIKE sy-loopc,
      g_tab_nf_vincu_lines  LIKE sy-loopc,
      vg_quantidade         TYPE j_1bnetqty,
      vg_quantcompe         TYPE j_1bnetqty,
      vg_quantacomp         TYPE j_1bnetqty,
      vg_saldo              TYPE j_1bnetqty,
      vg_vinculado          TYPE j_1bnetqty,
      vg_qtd_vincu          TYPE j_1bnetqty,
      vg_total_memorandos   TYPE j_1bnetqty,
      vg_nome_remetente     TYPE name1_gp,
      vg_nome_representante TYPE name1_gp,
      vg_nome_transportador TYPE name1_gp,
      vg_nome_nr_re         TYPE zid_transporte,
      vg_pais_destino       TYPE land1,
      nr_re_aux             TYPE c LENGTH 18,
      vg_id_trans           TYPE zid_transporte,
      vg_ds_trans           TYPE zds_nome_trans,
      vg_nome_branch        TYPE name1,
      vg_nome_pais_origem   TYPE landx,
      vg_nome_pais_destino  TYPE landx,
      vg_nome_uf_origem     TYPE bezei20,
      vg_observacao_form    TYPE c LENGTH 400,
      vg_novo_lanc          TYPE c LENGTH 1,
      "Variáveis para protocolo
      vg_consul_prot        TYPE c LENGTH 1,
      vg_alterou_protocolo  TYPE c LENGTH 1,
      vg_nm_emissor         TYPE name1_gp,
      vg_nm_empresa_em      TYPE butxt,
      vg_nm_filial_em       TYPE name1,
      vg_nm_destino         TYPE name1_gp,
      vg_nm_empresa_de      TYPE butxt,
      vg_nm_filial_de       TYPE name1,
      campo_1               TYPE char50,
      campo_2               TYPE char50,
      campo_3               TYPE char50,
      campo_4               TYPE char50,
      l_resp                TYPE c,
      p_direcao_1           TYPE n,
      todos                 TYPE c,
      radio1(1)             TYPE c,
      radio2(1)             TYPE c,
      terceiro              TYPE c LENGTH 1,
      inicia_selec          TYPE c,
      layout_new            TYPE c.

*----------------------------------------------------------------------*
* Work Áreas
*----------------------------------------------------------------------*
DATA: wa_memorando          TYPE zmemo_memorando,
      wa_notas_exp          TYPE zmemo_nota_exp,
      wa_conhec             TYPE zmemo_memorando_conhec,
*      WA_CONHEC             TYPE ZNON_CONHEC,
      wa_protocolos         TYPE zmemo_protocolo,
      wa_nf_livre           TYPE zmemo_notas_livres,
      wa_nf_vincu           TYPE zmemo_notas_vinculadas,
      wa_nf_vincu_s         TYPE zmemo_notas_vinculadas_s,
      wa_nf_aux             TYPE zmemo_notas_vinculadas,
      wa_memorando_tela     TYPE zmemo_memorando_tela,
      wa_memorando_tela_aux TYPE zmemo_memorando_tela,
      wa_memorando_tela_alt TYPE zmemo_memorando_tela,
      wa_prot_memo          TYPE zmemo_protocolo_memo,
      wa_zdoc_memorando     TYPE zdoc_memorando,
      wa_zdoc_memo_nota     TYPE zdoc_memo_nota,
      wa_exportacoes        TYPE ty_zexport_acomp, "zexport_acomp,
      wa_export_acomp       TYPE ty_zmemo_export_acomp, "zmemo_export_acomp,
      wa_notas              TYPE ty_zexport_notas,  "LIKE zexport_notas,
      wa_cont               TYPE REF TO cl_gui_custom_container , " Objeto Container
      wa_alv                TYPE REF TO cl_gui_alv_grid         , " Objeto ALV
      wa_layout             TYPE lvc_s_layo                     . " Layout da Lista / Fim do DATA


*----------------------------------------------------------------------*
* Tabelas Internas
*----------------------------------------------------------------------*
DATA: it_memorandos         TYPE TABLE OF zmemo_memorando INITIAL SIZE 0 WITH HEADER LINE,
      it_memorandos_alv     TYPE TABLE OF zmemo_memorando,
      it_notas_exp          TYPE TABLE OF zmemo_nota_exp  INITIAL SIZE 0 WITH HEADER LINE,
      it_memorandos_aux     TYPE TABLE OF zmemo_memorando INITIAL SIZE 0 WITH HEADER LINE,
      it_conhec             TYPE TABLE OF zmemo_memorando_conhec INITIAL SIZE 0 WITH HEADER LINE,
*      IT_CONHEC             TYPE TABLE OF ZNON_CONHEC INITIAL SIZE 0 WITH HEADER LINE,
      it_nf_livre           TYPE TABLE OF zmemo_notas_livres WITH HEADER LINE,
      it_nf_vincu           TYPE TABLE OF zmemo_notas_vinculadas INITIAL SIZE 0 WITH HEADER LINE,
      it_nf_vincu_s         TYPE TABLE OF zmemo_notas_vinculadas_s INITIAL SIZE 0 WITH HEADER LINE,
      it_nf_desv            TYPE TABLE OF zmemo_notas_vinculadas INITIAL SIZE 0 WITH HEADER LINE,
*      it_exportacoes        TYPE TABLE OF zexport_acomp INITIAL SIZE 0 WITH HEADER LINE,
*      it_export_acomp       TYPE TABLE OF zmemo_export_acomp INITIAL SIZE 0 WITH HEADER LINE,

      it_exportacoes        TYPE TABLE OF ty_zexport_acomp INITIAL SIZE 0 WITH HEADER LINE,
      it_export_acomp       TYPE TABLE OF ty_zmemo_export_acomp INITIAL SIZE 0 WITH HEADER LINE,

      it_fcode              LIKE TABLE OF wa_fcode,
      it_notas              TYPE TABLE OF ty_zexport_notas, "LIKE TABLE OF zexport_notas,
      it_notas2             TYPE TABLE OF ty_zexport_notas, "LIKE TABLE OF zexport_notas,
      it_notas3             TYPE TABLE OF ty_zexport_notas, "LIKE TABLE OF zexport_notas,
      it_zdoc_memorando     TYPE TABLE OF zdoc_memorando,
      it_zdoc_memo_nota     TYPE TABLE OF zdoc_memo_nota,
      it_memorando_tela     TYPE TABLE OF zmemo_memorando_tela INITIAL SIZE 0 WITH HEADER LINE,
      it_memorando_tela_aux TYPE TABLE OF zmemo_memorando_tela INITIAL SIZE 0 WITH HEADER LINE,
      it_memorando_dele     TYPE TABLE OF zmemo_memorando_tela INITIAL SIZE 0 WITH HEADER LINE,
      it_protocolos         TYPE TABLE OF zmemo_protocolo INITIAL SIZE 0 WITH HEADER LINE,
      it_prot_memo          TYPE TABLE OF zmemo_protocolo_memo INITIAL SIZE 0 WITH HEADER LINE,
      it_fcat               TYPE TABLE OF lvc_s_fcat. " Tabela Estrutura colunas relatorio.

*----------------------------------------------------------------------*
* Controles de Tela
*----------------------------------------------------------------------*
*&SPWIZARD: DECLARATION OF TABLECONTROL 'TAB_MEMORANDOS' ITSELF
CONTROLS: "tab_memorandos  TYPE TABLEVIEW USING SCREEN 1002,
  tab_memo_nf_exp TYPE TABLEVIEW USING SCREEN 1004,
  tab_conhec      TYPE TABLEVIEW USING SCREEN 2006,
  tab_nf_livres   TYPE TABLEVIEW USING SCREEN 4002,
  tab_nf_livres_s TYPE TABLEVIEW USING SCREEN 6002,
  tab_nf_vincu    TYPE TABLEVIEW USING SCREEN 4002,
  tab_nf_vincu_s  TYPE TABLEVIEW USING SCREEN 6002,
  tab_memo_todos  TYPE TABLEVIEW USING SCREEN 2002,
  tab_export_acp  TYPE TABLEVIEW USING SCREEN 5002,
  tab_cabe_protoc TYPE TABLEVIEW USING SCREEN 8002,
  tab_vinc_memo   TYPE TABLEVIEW USING SCREEN 8052,
  tab_memo_vinc   TYPE TABLEVIEW USING SCREEN 8052,
  tabmemo         TYPE TABSTRIP,
  tabnota         TYPE TABSTRIP,
  ctlfichas_ftr   TYPE TABSTRIP,
  ctlfichas       TYPE TABSTRIP.

DATA: ctl_alv_memo           TYPE REF TO cl_gui_alv_grid,
      ctl_cccontainer        TYPE REF TO cl_gui_custom_container,
      ctl_alv_memo_editar    TYPE REF TO cl_gui_alv_grid,
      ctl_cccontainer_editar TYPE REF TO cl_gui_custom_container.


DATA: it_fieldcatalog        TYPE lvc_t_fcat,
      wa_fieldcatalog        TYPE lvc_s_fcat,
      it_fieldcatalog_editar TYPE lvc_t_fcat,
      wa_fieldcatalog_editar TYPE lvc_s_fcat.

DATA: gs_scroll_col TYPE lvc_s_col,
      gs_scroll_row TYPE lvc_s_roid.

DATA: it_fnl_text TYPE TABLE OF dd07v WITH KEY domvalue_l,
      it_stt_text TYPE TABLE OF dd07v WITH KEY domvalue_l,
      it_drc_text TYPE TABLE OF dd07v WITH KEY domvalue_l,
      wa_dmo_text TYPE dd07v.

DATA:     g_tab_memo_vinc_lines  LIKE sy-loopc.

*data: GS_VARIANT_C TYPE DISVARIANT.

*----------------------------------------------------------------------*
* Selections Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN NESTING LEVEL 3.
SELECTION-SCREEN BEGIN OF BLOCK zb01 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: t_remete FOR  zdoc_memorando-remetente         NO-EXTENSION NO INTERVALS,
                t_repres FOR  zdoc_memorando-representante     NO-EXTENSION NO INTERVALS,
                t_dt_emi FOR  zdoc_memo_nf_exp-dt_emissao_nota.
SELECTION-SCREEN END OF BLOCK zb01.
SELECTION-SCREEN BEGIN OF BLOCK zb02 WITH FRAME TITLE text-s02.
SELECT-OPTIONS: t_serie  FOR  zdoc_memo_nf_exp-serie       NO-EXTENSION NO INTERVALS,
                t_nrnota FOR  zdoc_memo_nf_exp-numero_nota NO-EXTENSION NO INTERVALS,
                t_emisso FOR  zdoc_memo_nf_exp-emissor     NO-EXTENSION NO INTERVALS.
PARAMETERS :    p_safra  TYPE charg_d.
SELECTION-SCREEN END OF BLOCK zb02.
SELECTION-SCREEN END OF SCREEN 1001.

SELECTION-SCREEN BEGIN OF SCREEN 1003 AS SUBSCREEN NESTING LEVEL 3.
SELECTION-SCREEN BEGIN OF BLOCK zb05 WITH FRAME TITLE text-s01.
SELECT-OPTIONS: t_memor FOR zdoc_memorando-numero_memo NO-EXTENSION NO INTERVALS,
                t_statu FOR zdoc_memorando-status NO-EXTENSION NO INTERVALS.
PARAMETERS: t_cancs AS CHECKBOX,
            t_propr AS CHECKBOX,
            t_terce AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK zb05.
SELECTION-SCREEN END OF SCREEN 1003.

SELECTION-SCREEN BEGIN OF SCREEN 5001 AS SUBSCREEN NESTING LEVEL 3.
SELECTION-SCREEN BEGIN OF BLOCK zb03 WITH FRAME TITLE text-s03.
PARAMETERS: t_empres TYPE bukrs OBLIGATORY,
            t_centro TYPE werks_d,
            t_produt TYPE matnr.
SELECT-OPTIONS: t_period FOR  j_1bnfdoc-docdat OBLIGATORY,
                t_export FOR  kna1-kunnr.
SELECTION-SCREEN END OF BLOCK zb03.
SELECTION-SCREEN END OF SCREEN 5001.

SELECTION-SCREEN BEGIN OF SCREEN 5051 AS SUBSCREEN NESTING LEVEL 3.
SELECTION-SCREEN BEGIN OF BLOCK zb04 WITH FRAME TITLE text-s04.
PARAMETERS: t_empree TYPE bukrs OBLIGATORY,
            t_centre TYPE werks_d,
            t_produe TYPE matnr.
SELECT-OPTIONS: t_perioe FOR  j_1bnfdoc-docdat OBLIGATORY,
                t_remetp FOR  lfa1-lifnr.
SELECTION-SCREEN END OF BLOCK zb04.
SELECTION-SCREEN END OF SCREEN 5051.

SELECTION-SCREEN BEGIN OF SCREEN 8001 AS SUBSCREEN NESTING LEVEL 3.
SELECTION-SCREEN BEGIN OF BLOCK zb08 WITH FRAME TITLE text-s08.
SELECT-OPTIONS: t_emissp FOR lfa1-lifnr NO-EXTENSION NO INTERVALS,
                t_destin FOR lfa1-lifnr NO-EXTENSION NO INTERVALS,
                t_emisnf FOR lfa1-lifnr NO-EXTENSION NO INTERVALS.
PARAMETERS    : t_memop  TYPE zdoc_memorando-nr_memorando.
SELECT-OPTIONS: t_recibo FOR j_1bnfdoc-docdat.
SELECTION-SCREEN END OF BLOCK zb08.
SELECTION-SCREEN END OF SCREEN 8001.

SELECTION-SCREEN BEGIN OF SCREEN 9000 AS SUBSCREEN NESTING LEVEL 1.

SELECTION-SCREEN BEGIN OF BLOCK memotipo WITH FRAME TITLE text-n01.
PARAMETERS: n_propr TYPE c RADIOBUTTON GROUP gtp DEFAULT 'X' USER-COMMAND ok_dem1,
            n_terce TYPE c RADIOBUTTON GROUP gtp.
SELECTION-SCREEN END OF BLOCK memotipo.

SELECTION-SCREEN BEGIN OF BLOCK memocad WITH FRAME TITLE text-n02.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: n_remetx TYPE c LENGTH 28 DEFAULT 'Fornecedor (Remetente)'.
SELECTION-SCREEN POSITION 28.
SELECT-OPTIONS: n_remete FOR  zdoc_memorando-remetente  NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: n_reprex TYPE c LENGTH 28 DEFAULT 'Local Negócio (Representante)'.
SELECTION-SCREEN POSITION 28.
SELECT-OPTIONS: n_repres FOR zdoc_memorando-representante NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END OF LINE.

SELECT-OPTIONS: n_dt_emi FOR  zdoc_memo_nf_exp-dt_emissao_nota,
                n_memor  FOR  zdoc_memorando-numero_memo       NO-EXTENSION NO INTERVALS,
                n_statu  FOR  zdoc_memorando-status            NO-EXTENSION NO INTERVALS.
PARAMETERS:     n_cancs AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK memocad.

SELECTION-SCREEN BEGIN OF BLOCK memonot WITH FRAME TITLE text-n03.
SELECT-OPTIONS: n_serie  FOR  zdoc_memo_nf_exp-serie       NO-EXTENSION NO INTERVALS,
                n_nrnota FOR  zdoc_memo_nf_exp-numero_nota NO-EXTENSION NO INTERVALS,
                n_emisso FOR  zdoc_memo_nf_exp-emissor     NO-EXTENSION NO INTERVALS.
PARAMETERS :    n_safra  TYPE charg_d.
SELECTION-SCREEN END OF BLOCK memonot.

SELECTION-SCREEN END OF SCREEN 9000.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP gtp.
  CASE n_propr.
    WHEN abap_true.
      n_remetx = 'Fornecedor (Remetente)'.
      n_reprex = 'Local Negócio (Representante)' .
    WHEN abap_false.
      n_remetx = 'Local Negócio (Remetente)'.
      n_reprex = 'Cliente (Representante)'.
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF ( screen-name EQ 'N_REMETX' ) OR ( screen-name EQ 'N_REPREX' ).
      screen-input      = 0.
      screen-display_3d = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
*        Text
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    "PERFORM HANDLE_HOTSPOT_CLICK
    " USING ES_ROW_NO-ROW_ID
    "  E_COLUMN_ID-FIELDNAME.
  ENDMETHOD.                    "handle_hotspot_click
  METHOD on_double_click.
    "READ TABLE IT_ZNOM_TRANSPORTE INDEX E_ROW INTO WA_ZNOM_TRANSPORTE.
    "PERFORM TROCA_ABA_02 USING C_X.
    "LEAVE TO SCREEN 0001.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

ENDCLASS.               "LCL_EVENT_HANDLER
