*&---------------------------------------------------------------------*
*&  Include           ZLESR0102_TOP
*&---------------------------------------------------------------------*

REPORT zlesr0102.

*----------------------------------------------------------------------*
* ALV CONTROL.
*----------------------------------------------------------------------*
INCLUDE <cl_alv_control>.

*----------------------------------------------------------------------*
* TYPE POOLS.
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*

TABLES: vbak, vbkd, ekko, zsdt0001, lfa1, vbpa, somlreci1.

*----------------------------------------------------------------------*
* TYPES
*----------------------------------------------------------------------*

DATA:  BEGIN OF it_msg OCCURS 0.
         INCLUDE STRUCTURE bdcmsgcoll.
DATA:  END OF it_msg.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_vbpa_cr, "Ponto de coleta  REMESSA
         vbeln TYPE vbpa-vbeln,
         lifnr TYPE vbpa-lifnr,
       END OF ty_vbpa_cr,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         spart TYPE mara-spart,
       END OF ty_mara,

       BEGIN OF ty_vbpa_co, "Ponto de coleta  ORDEM
         vbeln TYPE vbpa-vbeln,
         lifnr TYPE vbpa-lifnr,
       END OF ty_vbpa_co,

       BEGIN OF ty_ekpa_pr, "Ponto de coleta  Pedido
         ebeln TYPE ekpa-ebeln,
         lifn2 TYPE ekpa-lifn2,
       END OF ty_ekpa_pr,

       BEGIN OF ty_vbap, "Itinerário  ORDE M
         vbeln TYPE vbap-vbeln,
         route TYPE vbap-route,
         matnr TYPE vbap-matnr,
         posnr TYPE vbap-posnr,
       END OF ty_vbap,

       BEGIN OF ty_ekpv, "Itinerário  PEDIDO
         ebeln TYPE ekpv-ebeln,
         route TYPE ekpv-route,
       END OF ty_ekpv,

       BEGIN OF ty_vbak,
         vbeln        TYPE vbak-vbeln,
         auart        TYPE vbak-auart,
         kunnr        TYPE vbak-kunnr,
         spart        TYPE vbak-spart,
         vkbur        TYPE vbak-vkbur,
         tp_movimento TYPE zsdt0001-tp_movimento,
       END OF ty_vbak,

       BEGIN OF ty_likp,
         vbeln TYPE likp-vbeln,
         route TYPE likp-route,
         inco1 TYPE likp-inco1,
       END OF ty_likp,

       BEGIN OF ty_lips,
         vbeln TYPE lips-vbeln,
         matnr TYPE lips-matnr,
       END OF ty_lips,

       BEGIN OF ty_tvakt,
         auart TYPE tvakt-auart,
         bezei TYPE tvakt-bezei,
       END OF ty_tvakt,

       BEGIN OF ty_tvtk,
         shtyp TYPE tvtk-shtyp,
         laufk TYPE tvtk-laufk,
         vsart TYPE tvtk-vsart,
       END OF ty_tvtk,

       BEGIN OF ty_t161t,
         bsart TYPE t161t-bsart,
         batxt TYPE t161t-batxt,
       END OF ty_t161t,

       BEGIN OF ty_vbkd,
         vbeln TYPE vbkd-vbeln,
         inco1 TYPE vbkd-inco1,
       END OF ty_vbkd,

       BEGIN OF ty_ekko,
         ebeln        TYPE ekko-ebeln,
         bsart        TYPE ekko-bsart,
         reswk        TYPE ekko-reswk,
         lifnr        TYPE ekko-lifnr, "US #66690 - WPP
         tp_movimento TYPE zsdt0001-tp_movimento,
       END OF ty_ekko,

       BEGIN OF ty_ekpo,
         ebeln TYPE ekpo-ebeln,
         ebelp TYPE ekpo-ebelp,
         werks TYPE ekpo-werks,
         inco1 TYPE ekpo-inco1,
         lifnr TYPE lfa1-lifnr,
       END OF ty_ekpo,

       BEGIN OF ty_vbpa,
         vbeln TYPE vbpa-vbeln,
         parvw TYPE vbpa-parvw,
         lifnr TYPE vbpa-lifnr,
         kunnr TYPE vbpa-kunnr,
         dlgrp TYPE lfa1-dlgrp,
       END OF ty_vbpa,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
         lzone TYPE kna1-lzone,
       END OF ty_kna1,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
         dlgrp TYPE lfa1-dlgrp,
         lzone TYPE lfa1-lzone,
         regio TYPE lfa1-regio,
         stcd1 TYPE lfa1-stcd1,
       END OF ty_lfa1,

       BEGIN OF ty_t001w,
         werks TYPE t001w-werks,
         name1 TYPE t001w-name1,
       END OF ty_t001w,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF ty_makt,


       BEGIN OF ty_trans,
         lifnr TYPE lfa1-lifnr , " Agente Frete
         name1 TYPE lfa1-name1 , " Agente Frete
         cnpj  TYPE lfa1-stcd1,
       END OF ty_trans,

       BEGIN OF ty_veic,
         placa    TYPE zsdt0001-placa_cav,
         tipo     TYPE zlest0002-tp_veiculo,
         cod_prop TYPE zlest0002-proprietario,
         nom_prop TYPE lfa1-name1,
         rntc(10),
         renavam  TYPE zlest0002-cd_renavam,
         cnpj     TYPE lfa1-stcd1,
         cpf      TYPE lfa1-stcd2,
         cidade   TYPE zlest0002-cd_cidade,
         uf       TYPE zlest0002-cd_uf,
       END OF ty_veic,

       BEGIN OF ty_mot,
         motorista TYPE lfa1-lifnr,
         name1     TYPE lfa1-name1,
         cpf       TYPE lfa1-stcd2,
         habil     TYPE lfa1-stcd4,
         uf_habil  TYPE lfb1-zsabe,
         rg        TYPE lfa1-stcd3,
         uf_rg     TYPE lfb1-eikto,
       END OF ty_mot,

       BEGIN OF ty_ag_frete,
         transpor1 TYPE lfa1-lifnr,
         name1     TYPE lfa1-name1,
         cnpj1     TYPE lfa1-stcd1,
         inscr1    TYPE lfa1-stcd3,
         transpor2 TYPE lfa1-lifnr,
         name2     TYPE lfa1-name1,
         cnpj2     TYPE lfa1-stcd1,
         inscr2    TYPE lfa1-stcd3,
       END OF ty_ag_frete,

       BEGIN OF ty_editor,
         line(100),
       END OF ty_editor,


       BEGIN OF ty_zsdt0062,
         vbeln    TYPE zsdt0062-vbeln,
         ebeln    TYPE zsdt0062-ebeln,
         ebelp    TYPE zsdt0062-ebelp,
         matnr    TYPE zsdt0062-matnr,
         status   TYPE zsdt0062-status,
         lgort    TYPE zsdt0062-lgort,
         charg    TYPE zsdt0062-charg,
         qtd_vinc TYPE zsdt0062-qtd_vinc,
       END OF ty_zsdt0062,


       BEGIN OF ty_parc,
         parvw    TYPE zfiwrt0015-parvw,
         parid    TYPE zfiwrt0015-parid,
         nome(80),
         style    TYPE lvc_t_styl,
       END OF ty_parc,

       BEGIN OF ty_par_est_migo,
         mat_doc    TYPE bapi2017_gm_head_02-mat_doc,
         doc_year   TYPE bapi2017_gm_head_02-doc_year,
         pstng_date TYPE bapi2017_gm_head_02-pstng_date,
         ent_sai    TYPE c,
         form_lote  TYPE c,
         remessa    TYPE likp-vbeln,
       END OF ty_par_est_migo.

TYPES: BEGIN OF ty_zsdt0001.
         INCLUDE TYPE zde_les_zsdt0001.
TYPES: END OF ty_zsdt0001.

TYPES: BEGIN OF ty_zsdt0001_aux.
         INCLUDE TYPE zde_les_zsdt0001.
TYPES:   sdabw TYPE a942-sdabw.
TYPES: viagem_id TYPE zlest0185-viagem_id.
TYPES: END OF ty_zsdt0001_aux.

TYPES: BEGIN OF ty_saida.
         INCLUDE TYPE zde_les_saida_zsdt0001.
TYPES: END OF ty_saida.
*** TYPES geração de PDF
TYPES: BEGIN OF ty_fileinfo,
         tipodoc  TYPE zsde_tipodoc,
         filename TYPE string,
         data     TYPE xstring,
         len      TYPE i.
TYPES: END OF ty_fileinfo.

TYPES t_fileinfotab TYPE STANDARD TABLE OF ty_fileinfo WITH KEY filename.

*       BEGIN TYPE OF TY_SAIDA.
*         ICON(4),
*         BUKRS            TYPE ZSDT0001-BUKRS,
*         BRANCH           TYPE ZSDT0001-BRANCH,
*         CH_REFERENCIA    TYPE ZSDT0001-CH_REFERENCIA,
*         DT_MOVIMENTO     TYPE ZSDT0001-DT_MOVIMENTO,
*         NR_ROMANEIO      TYPE ZSDT0001-NR_ROMANEIO,
*         NRO_CG           TYPE ZSDT0001-NRO_CG,
*         PLACA_CAV        TYPE ZSDT0001-PLACA_CAV,
*         REGION           TYPE ZSDT0001-REGION,
*         OPERACAO(25),
*         EBELN            TYPE ZSDT0001-VBELN,
*         "CS2017002682 - 29.11.2017 - Ini
*         EBELP            TYPE EKPO-EBELP,
*         EMATN            TYPE EKPO-MATNR,
*         LGORT_N          TYPE ZSDT0062-LGORT,
*         CHARG_N          TYPE ZSDT0062-CHARG,
*         "CS2017002682 - 29.11.2017 - Fim
*         VBELN            TYPE ZSDT0001-VBELN,
*         PESO_LIQ         TYPE ZSDT0001-PESO_LIQ,
*         PESO_FISCAL      TYPE ZSDT0001-PESO_FISCAL,
*         PESO_DESCARGA    TYPE ZSDT0001-PESO_DESCARGA,
*         PESO_RETIDO      TYPE ZSDT0001-PESO_RETIDO,
*         PERC_RET         TYPE ZSDT0001-PERC_RET,
*         PESO_LIQ_POS_RET TYPE ZSDT0001-PESO_LIQ_POS_RET,
*         NETPR            TYPE ZFIWRT0009-NETPR, "valor unitario da nota
*         NR_SAFRA         TYPE ZSDT0001-NR_SAFRA,
*         LIFNR_C          TYPE LFA1-LIFNR, " Ponto de coleta
*         NAME1_C          TYPE LFA1-NAME1, " Ponto de coleta
*         KUNNR            TYPE KNA1-KUNNR,
*         NAME1            TYPE KNA1-NAME1,
*         INCO1            TYPE VBKD-INCO1,
*         ROUTE            TYPE EKPV-ROUTE, " Itinerário
*         KBETR            TYPE KONP-KBETR, " Vlr frete
*         KONWA            TYPE KONP-KONWA, " Unidade de condição (moeda ou porcentagem)
*         KRECH            TYPE KONP-KRECH, " Regra de cálculo de condição
*         LIFNR            TYPE VBPA-LIFNR , " Agente Frete
*         PONTO_COLETA     TYPE LFA1-LIFNR,
*         LOCAL_ENTREGA    TYPE LFA1-LIFNR,
*         ST_PROC          TYPE ZSDT0001-ST_PROC,
*         SHTYP            TYPE ZSDT0011-SHTYP,
*         CFOP             TYPE ZSDT0121-CFOP,
*         ROMANEIOS        TYPE ZSDT0001_T,
*         ROMANEIOS_AGR    TYPE ZSDT0001_T,
*         DELIVERYS        TYPE TAB_LIKP,
*         CONT_FRE         TYPE I,
*         SEQ_LCTO(10),
*         DANFEZ(10),
*         AVISO(10),
*         REMESSA(45),
**         REMESSA(10),
*         FATURA(10),
*         DANFE(10),
*         TRANSP(15),
*         DOCCUS(10),
*         OVSERV(10),
*         FATSERV(10),
*         DACTE(10),
*         TIPO(1),
*         MATNR            TYPE ZSDT0001-MATNR,
*         MATERIAL(60),
*         ID_ORDEM         TYPE ZDE_ID_ORDEM,
*         LINE_COLOR(4)    TYPE C, "Used to store row color attributes
*         COLOR_CELL       TYPE LVC_T_SCOL,  " Cell color
*         STYLE            TYPE LVC_T_STYL,
*       END OF TY_SAIDA.

*---------------------------------------------------------------------*
*  Inicio Definition Classes
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: catch_hotspot
      FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.

    CLASS-METHODS: on_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm .

    CLASS-METHODS: on_data_changed_finished
      FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified
                et_good_cells.

    CLASS-METHODS: on_f4
      FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                es_row_no
                er_event_data
                et_bad_cells
                e_display.

    CLASS-METHODS: set_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    CLASS-METHODS: handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: BEGIN OF tg_impo OCCURS 0,
        taxtyp   TYPE zfiwrt0010-taxtyp,
        ttypetxt TYPE j_1bttytxt,
        taxgrp   TYPE j_1btaxgrp,
        base     TYPE zfiwrt0010-base,
        rate     TYPE zfiwrt0010-rate,
        taxval   TYPE zfiwrt0010-taxval,
        excbas   TYPE zfiwrt0010-excbas,
        othbas   TYPE zfiwrt0010-othbas,
      END OF tg_impo,

      BEGIN OF tg_itens OCCURS 0,
        mark(1),
        itmnum  TYPE zfiwrt0009-itmnum,
        matnr   TYPE zfiwrt0009-matnr,
        maktx   TYPE makt-maktx,
        cfop    TYPE zfiwrt0006-cfop,
        charg   TYPE zfiwrt0009-charg,
        werks   TYPE t001w-werks,
        lgort   TYPE zfiwrt0009-lgort,
        menge   TYPE zfiwrt0009-menge,
        meins   TYPE zfiwrt0009-meins,
        netpr   TYPE zfiwrt0009-netpr,
        netwr   TYPE zfiwrt0009-netwr,
        anln1   TYPE zfiwrt0009-anln1,
        anln2   TYPE zfiwrt0009-anln2,
        steuc   TYPE marc-steuc,
      END OF tg_itens,

      BEGIN OF tg_contab OCCURS 0,
        bschl   TYPE zfiwrt0011-bschl,
        hkont   TYPE zfiwrt0011-hkont,
        txt50   TYPE skat-txt50,
        dmbtr   TYPE zfiwrt0011-dmbtr,
        taxtyp  TYPE zfiwrt0011-taxtyp,
        estorno TYPE zfiwrt0011-estorno,
        newbw   TYPE zfiwrt0011-newbw,
        zlsch   TYPE zfiwrt0011-zlsch,
        zfbdt   TYPE zfiwrt0011-zfbdt,
        kostl   TYPE zfiwrt0011-kostl,
        umskz   TYPE zfiwrt0011-umskz,
        style2  TYPE lvc_t_styl,
      END OF tg_contab,

      BEGIN OF tg_movest OCCURS 0,
        bwart   TYPE zfiwrt0004-bwart,
        tcode   TYPE zfiwrt0004-tcode,
        mwskz1  TYPE zfiwrt0004-mwskz1,
        estorno TYPE zfiwrt0004-estorno,
      END OF tg_movest,

      BEGIN OF tg_mensagems OCCURS 0,
        seqnum  TYPE zfiwrt0005-seqnum,
        linnum  TYPE zfiwrt0005-linnum,
        message TYPE zfiwrt0005-message,
      END OF tg_mensagems,


      BEGIN OF tg_impo_comp OCCURS 0,
        itmnum TYPE zfiwrt0010-itmnum.
        INCLUDE STRUCTURE tg_impo.
DATA: END OF tg_impo_comp.

DATA: BEGIN OF tg_log_erro OCCURS 0,
        msgtyp TYPE zlest0100-msgtyp,
        msgid	 TYPE zlest0100-msgid,
        msgnr  TYPE zlest0100-msgnr,
        msgv1  TYPE zlest0100-msgv1,
      END OF tg_log_erro,

      BEGIN OF tg_zsdt0001_item OCCURS 0.
        INCLUDE STRUCTURE zsdt0001_item.
DATA  END OF tg_zsdt0001_item.

DATA: it_a900           TYPE TABLE OF a900,
      it_a910           TYPE TABLE OF a910,
      it_a911           TYPE TABLE OF a911,
      it_a915           TYPE TABLE OF a915,
      it_a918           TYPE TABLE OF a918,
      it_a919           TYPE TABLE OF a919,
      it_a942           TYPE TABLE OF a942,
      it_konp           TYPE TABLE OF konp,
      it_zsdt0001       TYPE TABLE OF ty_zsdt0001,
      it_zsdt0001_aux   TYPE TABLE OF ty_zsdt0001_aux,
      wa_zsdt0001_aux   TYPE ty_zsdt0001_aux,
      it_zsdt0001_fre   TYPE TABLE OF ty_zsdt0001,
      wa_zsdt0001_fre   TYPE ty_zsdt0001,
      it_zsdt0011_o     TYPE TABLE OF zsdt0011,
      it_zsdt0011_p     TYPE TABLE OF zsdt0011,
      it_zlest0185      TYPE TABLE OF zlest0185,
      wa_zlest0185      TYPE zlest0185,
      it_zlest0132      TYPE TABLE OF zlest0132,
      it_zsdt0062       TYPE TABLE OF ty_zsdt0062,
      it_likp           TYPE TABLE OF ty_likp,
      it_lips           TYPE TABLE OF ty_lips,
      git_vttk          TYPE TABLE OF vttk,
      it_mara           TYPE TABLE OF ty_mara,
      it_vbak           TYPE TABLE OF ty_vbak,
      it_tvakt          TYPE TABLE OF ty_tvakt,
      it_t161t          TYPE TABLE OF ty_t161t,
      it_vbkd           TYPE TABLE OF ty_vbkd,
      it_tvtk           TYPE TABLE OF ty_tvtk,
      it_ekko           TYPE TABLE OF ty_ekko,
      it_ekpo           TYPE TABLE OF ty_ekpo,
      it_vbpa           TYPE TABLE OF ty_vbpa,
      it_vbpa_2         TYPE TABLE OF ty_vbpa,
      it_kna1           TYPE TABLE OF ty_kna1,
      it_lfa1           TYPE TABLE OF ty_lfa1,
      it_t001w          TYPE TABLE OF ty_t001w,
      it_makt           TYPE TABLE OF ty_makt,
      it_vbpa_cr        TYPE TABLE OF ty_vbpa_cr, "Ponto de coleta  REMESSA
      it_vbpa_co        TYPE TABLE OF ty_vbpa_co, "Ponto de coleta  ORDEM
      it_ekpa_pr        TYPE TABLE OF ty_ekpa_pr, "Ponto de coleta  Pedido
      it_vbap           TYPE TABLE OF ty_vbap, "Itinerário  ORDE M
      it_ekpv           TYPE TABLE OF ty_ekpv, "Itinerário  PEDIDO
      it_veic           TYPE TABLE OF ty_veic,
      ti_zlest0100      TYPE TABLE OF zlest0100  WITH HEADER LINE,
      it_zlest0100      TYPE TABLE OF zlest0100  WITH HEADER LINE,
      t_auart           TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      t_fatura_agrupada TYPE TABLE OF zsdt0121,
      w_fatura_agrupada TYPE zsdt0121,
      t_usermd          TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
      it_saida          TYPE TABLE OF ty_saida WITH HEADER LINE,
      tl_texto          TYPE catsxt_longtext_itab,
      wl_text_header    TYPE sytitle,
      wl_texto          TYPE LINE OF catsxt_longtext_itab,
      it_color          TYPE TABLE OF lvc_s_scol,
      style             TYPE lvc_t_styl WITH HEADER LINE,
      tg_impo_aux       LIKE TABLE OF tg_impo WITH HEADER LINE,
      tg_mensagems_aux  LIKE TABLE OF tg_mensagems WITH HEADER LINE,
      tl_0002           TYPE TABLE OF zfiwrt0002 WITH HEADER LINE,
      tl_0003           TYPE TABLE OF zfiwrt0003 WITH HEADER LINE,
      tl_0004           TYPE TABLE OF zfiwrt0004 WITH HEADER LINE,
      tl_0005           TYPE TABLE OF zfiwrt0005 WITH HEADER LINE,
      tl_0006           TYPE TABLE OF zfiwrt0006 WITH HEADER LINE,
      tl_0007           TYPE TABLE OF zfiwrt0007 WITH HEADER LINE,
      tl_1baj           TYPE TABLE OF j_1baj     WITH HEADER LINE,
      tl_1bajt          TYPE TABLE OF j_1bajt    WITH HEADER LINE,
      tl_tbsl           TYPE TABLE OF tbsl       WITH HEADER LINE,
      tl_skat           TYPE TABLE OF skat       WITH HEADER LINE,
* ---> S4 Migration - 17/07/2023 - CA
*      tl_cskb           TYPE TABLE OF cskb       WITH HEADER LINE,
* <--- S4 Migration - 17/07/2023 - CA
      tl_user           TYPE TABLE OF user_addr  WITH HEADER LINE,
      tg_parc           TYPE TABLE OF ty_parc    WITH HEADER LINE.

*** Geração de PDF.
DATA: it_pdffiles     TYPE t_fileinfotab,
      pdf_merger      TYPE REF TO cl_rspo_pdf_merge,
      ex              TYPE REF TO cx_rspo_pdf_merge,
      lv_ex_txt       TYPE string,
      merged_document TYPE xstring,
      pdf_result      TYPE xstring,
      docindex        TYPE i VALUE 0,
      errordoc        TYPE xstring,
      rc              TYPE i VALUE 0,
      lnum            TYPE i VALUE 0,
      l_add01(4)      TYPE n,
      p_sel           TYPE i VALUE 1.
*** Data Sending email
*      it_packing_list TYPE TABLE OF sopcklsti1,
*      it_header       TYPE TABLE OF solisti1,
*      it_contents_txt TYPE TABLE OF solisti1,
*      it_contents_bin TYPE TABLE OF solisti1,
*      it_receivers    TYPE TABLE OF somlreci1.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_a900          TYPE a900,
      wa_a910          TYPE a910,
      wa_a911          TYPE a911,
      wa_a915          TYPE a915,
      wa_a918          TYPE a918,
      wa_a919          TYPE a919,
      wa_a942          TYPE a942,
      wa_konp          TYPE konp,
      w_vbap           TYPE vbap,
      wa_zsdt0001      TYPE ty_zsdt0001,
      wa_zsdt0011      TYPE zsdt0011,
      wa_zlest0132     TYPE zlest0132,
      wa_zsdt0062      TYPE ty_zsdt0062,
      wa_zfiwrt0008    TYPE zfiwrt0008,
      wa_mara          TYPE ty_mara,
      wa_vbak          TYPE ty_vbak,
      wa_tvakt         TYPE ty_tvakt,
      wa_t161t         TYPE ty_t161t,
      wa_vbkd          TYPE ty_vbkd,
      wa_tvtk          TYPE ty_tvtk,
      wa_ekko          TYPE ty_ekko,
      wa_ekpo          TYPE ty_ekpo,
      wa_vbpa          TYPE ty_vbpa,
      wa_vbpa_2        TYPE ty_vbpa,
      wa_zdco_produtor TYPE zdco_produtor,
      wa_kna1          TYPE ty_kna1,
      wa_lfa1          TYPE ty_lfa1,
      w_lfa1           TYPE lfa1,
      wa_t001w         TYPE ty_t001w,
      wa_makt          TYPE ty_makt,
      wa_zsdt0151      TYPE zsdt0151,
      wa_t005s         TYPE t005s,
      wa_vbpa_cr       TYPE ty_vbpa_co, "Ponto de coleta  REMESSA
      wa_vbpa_co       TYPE ty_vbpa_co, "Ponto de coleta  ORDEM
      wa_ekpa_pr       TYPE ty_ekpa_pr, "Ponto de coleta  Pedido
      wa_vbap          TYPE ty_vbap, "Itinerário  ORDE M
      wa_ekpv          TYPE ty_ekpv, "Itinerário  PEDIDO
      wa_trans         TYPE ty_trans, "Transportadora
      wa_mot           TYPE ty_mot , "Motorista
      wa_ag_frete      TYPE ty_ag_frete , "Agente de Frete
      wa_likp          TYPE likp , "Remessa
      wa_zlest0100     TYPE zlest0100,
      wa_zcte_ciot     TYPE zcte_ciot,
      wa_saida         TYPE ty_saida,
      wa_saida2        TYPE ty_saida,
      wa_color         TYPE lvc_s_scol,
      wa_msg           TYPE bdcmsgcoll,
      wa_style         TYPE lvc_s_styl,
      wl_0001          TYPE zfiwrt0001,
      wl_0019          TYPE zfiwrt0019,
      wl_kna1          TYPE kna1,
      wl_lfa1          TYPE lfa1,
      wl_t001w         TYPE t001w,
      wl_t001          TYPE t001,
      wl_1bbranch      TYPE j_1bbranch,
      wl_1bad          TYPE j_1bad,
      wl_1badt         TYPE j_1badt,
      wl_1baa          TYPE j_1baa,
      wl_indcoper      TYPE zfiwrt0006-indcoper,
      wg_shipfrom      TYPE lfa1-regio,
      wg_shipto        TYPE lfa1-regio,
      p_operacao       TYPE zfiwrt0001-operacao,
      p_lgort_d        TYPE zlest0117-lgort_dest,
      p_lgort_o        TYPE zlest0117-lgort_orig,
      p_parvw          TYPE zfiwrt0008-parvw,
      p_parid          TYPE zfiwrt0008-parid,
      wl_par_est_migo  TYPE ty_par_est_migo,
*** Workare sending email
      wa_contents_txt  TYPE solisti1.



*----------------------------------------------------------------------*
* VARIAVEIS
*----------------------------------------------------------------------*
DATA: l_var            TYPE indx-srtfd,
      wl_ordemt        TYPE thead-tdname,
      vl_delivery      TYPE bapishpdelivnumb-deliv_numb,
      v_docnum         TYPE j_1bnfe_active-docnum,
      vl_vbeln         TYPE vbfa-vbeln,
      vl_fksto         TYPE vbrk-fksto,
      vl_mjahr         TYPE vbfa-mjahr,
      vl_refkey        TYPE j_1bnflin-refkey,
      vl_fknum         TYPE zsdt0001-fknum,
      vl_ov_frete      TYPE zsdt0001-ov_frete,
      v_tknum          TYPE vttk-tknum,
      vl_fatura_frete  TYPE zsdt0001-fatura_frete,
      vl_bukrs         TYPE j_1bnfdoc-bukrs,
      vl_docnum        TYPE j_1bnflin-docnum,
      vl_seq_lcto      TYPE zfiwrt0008-seq_lcto,
      vl_message       TYPE char600,
      vl_ponteiro      TYPE zlest0100-cont,
      vcandat          TYPE j_1bnfdoc-candat,
      v_xblnr          TYPE likp-xblnr,
      tabix            TYPE sy-tabix,
      wl_sucess(1),
      wl_erro(1),
      wg_exit(1)       TYPE c,
      wg_save(1)       TYPE c,
      wl_mode(1),
      wg_flag,
      wg_documento(10),
      w_answer(1),
      vg_pdf(1)        TYPE c,
      vg_cockpit_ant   TYPE c LENGTH 2.  "*-CS2021000218-16.11.2022-#90706-JT

*-----------------------------------------------------------------------*
*    Variáveis de Controle
*-----------------------------------------------------------------------*
DATA: vg_cockpit                TYPE c LENGTH 2,

      "Status Precedente -------------------------------*
      vg_st_remessa_before      TYPE zsdt0001-st_proc,
      vg_st_fatura_before       TYPE zsdt0001-st_proc,
      vg_st_danfe_before        TYPE zsdt0001-st_proc,
      vg_st_transp_before       TYPE zsdt0001-st_proc,
      vg_st_custo_before        TYPE zsdt0001-st_proc,
      vg_st_ov_frete_before     TYPE zsdt0001-st_proc,
      vg_st_fatura_frete_before TYPE zsdt0001-st_proc,
      vg_st_dacte_before        TYPE zsdt0001-st_proc,
      vg_st_znfw_before         TYPE zsdt0001-st_proc,
      vg_st_danfe_znfw_before   TYPE zsdt0001-st_proc,
      vg_st_aviso_rec_before    TYPE zsdt0001-st_proc,
      vg_st_finalizado_before   TYPE zsdt0001-st_proc,

      "Status Atuais -----------------------------------*
      vg_st_remessa             TYPE zsdt0001-st_proc,
      vg_st_fatura              TYPE zsdt0001-st_proc,
      vg_st_danfe               TYPE zsdt0001-st_proc,
      vg_st_transp              TYPE zsdt0001-st_proc,
      vg_st_custo               TYPE zsdt0001-st_proc,
      vg_st_ov_frete            TYPE zsdt0001-st_proc,
      vg_st_fatura_frete        TYPE zsdt0001-st_proc,
      vg_st_dacte               TYPE zsdt0001-st_proc,
      vg_st_znfw                TYPE zsdt0001-st_proc,
      vg_st_danfe_znfw          TYPE zsdt0001-st_proc,
      vg_st_aviso_rec           TYPE zsdt0001-st_proc,
      vg_st_finalizado          TYPE zsdt0001-st_proc,
      vg_st_aguard_doc_carg     TYPE zsdt0001-st_proc.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA: editcontainer      TYPE REF TO cl_gui_custom_container,
      cl_container       TYPE REF TO cl_gui_custom_container,
      cl_container_95    TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id      TYPE REF TO cl_dd_document,
      cl_grid            TYPE REF TO cl_gui_alv_grid,
      container          TYPE REF TO cl_gui_custom_container,
      picture            TYPE REF TO cl_gui_picture,
      grid1              TYPE REF TO cl_gui_alv_grid,
      grid2              TYPE REF TO cl_gui_alv_grid,
      obg_conteiner      TYPE REF TO cl_gui_custom_container,
      obg_conteiner_veic TYPE REF TO cl_gui_custom_container,
      wa_stable          TYPE lvc_s_stbl,
      is_stable          TYPE lvc_s_stbl VALUE 'XX',
      wa_afield          TYPE lvc_s_fcat,
      it_fieldcat        TYPE lvc_t_fcat,
      w_fieldcat         TYPE lvc_s_fcat,
      gt_f4              TYPE lvc_t_f4 WITH HEADER LINE,
      tl_function        TYPE ui_functions,
      wl_function        LIKE tl_function WITH HEADER LINE,
      i_sort             TYPE lvc_t_sort,
      wa_layout          TYPE lvc_s_layo,
      gs_variant_c       TYPE disvariant,
      ok-code            TYPE sy-ucomm,
      wa_estrutura       TYPE ty_estrutura,
      tl_rows            TYPE lvc_t_row,
      sl_rows            TYPE lvc_s_row,
      estrutura          TYPE TABLE OF ty_estrutura,
      url(255)           TYPE c,
      graphic_url(255).

DATA: BEGIN OF graphic_table OCCURS 0,
        line(255) TYPE x,
      END OF graphic_table.

DATA: l_graphic_conv TYPE i,
      l_graphic_offs TYPE i,
      graphic_size   TYPE i,
      l_graphic_xstr TYPE xstring.

DATA: lo_container       TYPE REF TO cl_gui_custom_container,
      lo_pic             TYPE REF TO cl_gui_picture,
      g_custom_cont_desc TYPE REF TO cl_gui_custom_container,
      obg_descbox        TYPE REF TO cl_gui_textedit,
      g_descbox          TYPE scrfname VALUE 'CC_DESC',
      txt_correc         TYPE c LENGTH 1000,
      tg_editor          TYPE TABLE OF ty_editor,
      wl_cont            TYPE sy-tabix,
      wl_cont_aux        TYPE sy-tabix,
      wl_cont_aux2       TYPE sy-tabix,
      wg_editor          TYPE ty_editor.

DATA vinco1       TYPE vbkd-inco1 VALUE IS INITIAL.


*----------------------------------------------------------------------*
* BAPI
*----------------------------------------------------------------------*
DATA: t_success         TYPE STANDARD TABLE OF bapivbrksuccess WITH HEADER LINE,
      w_success         LIKE LINE OF t_success,
      t_billing         TYPE STANDARD TABLE OF bapivbrk WITH HEADER LINE,
      w_billing         LIKE LINE OF t_billing,
      wa_setleaf        TYPE setleaf,
      t_textdatain      TYPE STANDARD TABLE OF bapikomfktx WITH HEADER LINE,
      w_textdatain      LIKE LINE OF t_textdatain,
      x_header          TYPE thead,
      it_lines          TYPE STANDARD TABLE OF tline WITH HEADER LINE,
      it_lines_zfat     TYPE STANDARD TABLE OF tline WITH HEADER LINE,
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
      is_cancelled      TYPE bapivbrkout-cancelled,
      t_return          TYPE STANDARD TABLE OF bapireturn1 WITH HEADER LINE.

*&--------------------------------------------------------------------&*
*& SHDB                                                               &*
*&--------------------------------------------------------------------&*
DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata LIKE LINE OF ti_bdcdata,
      tl_bdc     TYPE TABLE OF bdcdata,
      wl_bdc     TYPE bdcdata,
      opt        TYPE ctu_params.
*      W_NAME1    TYPE LFA1-NAME1.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

DATA: vg_no_grid  TYPE char01.   "*-CS2024000086-26.09.2024-#151423-JT-inicio

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x          TYPE c VALUE 'X'.

*&--------------------------------------------------------------------&*
*& FIELD-SYMBOLS                                                      &*
*&--------------------------------------------------------------------&*
FIELD-SYMBOLS: <wdlv>   TYPE any. "BAPISHPDELIVNUMB-DELIV_NUMB.


*&--------------------------------------------------------------------&*
*& Ranges                                                             &*
*&--------------------------------------------------------------------&*
RANGES: r_tp_movimento FOR zsdt0001-tp_movimento,
        r_matnr        FOR zsdt0001-matnr,
        r_bukrs        FOR zsdt0001-bukrs,
        r_branch       FOR zsdt0001-branch,
        r_dlgrp        FOR lfa1-dlgrp,
        r_vsart        FOR tvtk-vsart,
        r_coleta       FOR lfa1-lifnr.

*&--------------------------------------------------------------------&*
*& Define                                                             &*
*&--------------------------------------------------------------------&*
DEFINE new_line.
  CLEAR  wa_contents_txt.
         wa_contents_txt = &1.
  APPEND wa_contents_txt TO t_con_txt.
END-OF-DEFINITION.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_01 RADIOBUTTON GROUP rad2 USER-COMMAND act DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i01. "Commodities (Formação Lote, Vendas e Trâsferências Expedidas)
  SELECTION-SCREEN END OF LINE.

*"linha branco
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 03(65) TEXT-IBR.
*SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_02 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(79) TEXT-i02. "Commodities (Armazenagem Enviadas - Remessas e Devoluções )
  SELECTION-SCREEN END OF LINE.


  "linha branco
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 03(65) TEXT-ibr.
  SELECTION-SCREEN END OF LINE.


  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_04 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(65) TEXT-i04. "Fertilizantes (Porto Velho)
  SELECTION-SCREEN END OF LINE.

  "linha branco
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 03(65) TEXT-ibr.
  SELECTION-SCREEN END OF LINE.

  "SELECTION-SCREEN BEGIN OF LINE.
  "PARAMETERS r_cp_08 RADIOBUTTON GROUP rad2.
  "SELECTION-SCREEN COMMENT 03(65) text-i10. "Frete
  "SELECTION-SCREEN END OF LINE.

  "linha branco
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 03(65) TEXT-ibr.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_09 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(65) TEXT-i11. "Troca de notas - Commodities (Romaneio de Entrada Completo)
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_10 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(65) TEXT-i16. "Troca de notas - Commodities (Romaneio de Entrada Completo - Transferência)
  SELECTION-SCREEN END OF LINE.

*"linha branco
*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 03(65) TEXT-IBR.
*SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_03 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(65) TEXT-i12. "
  SELECTION-SCREEN END OF LINE.

  "linha branco
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 03(65) TEXT-ibr.
  SELECTION-SCREEN END OF LINE.



  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 01(65) TEXT-i05. "Insumos (Vendas e Trânsferências expedidas )
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_cp_05 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 03(09) TEXT-i07. "Sementes

    PARAMETERS r_cp_06 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 15(10) TEXT-i08. "Defensivos

    PARAMETERS r_cp_07 RADIOBUTTON GROUP rad2.
    SELECTION-SCREEN COMMENT 28(13) TEXT-i09. "Fertilizantes
  SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*SELECTION-SCREEN COMMENT 1(65) TEXT-I06. "Separador
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b3.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_bukrs  TYPE zsdt0001-bukrs, "NO-DISPLAY.  "Empresa
              p_branch TYPE zsdt0001-branch. "Filial

  SELECT-OPTIONS: s_branch FOR zsdt0001-branch. "RJF
*SELECTION-SCREEN COMMENT 4(20) TEXT-SU1 FOR FIELD P_BRANCH.

  PARAMETERS: p_coleta TYPE lfa1-lifnr MODIF ID a.
  SELECTION-SCREEN COMMENT 50(20) w_name1 MODIF ID a.

  SELECT-OPTIONS: s_vbeln FOR vbak-vbeln,                             "Ordem de venda
                  s_ebeln FOR ekko-ebeln,                             "Pedido
                  s_matnr FOR zsdt0001-matnr,                         "Produto
                  s_inco1 FOR vbkd-inco1,                             "Tipo Frete
                  s_lifnr FOR lfa1-lifnr NO INTERVALS ,               "Agente de frete
                  s_data  FOR zsdt0001-dt_movimento DEFAULT sy-datum, "Data de Movimento
                  s_doc   FOR zsdt0001-doc_transp,                    "Documento Transporte
                  s_roman FOR zsdt0001-nr_romaneio.                   "Nr.Romaneio  "*-CS2024000090-28.05.2024-#133805-JT
SELECTION-SCREEN: END OF BLOCK b1.

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
*-CS2021000218-16.11.2022-#90706-JT-inicio
PARAMETERS:     p_init  TYPE char1                 NO-DISPLAY.
SELECT-OPTIONS: p_dtinsu FOR zsdt0001-dt_movimento NO-DISPLAY.
SELECT-OPTIONS: p_dtoutr FOR zsdt0001-dt_movimento NO-DISPLAY.
*-CS2021000218-16.11.2022-#90706-JT-fim
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS r_dt_a RADIOBUTTON GROUP rad1 USER-COMMAND act DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 03(09) TEXT-i13. "

    PARAMETERS r_dt_f RADIOBUTTON GROUP rad1.
    SELECTION-SCREEN COMMENT 15(10) TEXT-i14. "

    " Comentado pelo BUG-30747
    " a Opção R_DT_T usa-se na seleção de dados quando não é selecionado R_DT_A ou R_DT_F
    " adicionando no Where S_CHAVE e P_INTER, esses campos provavelmente são de alguma interface de outro processo.
    PARAMETERS r_dt_t RADIOBUTTON GROUP rad1 .
    "SELECTION-SCREEN COMMENT 28(13) TEXT-I15. "
    " Comentado pelo BUG-30747

  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS: s_chave FOR zsdt0001-ch_referencia NO-DISPLAY.
  PARAMETERS: p_inter TYPE zsdt0001-id_interface NO-DISPLAY.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF SCREEN 0410 AS SUBSCREEN.
  SELECT-OPTIONS: s_email FOR somlreci1-receiver NO INTERVALS.
SELECTION-SCREEN END OF SCREEN 0410.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*

AT SELECTION-SCREEN OUTPUT.

  PERFORM f_set_tp_cockpit.
  SELECT SINGLE name1 INTO w_name1 FROM lfa1 WHERE lifnr = p_coleta.

  "Faturamento RPA - Ini
  DATA(_user_rpa) = abap_false.
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_user_rpa)
   WHERE name EQ 'MAGGI_USER_RPA'
     AND low = @sy-uname.

  IF sy-subrc IS INITIAL.
    _user_rpa = abap_true.
  ENDIF.
  "Faturamento RPA - Fim

  IF p_inter NE zcl_romaneio=>interface_carga_sap  "WPP 23102024 - US-153330 --->>>
     AND s_chave[] IS INITIAL.  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>

*-Equalização RISE x PRD - 19.07.2023 - JT - inicio
*-CS2021000218-16.11.2022-#90706-JT-inicio
    IF p_init = abap_off.
      FREE: p_dtinsu.
      p_dtinsu-sign   = 'I'.
      p_dtinsu-option = 'EQ'.
      p_dtinsu-low    = sy-datum - 15.
      p_dtinsu-high   = sy-datum.
      APPEND p_dtinsu.

      FREE: p_dtoutr.
      p_dtoutr-sign   = 'I'.
      p_dtoutr-option = 'EQ'.
      p_dtoutr-low    = sy-datum.
      APPEND p_dtoutr.

      p_init          = abap_true.
    ENDIF.
*-CS2021000218-16.11.2022-#90706-JT-fim

*-CS2021000218-16.11.2022-#90706-JT-inicio
    IF vg_cockpit <> vg_cockpit_ant AND sy-slset IS INITIAL.
      IF vg_cockpit = '06'.
        s_data[]   = p_dtinsu[].
        p_dtinsu[] = s_data[].
      ELSE.
        s_data[]   = p_dtoutr[].
        p_dtoutr[] = s_data[].
      ENDIF.
    ELSE.
      IF vg_cockpit = '06'.
        p_dtinsu[] = s_data[].
      ELSE.
        p_dtoutr[] = s_data[].
      ENDIF.
    ENDIF.
    vg_cockpit_ant = vg_cockpit.
    sy-slset       = abap_off.
*-CS2021000218-16.11.2022-#90706-JT-fim
*-Equalização RISE x PRD - 19.07.2023 - JT - fim

  ENDIF. "WPP 23102024 - US-153330 --->>>

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
  if r_dt_t is NOT INITIAL and s_chave[] is NOT INITIAL.
    CLEAR: s_data[].
  endif.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<----


  LOOP AT SCREEN.

    CASE vg_cockpit. "Autorização Cockpit
      WHEN '04'.
        IF screen-group1 = 'A'.
          screen-active = 1.
        ENDIF.
      WHEN OTHERS.
        IF screen-group1 = 'A'.
          screen-active = 0.
        ENDIF.

        IF _user_rpa EQ abap_false.
          IF ( p_branch IS NOT INITIAL ) AND
             ( vg_cockpit NE '09' AND vg_cockpit NE '10' ).

            AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
              ID 'WERKS' FIELD  p_branch
              ID 'ACTVT' FIELD '03'.    "Alteração

            CASE sy-subrc.
              WHEN 0.
                "Tem autorização!
              WHEN 4.
                MESSAGE 'Sem autorização para esta filial' TYPE 'I'.
                SET CURSOR FIELD 'P_BRANCH'.
              WHEN 12.
                MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
                SET CURSOR FIELD 'P_BRANCH'.
              WHEN OTHERS.

            ENDCASE.
          ENDIF.
        ENDIF.

    ENDCASE.

    "Faturamento RPA - Ini
    CASE _user_rpa.
      WHEN abap_true.
        IF ( ( screen-name CS 'P_BRANCH' ) AND ( ( sy-tcode = 'ZLES0136' ) OR ( sy-tcode = 'ZLES0137' ) ) ).
          screen-invisible = 1.
          screen-input = 0.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN abap_false.
        IF ( ( screen-name CS 'P_BUKRS' OR screen-name CS 'S_BRANCH' ) AND ( ( sy-tcode = 'ZLES0136' ) OR ( sy-tcode = 'ZLES0137' ) ) ).
          screen-invisible = 1.
          screen-input = 0.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
    "Faturamento RPA - Fim

    "Campo agente frete
    IF ( screen-name CS 'BUKRS' OR screen-name CS 'BRANCH' OR screen-name CS 'VBELN' OR screen-name CS 'EBELN' OR screen-name CS 'MATNR' OR screen-name CS 'INCO1' OR screen-name CS 'DATA' OR screen-name CS 'DOC' ) OR ( NOT screen-name CS 'S_LIFNR' ).
      IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZMM0127'.
        screen-invisible = 0.
        screen-input = 1.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = 0.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
    IF  screen-name CS 'S_LIFNR'.
      IF sy-tcode = 'ZLES0137'.
        screen-invisible = 0.
        screen-input = 1.
        MODIFY SCREEN.
      ELSE.
        screen-invisible = 1.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF ( screen-name CS 'R_DT_T' ) AND ( sy-tcode = 'ZLES0136' ).
      screen-invisible = 1.
      screen-input = 0.
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

*-CS2024000090-28.05.2024-#133805-JT-inicio
    IF screen-name CS 'S_ROMAN'.
      IF ( vg_cockpit = '01' OR
           vg_cockpit = '05' OR  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
           vg_cockpit = '06' OR  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
           vg_cockpit = '07'     "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>>
          ) AND sy-tcode = 'ZLES0136'.
        screen-invisible = 0.
        screen-active    = 1.
        screen-input     = 1.
        MODIFY SCREEN.
      ELSE.
        FREE: s_roman.
        screen-invisible = 1.
        screen-active    = 0.
        screen-input     = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
*-CS2024000090-28.05.2024-#133805-JT-fim

  ENDLOOP.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_set_tp_cockpit.

  IF ( vg_cockpit EQ '09' OR vg_cockpit EQ '10' ) AND sy-tcode NE 'ZMM0127'.
    MESSAGE 'Romaneio Entrada Completo Somente pela Transação ZMM0127' TYPE 'S'.
    EXIT.
  ENDIF.

*-CS2024000090-28.05.2024-#133805-JT-inicio
  IF s_roman[] IS NOT INITIAL.
    IF s_data[] IS INITIAL OR ( p_branch IS INITIAL AND s_branch[] IS INITIAL ).
      MESSAGE s024(sd) WITH 'Preencher a Data de Movimento e Filial!'.
      EXIT.
    ENDIF.
  ENDIF.
*-CS2024000090-28.05.2024-#133805-JT-fim

  "Faturamento RPA - Ini
  DATA(_user_rpa) = abap_false.
  SELECT SINGLE *
    FROM tvarvc INTO @DATA(lwa_user_rpa)
   WHERE name EQ 'MAGGI_USER_RPA'
     AND low = @sy-uname.

  IF sy-subrc IS INITIAL.
    _user_rpa = abap_true.
  ENDIF.

  IF p_bukrs IS INITIAL AND _user_rpa = abap_true.
    MESSAGE 'Empresa é um campo obrigatóro' TYPE 'S'.
    EXIT.
  ENDIF.
  "Faturamento RPA - Fim

  IF s_branch IS INITIAL AND p_branch IS INITIAL AND vg_cockpit NE '04'. "RJF
    MESSAGE 'Filial é um campo obrigatóro' TYPE 'S'.
    EXIT.
  ENDIF.

  IF p_bukrs IS INITIAL AND vg_cockpit NE '04'.
    IF p_branch IS INITIAL.
      MESSAGE 'Empresa é um campo obrigatóro' TYPE 'S'.
      EXIT.
    ELSE.
      SELECT SINGLE bukrs
        INTO p_bukrs
        FROM j_1bbranch
        WHERE branch = p_branch.
    ENDIF.
  ENDIF.

  IF ( s_data IS INITIAL ) AND
     ( r_dt_f = 'X'      ).
    MESSAGE 'Informe a Data de Movimento.' TYPE 'S'.
    SET CURSOR FIELD 'S_DATA-LOW' .
    EXIT.
  ENDIF.

  IF vg_cockpit IS INITIAL. "Check Definição Cockpit
    MESSAGE 'Tipo Cockpit não definido!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF ( vg_cockpit NE '05' ) AND "Bloqueio de Opções em desenvolvimento
     ( vg_cockpit NE '06' ) AND
     ( vg_cockpit NE '07' ) AND
     ( vg_cockpit NE '03' ) AND "ALRS / pteste
     ( vg_cockpit NE '01' ) AND "ALRS / pteste
     ( vg_cockpit NE '04' ) AND "ALRS/ pteste
     ( vg_cockpit NE '09' ) AND
     ( vg_cockpit NE '10' ).
    MESSAGE 'Opção não disponível!' TYPE 'S'.
    EXIT.
  ENDIF.

  CASE vg_cockpit. "Validar Regra ###
    WHEN '04'.
      IF p_coleta IS INITIAL.
        MESSAGE | Para Fertilizantes, informe o ponto de coleta! | TYPE 'S'.
        EXIT.
      ENDIF.
      IF p_branch IS NOT INITIAL.
        SELECT SINGLE *
          FROM zlest0132
          INTO @DATA(w132)
          WHERE branch = @p_branch
          AND   parid  = @p_coleta.
        IF sy-subrc NE 0.
          MESSAGE  |Ponto de coleta não cadastrado na ZLES0126 para filial  { p_branch  }| TYPE 'S'.
          EXIT.
        ENDIF.
      ELSE.
        SELECT SINGLE *
         FROM zlest0132
         INTO @DATA(w1322)
         WHERE  parid  = @p_coleta.
        IF sy-subrc NE 0.
          MESSAGE  |Ponto de coleta não cadastrado na ZLES0126 | TYPE 'S'.
          EXIT.
        ENDIF.
      ENDIF.
    WHEN '09' OR '10'.
    WHEN OTHERS.
      IF _user_rpa EQ abap_false.
        AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
        ID 'WERKS' FIELD  p_branch
        ID 'ACTVT' FIELD '03'.    "Alteração
      ENDIF.
  ENDCASE.

  IF vg_cockpit NE '09' AND vg_cockpit NE '10'.
    "Check de permissão de visão
    AUTHORITY-CHECK OBJECT 'ZLES0136'
    ID 'ZTP_FT_ROM' FIELD vg_cockpit.

    IF sy-subrc <> 0.
      SELECT SINGLE *
        FROM user_addrp INTO @DATA(wl_user)
       WHERE bname = @sy-uname.

      IF ( sy-subrc = 0 ) AND ( wl_user-name_first IS NOT INITIAL ).
        MESSAGE | { wl_user-name_first }, seu perfil está sem acesso ao tipo de faturamento selecionado! | TYPE 'S'.
      ELSE.
        MESSAGE | Perfil do usuário sem acesso ao tipo de faturamento! | TYPE 'S'.
      ENDIF.

      EXIT.
    ENDIF.
  ENDIF.

  IF ( sy-subrc IS INITIAL ) OR ( vg_cockpit EQ '09' OR vg_cockpit EQ '10' ).
    PERFORM: f_seleciona_dados, " Form seleciona dados
             f_saida, " Form de saida
             f_imprime_dados.
  ENDIF.

END-OF-SELECTION.
