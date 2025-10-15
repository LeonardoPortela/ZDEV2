
************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 17.04.2007                                          *
* Tipo de prg ...: Carga de Dados com bapi                             *
* Objetivo    ...: Carga de dados mestres de materiais                 *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 17.04.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
REPORT zmmb001 LINE-SIZE 160
               LINE-COUNT 62(03)
               MESSAGE-ID z01 NO STANDARD PAGE HEADING.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
*
TABLES: tvkov, tq34.

INCLUDE <icon>.

* T.Internas p/ armazenar as estruturas organizacionais do material
* que devem ser criadas
DATA: BEGIN OF t_salesorg OCCURS 0,
        vkorg LIKE bapi_mvke-sales_org,
      END   OF t_salesorg.

DATA: BEGIN OF t_distrchan OCCURS 0,
        vtweg LIKE bapi_mvke-distr_chan,
      END   OF t_distrchan.

* T.internas para criação dos tipos de avaliação do material.
* Separa tipos de avaliação e seus valores correspondentes
DATA : BEGIN OF t_valtype OCCURS 0,
         value LIKE bapi_mbew-val_type,
       END   OF t_valtype.

DATA : BEGIN OF t_pricectrl OCCURS 0,
         value LIKE bapi_mbew-price_ctrl,
       END   OF t_pricectrl.

DATA : BEGIN OF t_price     OCCURS 0,
         std_price LIKE bapi_mbew-std_price,
         moving_pr LIKE bapi_mbew-moving_pr,
       END   OF t_price.

DATA : BEGIN OF t_valclass  OCCURS 0,
         value LIKE bapi_mbew-val_class,
       END   OF t_valclass.

DATA : BEGIN OF t_mlsettle  OCCURS 0,
         value LIKE bapi_mbew-ml_settle,
       END   OF t_mlsettle.

DATA : BEGIN OF t_matorigin OCCURS 0,
         value LIKE bapi_mbew-mat_origin,
       END   OF t_matorigin.

DATA : BEGIN OF t_avaliacao OCCURS 0,
         val_type   LIKE bapi_mbew-val_type,
         price_ctrl LIKE bapi_mbew-price_ctrl,
         val_class  LIKE bapi_mbew-val_class,
         ml_settle  LIKE bapi_mbew-ml_settle,
         mat_origin LIKE bapi_mbew-mat_origin,
         std_price  LIKE bapi_mbew-std_price,
         moving_pr  LIKE bapi_mbew-moving_pr,
       END   OF t_avaliacao.

* T.internas para criação de fatores de conversão do material
* em várias medidas.
DATA : BEGIN OF t_denominatr OCCURS 0,
         value(06),
       END   OF t_denominatr.

DATA : BEGIN OF t_altunit OCCURS 0,
         value(06),
       END   OF t_altunit.

DATA : BEGIN OF t_numerator OCCURS 0,
         value(06),
       END   OF t_numerator.

* Váriáves globais.

DATA: v_matold       LIKE mara-matnr,
      v_mtart        LIKE mara-mtart,
      v_message(220) TYPE c,
      v_bapiret2     LIKE bapiret2,
      v_tabix        LIKE sy-tabix,
      v_ctrlcol1     TYPE alsmex_tabline-value,
      v_lines        LIKE sy-tabix,
      v_icon         TYPE c.

FIELD-SYMBOLS <icone>    LIKE icon_checked.

*----------------------------------------------------------------------*
* Declaração para Batch_input de determinação de CFOP
*----------------------------------------------------------------------*
*
DATA: t_bdcdata LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
      t_messtab LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para BAPI_STDMATERIAL_GETINTNUMBER
*----------------------------------------------------------------------*
*
* Estruturas
DATA : i_matl_type  LIKE bapimatdoa-matl_type,   "Tipo de material
       i_ind_sector LIKE bapimatdoa-ind_sector,  "Setor industrial
       e_return_get LIKE bapireturn1.            "Return function

* Tabelas
DATA : t_material_number LIKE bapimatinr OCCURS 1 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para BAPI_MATERIAL_SAVEDATA
*----------------------------------------------------------------------*
*
* Estruturas
DATA : i_mathead     LIKE bapimathead,
       i_mara        LIKE bapi_mara,
       i_marax       LIKE bapi_marax,
       i_marc        LIKE bapi_marc,
       i_marcx       LIKE bapi_marcx,
       i_mbew        LIKE bapi_mbew,
       i_mbewx       LIKE bapi_mbewx,
       i_mvke        LIKE bapi_mvke,
       i_mvkex       LIKE bapi_mvkex,
       i_mard        LIKE bapi_mard,
       i_mardx       LIKE bapi_mardx,
       i_mlgn        LIKE bapi_mlgn,
       i_mlgnx       LIKE bapi_mlgnx,
       e_return_save LIKE bapiret2.

* Tabelas
DATA : t_makt       LIKE bapi_makt  OCCURS 0 WITH HEADER LINE,
       t_marm       LIKE bapi_marm  OCCURS 0 WITH HEADER LINE,
       t_marmx      LIKE bapi_marmx OCCURS 0 WITH HEADER LINE,
       t_mean       LIKE bapi_mean  OCCURS 0 WITH HEADER LINE,
       t_mlan       LIKE bapi_mlan  OCCURS 0 WITH HEADER LINE,
       t_matreturn2 LIKE bapi_matreturn2
                                         OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para função BAPI_MATINSPCTRL_SAVEREPLICA
*----------------------------------------------------------------------*
DATA : t_inspectionctrl LIKE bapi1001004_qmat
                                         OCCURS 0 WITH HEADER LINE,
       t_return2        LIKE bapiret2   OCCURS 0 WITH HEADER LINE.

*----------------------------------------------------------------------*
* Declaração para função CATSXT_GET_DDIC_FIELDINFO
*----------------------------------------------------------------------*
DATA : t_fields  TYPE ddfields,
       t_fieldsx TYPE ddfields.

FIELD-SYMBOLS: <field>  TYPE any,
               <fieldx> TYPE any.


*----------------------------------------------------------------------*
* Declaração para função GET_MATERIAL_ID
*----------------------------------------------------------------------*
DATA : i_bismt LIKE mara-bismt,
       t_matid LIKE mara       OCCURS 0 WITH HEADER LINE.


*----------------------------------------------------------------------*
* Declaração para função ALSM_EXCEL_TO_INTERNAL_TABLE
*----------------------------------------------------------------------*
DATA: t_planilha         LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF wa_arqtxt,
        old_mat_no       TYPE c LENGTH 18,          "Nº do material
        ind_sector       TYPE c LENGTH 1,           "Setor industrial
        matl_type        TYPE c LENGTH 4,           "Tipo de material
        plant            TYPE c LENGTH 4,           "Centro
        stge_loc         TYPE c LENGTH 4,           "Depósito
        sales_org        TYPE c LENGTH 4,           "Org. de vendas
        distr_chan       TYPE c LENGTH 2,           "Canal de distr.
        whse_no          TYPE c LENGTH 3,           "Nºdepósito
        matl_desc        TYPE c LENGTH 40,          "Descrição material
        base_uom         TYPE c LENGTH 3,           "Un.Medida básica
        matl_group       TYPE c LENGTH 9,           "Grp de mercadorias
        division         TYPE c LENGTH 2,           "Setor de atividade
        denominatr       TYPE c LENGTH 6,           "Den conv med básica
        alt_unit         TYPE c LENGTH 3,           "Un Med alternativa
        numerator        TYPE c LENGTH 6,           "Contador p/convers.
        ean_upc          TYPE c LENGTH 18,          "Nº europ.art.(EAN)
        sales_unit       TYPE c LENGTH 3,           "Unidade de venda
        tax_type1        TYPE c LENGTH 4,           "Ctg Imposto
        taxclass_1       TYPE c LENGTH 1,           "Classf. fiscal mat.
        matl_stats       TYPE c LENGTH 1,           "Grp estat. material
        item_cat         TYPE c LENGTH 4,           "Grp ctgs item mat.

        acct_assgt       TYPE c LENGTH 2,           "Grp classf contábil
        prod_hier        TYPE c LENGTH 18,          "Hierarquia produtos
        batch_mgmt       TYPE c LENGTH 1,           "Cod sujeiçao a lote
        trans_grp        TYPE c LENGTH 4,           "Grupo de transporte
        loadinggrp       TYPE c LENGTH 4,           "Grupo de carregamto
        availcheck       TYPE c LENGTH 2,           "Grp verif.disponib.
        profit_ctr       TYPE c LENGTH 10,          "Centro de lucro
        ctrl_code        TYPE c LENGTH 16,          "Cod ctrl p/imposto
        pur_group        TYPE c LENGTH 3,           "Grp de compradores
        sourcelist       TYPE c LENGTH 1,           "Cod livro forneced
        pur_valkey       TYPE c LENGTH 4,           "Chv valores Compras
        po_unit          TYPE c LENGTH 3,           "Un medida do pedido
        mat_cfop         TYPE c LENGTH 2,           "Categoria CFOP
        mrp_group        TYPE c LENGTH 4,           "Grupo MRP
        mrp_type         TYPE c LENGTH 2,           "Tipo de MRP
        reorder_pt       TYPE c LENGTH 16,          "Ponto de Abastecto
        mrp_ctrler       TYPE c LENGTH 3,           "Planejador MRP
        lotsizekey       TYPE c LENGTH 2,           "Chv calc lote MRP
        round_val        TYPE c LENGTH 16,          "Vl.Arredond.qt ped.
        minlotsize       TYPE c LENGTH 16,          "Tam mínimo do lote
        maxlotsize       TYPE c LENGTH 16,          "Tam máximo do lote
        fixed_lot        TYPE c LENGTH 16,          "Tam fixo lote
        proc_type        TYPE c LENGTH 1,           "Tipo suprimento
        sm_key           TYPE c LENGTH 3,           "Chv prazos p/tempos
        covprofile       TYPE c LENGTH 3,           "Perfil de cobertura
        batchentry       TYPE c LENGTH 1,           "Ctrl entrada lote
        iss_st_loc       TYPE c LENGTH 4,           "Depósito d'produção
        safety_stk       TYPE c LENGTH 16,          "Estoque d'segurança
        plnd_delry       TYPE c LENGTH 3,           "Prz prev fornecimto
        period_ind       TYPE c LENGTH 1,           "Cód do período
        plan_strgp       TYPE c LENGTH 2,           "Grp estratégia plan
        alt_bom_id       TYPE c LENGTH 1,           "Cd Sel list técnica
        grp_reqmts       TYPE c LENGTH 1,           "Cd agrp necessidade
        production_sched TYPE c LENGTH 3,           "Resp.ctrl produção
        prodprof         TYPE c LENGTH 6,           "Perfil ctrl produçã
        setuptime        TYPE c LENGTH 5,           "Tempos d'preparação
        interop          TYPE c LENGTH 5,           "Tempo de transição
        proc_time        TYPE c LENGTH 5,           "Tempo de processamt
        inhseprodt       TYPE c LENGTH 3,           "Tempo de prod inter
        stge_bin         TYPE c LENGTH 10,          "Posição no depósito
        temp_conds       TYPE c LENGTH 2,           "Cd condição tempera
        stor_conds       TYPE c LENGTH 2,           "Condições de estoc.
        stge_pd_un       TYPE c LENGTH 3,           "Un tempo max armaz
        shelf_life       TYPE c LENGTH 4,           "Prazo de validade
        round_up_rule    TYPE c LENGTH 1,           "Regra de arredond.
        period_ind_exp   TYPE c LENGTH 1,           "Cd período DV
        minremlife       TYPE c LENGTH 4,           "Temp min rest valid
        issue_unit       TYPE c LENGTH 3,           "Un medida de saída
        gross_wt         TYPE c LENGTH 16,          "Peso bruto
        net_weight       TYPE c LENGTH 16,          "Peso líquido
        unit_of_wt       TYPE c LENGTH 3,           "Unidade de peso
        volume           TYPE c LENGTH 16,          "Volume
        volumeunit       TYPE c LENGTH 3,           "Unidade de volume
        size_dim         TYPE c LENGTH 32,          "Tamanho/Dimensão
        withdrawal       TYPE c LENGTH 3,           "Cd ctg.dep p/ saída
        placement        TYPE c LENGTH 3,           "Cd ctg dep p/ ent
        stgesector       TYPE c LENGTH 3,           "Cd zona armazenagem
        l_equip_1        TYPE c LENGTH 16,          "Qtd meios aux carre
        leq_unit_1       TYPE c LENGTH 3,           "Unid medida 1.qtde
        unittype_1       TYPE c LENGTH 3,           "Tipo unidade dep.
        ind_post_to_insp TYPE c LENGTH 1,           "Reg em estoque QM

        doc_reqd         TYPE c LENGTH 1,           "Código p/documentaç
        catprofile       TYPE c LENGTH 9,           "Perfil Catálogo
        insp_int         TYPE c LENGTH 5,           "Int até prox ctrl.
        qm_procmnt       TYPE c LENGTH 1,           "QM em Supri. Ativo
        ctrl_key         TYPE c LENGTH 8,           "Chv ctrl adm qualid
        cert_type        TYPE c LENGTH 4,           "Cat. de certificado
        art              TYPE c LENGTH 8,           "Tipo de controle
        val_class        TYPE c LENGTH 50,          "Classe de avaliação
        ml_settle        TYPE c LENGTH 50,          "Apro.custos ledger
        std_price        TYPE c LENGTH 23,          "Preço standard
        moving_pr        TYPE c LENGTH 23,          "Preço médio móvel
        price_unit       TYPE c LENGTH 5,           "Unidade de preço
        val_cat          TYPE c LENGTH 1,           "Categ de avaliação
        val_type         TYPE c LENGTH 50,
        price_ctrl       TYPE c LENGTH 50,
        matl_usage       TYPE c LENGTH 1,           "Utilização material
        mat_origin       TYPE c LENGTH 50,          "Origem de material
        in_house         TYPE c LENGTH 1,           "Produção interna
        orig_group       TYPE c LENGTH 4,           "Grp origem subdiv
        orig_mat         TYPE c LENGTH 1,           "Origem ref material
        qty_struct       TYPE c LENGTH 1,           "Mat calc estr quant
        variance_key     TYPE c LENGTH 4,           "Chave de desvio
        lot_size         TYPE c LENGTH 16,          "Tamanho do lote
        plndprice1       TYPE c LENGTH 23,          "Preço plan futuro
        plndprdate1      TYPE c LENGTH 10,          "Valido apartir de
        item_cat2        TYPE c LENGTH 4,           "Grp cat item geral
        langu            TYPE c LENGTH 1,           "Código de idioma
        matl_desc2       TYPE c LENGTH 40,          "Descrição material

      END   OF wa_arqtxt.

DATA: BEGIN OF wa_datxls,
        line                          LIKE sy-tabix,             "linha da planilha
        old_mat_no                    LIKE i_mara-old_mat_no,    "Id material antigo
        ind_sector                    LIKE i_mathead-ind_sector, "Setor industrial
        matl_type                     LIKE i_mathead-matl_type,  "tipo de material
        plant                         LIKE i_marc-plant,         "Centro
        stge_loc                      LIKE i_mard-stge_loc,      "Depósito
        sales_org                     LIKE t_planilha-value,     "Org. de vendas
        distr_chan                    LIKE t_planilha-value,     "Canal de distr.
        whse_no                       LIKE i_mlgn-whse_no,       "Sistema de depósito
        matl_desc                     LIKE t_makt-matl_desc,     "Descrição material
        base_uom                      LIKE i_mara-base_uom,      "Unid medida básica
        matl_group                    LIKE i_mara-matl_group,    "Grp de mercadorias
        division                      LIKE i_mara-division,      "Setor de atividade
        denominatr                    LIKE t_planilha-value,     "Den conv.mad.básica
        alt_unit                      LIKE t_planilha-value,     "Un med alternativa
        numerator                     LIKE t_planilha-value,     "Contador p/conversã
        ean_upc                       LIKE t_marm-ean_upc,       "EAN
        sales_unit                    LIKE i_mvke-sales_unit,    "Unidade de venda
        tax_type1                     LIKE t_mlan-tax_type_1,    "Ctg Imposto
        taxclass_1                    LIKE t_mlan-taxclass_1,    "Classf. fiscal mat.
        matl_stats                    LIKE i_mvke-matl_stats,    "Grp estat. material
*        mat_pr_grp      like i_mvke-mat_pr_grp,    "Grp de material
        item_cat                      LIKE i_mvke-item_cat,      "Grp ctgs item mat.
        acct_assgt                    LIKE i_mvke-acct_assgt,    "Grp classf contábil
        prod_hier                     LIKE i_mvke-prod_hier,     "Hierarquia produtos
        batch_mgmt                    LIKE i_mara-batch_mgmt,    "Cod sujeiçao a lote
        trans_grp                     LIKE i_mara-trans_grp,     "Grupo de transporte
        loadinggrp                    LIKE i_marc-loadinggrp,    "Grupo de carregamto
        availcheck                    LIKE i_marc-availcheck,    "Grp verif.disponib.
        profit_ctr                    LIKE i_marc-profit_ctr,    "Centro de lucro
        ctrl_code                     LIKE i_marc-ctrl_code,     "Cod ctrl p/imposto
        pur_group                     LIKE i_marc-pur_group,     "Grp de compradores
        sourcelist                    LIKE i_marc-sourcelist,    "Cod livro forneced
        pur_valkey                    LIKE i_mara-pur_valkey,
        po_unit                       LIKE i_mara-po_unit,
*        ctrl_code       like i_marc-ctrl_code,
        mat_cfop                      LIKE i_marc-mat_cfop,      "Categoria CFOP
        mrp_group                     LIKE i_marc-mrp_group,
        mrp_type                      LIKE i_marc-mrp_type,      "Tipo de MRP
        reorder_pt                    LIKE i_marc-reorder_pt,    "Ponto de Abastecto
        mrp_ctrler                    LIKE i_marc-mrp_ctrler,    "Planejador MRP
        lotsizekey                    LIKE i_marc-lotsizekey,    "Chv calc lote MRP
        round_val                     LIKE i_marc-round_val,
        minlotsize                    LIKE i_marc-minlotsize,    "Tam mínimo do lote
        maxlotsize                    LIKE i_marc-maxlotsize,    "Tam máximo do lote
        fixed_lot                     LIKE i_marc-fixed_lot,     "Tam fixo lote
        proc_type                     LIKE i_marc-proc_type,     "Tipo suprimento
        sm_key                        LIKE i_marc-sm_key,        "Chv prazos p/tempos
        covprofile                    LIKE i_marc-covprofile,    "Perfil de cobertura
        batchentry                    LIKE i_marc-batchentry,    "Ctrl entrada lote
        iss_st_loc                    LIKE i_marc-iss_st_loc,    "Depósito d'produção
        safety_stk                    LIKE i_marc-safety_stk,    "Estoque d'segurança
        plnd_delry                    LIKE i_marc-plnd_delry,    "Prz prev fornecimto
        period_ind                    LIKE i_marc-period_ind,    "Cód do período
        plan_strgp                    LIKE i_marc-plan_strgp,    "Grp estratégia plan
        alt_bom_id                    LIKE i_marc-alt_bom_id,    "Cd Sel list técnica
        grp_reqmts                    LIKE i_marc-grp_reqmts,    "Cd agrp necessidade
        production_scheduler                       "Resp.ctrl produção
          LIKE i_marc-production_scheduler,
        prodprof                      LIKE i_marc-prodprof,      "Perfil ctrl produçã
        setuptime                     LIKE i_marc-setuptime,     "Tempos d'preparação
        interop                       LIKE i_marc-interop,
        proc_time                     LIKE i_marc-proc_time,
        inhseprodt                    LIKE i_marc-inhseprodt,
        stge_bin                      LIKE i_mard-stge_bin,
        temp_conds                    LIKE i_mara-temp_conds,    "Cd condição tempera
        stor_conds                    LIKE i_mara-stor_conds,
        stge_pd_un                    LIKE i_marc-stge_pd_un,    "Un tempo max armaz
        shelf_life                    LIKE i_mara-shelf_life,    "Prazo de validade
        round_up_rule_expiration_date
          LIKE i_mara-round_up_rule_expiration_date,
        period_ind_expiration_date                 "Cd período DV
          LIKE i_mara-period_ind_expiration_date,
        minremlife                    LIKE i_mara-minremlife,    "Temp min rest valid
*        cc_ph_inv       like i_marc-cc_ph_inv,     "Código inventário
        issue_unit                    LIKE i_marc-issue_unit,    "Un medida de saída
        gross_wt                      LIKE t_marm-gross_wt,      "Peso bruto
        net_weight                    LIKE i_mara-net_weight,    "Peso líquido
        unit_of_wt                    LIKE i_mara-unit_of_wt,    "Unidade de peso
        volume                        LIKE t_marm-volume,
        volumeunit                    LIKE t_marm-volumeunit,
        size_dim                      LIKE i_mara-size_dim,
        withdrawal                    LIKE i_mlgn-withdrawal,    "Cd ctg.dep p/ saída
        placement                     LIKE i_mlgn-placement,     "Cd ctg dep p/ ent
        stgesector                    LIKE i_mlgn-stgesector,    "Cd zona armazenagem
        l_equip_1                     LIKE i_mlgn-l_equip_1,     "Qtd meios aux carre
        leq_unit_1                    LIKE i_mlgn-leq_unit_1,    "Unid medida 1.qtde
        unittype_1                    LIKE i_mlgn-unittype_1,    "Tipo unidade dep.
        ind_post_to_insp_stock
          LIKE i_marc-ind_post_to_insp_stock,
        doc_reqd                      LIKE i_marc-doc_reqd,      "Código p/documentaç
        catprofile                    LIKE i_mara-catprofile,
        insp_int                      LIKE i_marc-insp_int,
        qm_procmnt                    LIKE i_mara-qm_procmnt,
        ctrl_key                      LIKE i_marc-ctrl_key,
        cert_type                     LIKE i_marc-cert_type,
        art                           LIKE bapi1001004_qmat-insptype, "Tipo controle
        val_class                     LIKE t_planilha-value,     "Classe de avaliação
        ml_settle                     LIKE t_planilha-value,     "Apro.custos ledger
        std_price                     LIKE i_mbew-std_price,     "Preço standard
        moving_pr                     LIKE i_mbew-moving_pr,
        price_unit                    LIKE i_mbew-price_unit,    "Unidade de preço
        val_cat                       LIKE i_mbew-val_cat,       "Categ de avaliação
        val_type                      LIKE t_planilha-value,
        price_ctrl                    LIKE t_planilha-value,
        matl_usage                    LIKE i_mbew-matl_usage,    "Utilização material
        mat_origin                    LIKE t_planilha-value,     "Origem de material
        in_house                      LIKE i_mbew-in_house,      "Produção interna
        orig_group                    LIKE i_mbew-orig_group,    "Grp origem subdiv
        orig_mat                      LIKE i_mbew-orig_mat,      "Origem ref material
        qty_struct                    LIKE i_mbew-qty_struct,    "Mat calc estr quant
        variance_key                  LIKE i_marc-variance_key,  "Chave de desvio
        lot_size                      LIKE i_marc-lot_size,
        plndprice1                    LIKE i_mbew-plndprice1,
        plndprdate1                   LIKE i_mbew-plndprdate1,
        item_cat2                     LIKE i_mara-item_cat,
        langu                         LIKE t_makt-langu,         "Idioma inglês
        matl_desc2                    LIKE t_makt-matl_desc,     "Descrição material
      END   OF wa_datxls.

DATA: t_arqtxt LIKE STANDARD TABLE OF wa_arqtxt,
      t_datxls LIKE STANDARD TABLE OF wa_datxls.

DATA: v_ncoln            LIKE sy-index VALUE 120.

CONSTANTS: c_ncoln LIKE sy-index VALUE 01,
           c_nline LIKE sy-index VALUE 01.

*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_cami  LIKE rlgrap-filename.    "Arquivo Excel
  PARAMETERS: p_nline LIKE sy-index.           "Nro aproximado de Linhas
SELECTION-SCREEN END   OF BLOCK b0.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  PARAMETERS: p_optxt RADIOBUTTON GROUP arq DEFAULT 'X',
              p_opxls RADIOBUTTON GROUP arq.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-004.
  PARAMETERS:
* Visão de Dados básicos
    p_basic LIKE bapimathead-basic_view      AS CHECKBOX
                 MODIF ID bl1,
* Visão de Vendas e distribuição
    p_sales LIKE bapimathead-sales_view      AS CHECKBOX
                 MODIF ID bl1,
* Visão de Compras
    p_purch LIKE bapimathead-purchase_view   AS CHECKBOX
                 MODIF ID bl1,
* Visão de MRP
    p_mrp   LIKE bapimathead-mrp_view        AS CHECKBOX
                 MODIF ID bl1,
* Visão de Previsão
    p_forec LIKE bapimathead-forecast_view   AS CHECKBOX
                 MODIF ID bl1,
* Visão de Preparação do trabalho
    p_work  LIKE bapimathead-work_sched_view AS CHECKBOX
                 MODIF ID bl1,
* Visão de Meios auxiliares de produção
    p_prt   LIKE bapimathead-prt_view        AS CHECKBOX
                 MODIF ID bl1,
* Visão de Depósito
    p_stora LIKE bapimathead-storage_view    AS CHECKBOX
                 MODIF ID bl1,
* Visão de Administração de depósitos
    p_wareh LIKE bapimathead-warehouse_view  AS CHECKBOX
                 MODIF ID bl1,
* Visão de Administração da qualidade
    p_quali LIKE bapimathead-quality_view    AS CHECKBOX
                 MODIF ID bl1,
* Visão da contabilidade
    p_accou LIKE bapimathead-account_view    AS CHECKBOX
                 MODIF ID bl1,
* Visão Cálculo de custo
    p_cost  LIKE bapimathead-cost_view       AS CHECKBOX
                 MODIF ID bl1.
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-005.
  PARAMETERS: p_cfop TYPE c  AS CHECKBOX,
*            p_clas type c  as checkbox,
              p_mode TYPE c  DEFAULT 'N'.
SELECTION-SCREEN END   OF BLOCK b3.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF ( screen-group1 EQ 'BL1' ).
*      if ( p_cfop eq 'X' ) or ( p_clas eq 'X' ).
*        screen-input = '0'.
*      else.
      screen-input = '1'.
*      endif.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cami.
  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_cami
      mask             = ',*.xls.'
      mode             = 'O'
      title            = 'Arquivo a importar !'
    IMPORTING
      filename         = p_cami
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.
*----------------------------------------------------------------------*
* Event initialization
*----------------------------------------------------------------------*
*
INITIALIZATION.
  SET TITLEBAR 'TITLE01'.
  CLEAR p_cami.
  IF p_nline IS INITIAL.
    p_nline = 500.
  ENDIF.
  MOVE 'X':    TO p_basic,
               TO p_sales,
               TO p_purch,
               TO p_mrp,
               TO p_forec,
               TO p_work,
               TO p_prt,
               TO p_stora,
               TO p_wareh,
               TO p_quali,
               TO p_accou,
               TO p_cost.

*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
START-OF-SELECTION.
  PERFORM f_processa_planilha.
  SORT t_datxls BY line.
  LOOP AT t_datxls INTO wa_datxls.
    PERFORM f_limpa_dados.
    PERFORM f_carrega_dados_bapi.
    ASSIGN icon_incomplete TO <icone>.
    IF ( p_cfop IS INITIAL ). " and ( p_clas is initial ).
      PERFORM f_maintain_material.
    ELSE.
      IF ( NOT p_cfop IS INITIAL ).
        PERFORM f_change_matcfop.
      ENDIF.
*      if ( not p_clas is initial ).
*        perform f_create_classificacao.
*      endif.
    ENDIF.

  ENDLOOP.
*----------------------------------------------------------------------*
* Event end-of-selection
*----------------------------------------------------------------------*
*
END-OF-SELECTION.
  ULINE.
*----------------------------------------------------------------------*
* Event TOP_OF_PAGE.
*----------------------------------------------------------------------*
*
TOP-OF-PAGE.

  ULINE.
  WRITE: /01 sy-vline,
          30 'Log de erros de importação - Cargas de Materiais',
          99 'Data.:',
         106 sy-datum,
         119 'Hora.:',
         126 sy-uzeit,
         137 'Pag.:',
         143 sy-pagno,
         160 sy-vline.
  ULINE.
  WRITE: /01 sy-vline,
          03 'Nome do arquivo importado:',
          30 p_cami,
         160 sy-vline.

  WRITE: /01 sy-vline,
          03 'Nro de linhas do arquivo :',
          30 p_nline,
          99 'Usuário:',
         108 sy-uname,
         119 'Programa:',
         129 sy-repid,
         160 sy-vline.
  ULINE.
  WRITE: /01 sy-vline,
          06 'Linha',
          13 'ID antigo',
          24 'Tp Mat',
          32 'Novo ID material',
          50 'Mensagem do log',
         160 sy-vline.
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  f_imprime_erros
*&---------------------------------------------------------------------*
FORM f_imprime_erros USING    p_message.
  WRITE: /01 sy-vline,
          03 <icone> AS ICON,
          11 wa_datxls-line,
          13 wa_datxls-old_mat_no,
          24 i_matl_type,
          32 v_matold,
          50 p_message,
         160 sy-vline.
ENDFORM.                    " f_imprime_erros

*----------------------------------------------------------------------*
* Event END_OF_PAGE.
*----------------------------------------------------------------------*
*
END-OF-PAGE.
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  f_processa_planilha
*&---------------------------------------------------------------------*
FORM f_processa_planilha.

  DATA: vl_bukrs LIKE bkpf-bukrs,
        vl_subrc LIKE sy-subrc,
        vl_cami  TYPE string,
        vl_line  LIKE sy-tabix.


  IF ( p_optxt EQ 'X' ).
*
* Carrega arquivo texto com dados de clientes
*
    REFRESH t_arqtxt.
    vl_cami = p_cami.

    CALL FUNCTION 'GUI_UPLOAD'
      EXPORTING
        filename                = vl_cami
      TABLES
        data_tab                = t_arqtxt
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        OTHERS                  = 22.

    IF ( sy-subrc NE 0 ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    CLEAR vl_line.

    LOOP AT t_arqtxt INTO wa_arqtxt.
      CLEAR  wa_datxls.
      vl_line = vl_line + 1.
      CHECK ( vl_line LE p_nline ).

      wa_datxls-line       = vl_line.
      TRANSLATE wa_arqtxt-matl_type TO UPPER CASE.
      TRANSLATE wa_arqtxt-plant     TO UPPER CASE.
      TRANSLATE wa_arqtxt-stge_loc  TO UPPER CASE.
*      wa_datxls-matl_type  = wa_arqtxt-matl_type.
*      wa_datxls-plant      = wa_arqtxt-plant.
*      wa_datxls-stge_loc   = wa_arqtxt-stge_loc.

      REPLACE ',' WITH '.' INTO wa_arqtxt-gross_wt.
      REPLACE ',' WITH '.' INTO wa_arqtxt-net_weight.
      REPLACE ',' WITH '.' INTO wa_arqtxt-volume.
      REPLACE ',' WITH '.' INTO wa_arqtxt-std_price.
      REPLACE ',' WITH '.' INTO wa_arqtxt-moving_pr.
*      wa_datxls-gross_wt   = wa_arqtxt-gross_wt.
*      wa_datxls-net_weight = wa_arqtxt-net_weight.
*      wa_datxls-volume     = wa_arqtxt-volume.
*      wa_datxls-std_price  = wa_arqtxt-std_price.
*      wa_datxls-moving_pr  = wa_arqtxt-moving_pr.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input    = wa_arqtxt-base_uom
          language = sy-langu
        IMPORTING
          output   = wa_arqtxt-base_uom.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_arqtxt-mat_cfop
        IMPORTING
          output = wa_arqtxt-mat_cfop.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input    = wa_arqtxt-stge_pd_un
          language = sy-langu
        IMPORTING
          output   = wa_arqtxt-stge_pd_un.

      CALL FUNCTION 'CONVERSION_EXIT_PERKZ_INPUT'
        EXPORTING
          input  = wa_arqtxt-period_ind_exp
        IMPORTING
          output = wa_arqtxt-period_ind_exp.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input    = wa_arqtxt-volumeunit
          language = sy-langu
        IMPORTING
          output   = wa_arqtxt-volumeunit.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input    = wa_arqtxt-leq_unit_1
          language = sy-langu
        IMPORTING
          output   = wa_arqtxt-leq_unit_1.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_arqtxt-profit_ctr
        IMPORTING
          output = wa_arqtxt-profit_ctr.

      MOVE-CORRESPONDING wa_arqtxt TO wa_datxls.
      wa_arqtxt-item_cat = wa_arqtxt-item_cat2 = wa_arqtxt-item_cat.
      wa_arqtxt-period_ind_exp = wa_datxls-period_ind_expiration_date.

      APPEND wa_datxls  TO t_datxls.
    ENDLOOP.

  ELSE.

    CLEAR   t_planilha.
    REFRESH t_planilha.
*> Carrega planilha selecionada para tabela interna
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_cami
        i_begin_col             = c_ncoln
        i_begin_row             = c_nline
        i_end_col               = v_ncoln
        i_end_row               = p_nline
      TABLES
        intern                  = t_planilha
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE e004 WITH TEXT-002.
    ENDIF.
*> Elimina linhas de cabeçalho e texto da tabela interna.
*  delete t_planilha where ( col   eq 1 )
*                      and ( value eq 'CAB' ).
*
    SORT t_planilha BY row col.
    REFRESH t_datxls.
    CLEAR   wa_datxls.

*> Move valores das células ti com dados de criação dos materiais
    LOOP AT t_planilha.

      ON CHANGE OF t_planilha-row.
        v_ctrlcol1 = t_planilha-value.
      ENDON.

      IF v_ctrlcol1 EQ 'CAB'.
        DELETE t_planilha.
        CONTINUE.
      ENDIF.

      AT NEW row.
        CLEAR wa_datxls.
        wa_datxls-line = t_planilha-row.
      ENDAT.


      CASE t_planilha-col.
        WHEN   4. wa_datxls-old_mat_no = t_planilha-value.
        WHEN   5. wa_datxls-ind_sector = t_planilha-value.
        WHEN   6.
          TRANSLATE t_planilha-value TO UPPER CASE.
          wa_datxls-matl_type  = t_planilha-value.
        WHEN   7.
          TRANSLATE t_planilha-value TO UPPER CASE.
          wa_datxls-plant      = t_planilha-value.
        WHEN   8.
          TRANSLATE t_planilha-value TO UPPER CASE.
          wa_datxls-stge_loc   = t_planilha-value.
        WHEN   9. wa_datxls-sales_org  = t_planilha-value.
        WHEN  10. wa_datxls-distr_chan = t_planilha-value.
        WHEN  11. wa_datxls-whse_no    = t_planilha-value.
        WHEN  12. " WM
        WHEN  13. wa_datxls-matl_desc  = t_planilha-value.
        WHEN  14.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input    = t_planilha-value
              language = sy-langu
            IMPORTING
              output   = wa_datxls-base_uom.
        WHEN  15. wa_datxls-matl_group = t_planilha-value.
        WHEN  16. wa_datxls-division   = t_planilha-value.
        WHEN  17. wa_datxls-denominatr = t_planilha-value.
        WHEN  18. wa_datxls-alt_unit   = t_planilha-value.
        WHEN  19. wa_datxls-numerator  = t_planilha-value.
        WHEN  20. wa_datxls-ean_upc    = t_planilha-value.
        WHEN  21. wa_datxls-sales_unit = t_planilha-value.
        WHEN  22. wa_datxls-tax_type1  = t_planilha-value.
        WHEN  23. wa_datxls-taxclass_1 = t_planilha-value.
        WHEN  24. wa_datxls-matl_stats = t_planilha-value.
        WHEN  25.
          wa_datxls-item_cat = wa_datxls-item_cat2 = t_planilha-value.
        WHEN  26. wa_datxls-acct_assgt = t_planilha-value.
        WHEN  27. wa_datxls-prod_hier  = t_planilha-value.
        WHEN  28. wa_datxls-availcheck = t_planilha-value.
        WHEN  29. wa_datxls-batch_mgmt = t_planilha-value.
        WHEN  30. wa_datxls-trans_grp  = t_planilha-value.
        WHEN  31. wa_datxls-loadinggrp = t_planilha-value.
        WHEN  32. wa_datxls-ctrl_code  = t_planilha-value.
        WHEN  33. wa_datxls-pur_group  = t_planilha-value.
        WHEN  34. wa_datxls-sourcelist = t_planilha-value.
        WHEN  35. wa_datxls-pur_valkey = t_planilha-value.
        WHEN  36. wa_datxls-po_unit    = t_planilha-value.
        WHEN  37. wa_datxls-ctrl_code  = t_planilha-value.
        WHEN  38.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = t_planilha-value
            IMPORTING
              output = wa_datxls-mat_cfop.
        WHEN  39. wa_datxls-mrp_group  = t_planilha-value.
        WHEN  40. wa_datxls-mrp_type   = t_planilha-value.
        WHEN  41. wa_datxls-reorder_pt = t_planilha-value.
        WHEN  42. wa_datxls-mrp_ctrler = t_planilha-value.
        WHEN  43. wa_datxls-lotsizekey = t_planilha-value.
        WHEN  44. wa_datxls-round_val  = t_planilha-value.
        WHEN  45. wa_datxls-minlotsize = t_planilha-value.
        WHEN  46. wa_datxls-maxlotsize = t_planilha-value.
        WHEN  47. wa_datxls-fixed_lot  = t_planilha-value.
        WHEN  48. wa_datxls-proc_type  = t_planilha-value.
        WHEN  49. wa_datxls-sm_key     = t_planilha-value.
        WHEN  50. wa_datxls-covprofile = t_planilha-value.
        WHEN  51. wa_datxls-batchentry = t_planilha-value.
        WHEN  52. wa_datxls-iss_st_loc = t_planilha-value.
        WHEN  53.
        WHEN  54. wa_datxls-safety_stk = t_planilha-value.
        WHEN  55. wa_datxls-plnd_delry = t_planilha-value.
        WHEN  56. wa_datxls-period_ind = t_planilha-value.
        WHEN  57. wa_datxls-plan_strgp = t_planilha-value.
        WHEN  58.
        WHEN  59. wa_datxls-alt_bom_id = t_planilha-value.
        WHEN  60. wa_datxls-grp_reqmts = t_planilha-value.
        WHEN  61. wa_datxls-production_scheduler = t_planilha-value.
        WHEN  62. wa_datxls-prodprof   = t_planilha-value.
        WHEN  63. wa_datxls-setuptime  = t_planilha-value.
        WHEN  64. wa_datxls-interop    = t_planilha-value.
        WHEN  65. wa_datxls-proc_time  = t_planilha-value.
        WHEN  66. wa_datxls-inhseprodt = t_planilha-value.
        WHEN  67. wa_datxls-stge_bin   = t_planilha-value.
        WHEN  68. wa_datxls-temp_conds = t_planilha-value.
        WHEN  69. wa_datxls-stor_conds = t_planilha-value.
        WHEN  70.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input    = t_planilha-value
              language = sy-langu
            IMPORTING
              output   = wa_datxls-stge_pd_un.
        WHEN  71. wa_datxls-shelf_life = t_planilha-value.
        WHEN  72.
          wa_datxls-round_up_rule_expiration_date = t_planilha-value.

        WHEN  73.
          CALL FUNCTION 'CONVERSION_EXIT_PERKZ_INPUT'
            EXPORTING
              input  = t_planilha-value
            IMPORTING
              output = wa_datxls-period_ind_expiration_date.

        WHEN  74. wa_datxls-minremlife = t_planilha-value.
*     when  75. wa_datxls-cc_ph_inv  = t_planilha-value.
        WHEN  75. wa_datxls-issue_unit = t_planilha-value.
        WHEN  76.
          REPLACE ',' WITH '.' INTO t_planilha-value.
          wa_datxls-gross_wt   = t_planilha-value.
        WHEN  77.
          REPLACE ',' WITH '.' INTO t_planilha-value.
          wa_datxls-net_weight = t_planilha-value.
        WHEN  78. wa_datxls-unit_of_wt = t_planilha-value.
        WHEN  79.
          REPLACE ',' WITH '.' INTO t_planilha-value.
          wa_datxls-volume             = t_planilha-value.
        WHEN  80.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input    = t_planilha-value
              language = sy-langu
            IMPORTING
              output   = wa_datxls-volumeunit.

        WHEN  81. wa_datxls-size_dim   = t_planilha-value.
        WHEN  82." WM
        WHEN  83. wa_datxls-withdrawal = t_planilha-value.
        WHEN  84. wa_datxls-placement  = t_planilha-value.
        WHEN  85. wa_datxls-stgesector = t_planilha-value.
        WHEN  86." WM
        WHEN  87. wa_datxls-l_equip_1  = t_planilha-value.
        WHEN  88.
          CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
            EXPORTING
              input    = t_planilha-value
              language = sy-langu
            IMPORTING
              output   = wa_datxls-leq_unit_1.
        WHEN  89. wa_datxls-unittype_1 = t_planilha-value.
        WHEN  90.
          wa_datxls-ind_post_to_insp_stock = t_planilha-value.
        WHEN  91. wa_datxls-doc_reqd   = t_planilha-value.
        WHEN  92. wa_datxls-catprofile = t_planilha-value.
        WHEN  93. wa_datxls-insp_int   = t_planilha-value.
        WHEN  94. wa_datxls-qm_procmnt = t_planilha-value.
        WHEN  95. wa_datxls-ctrl_key   = t_planilha-value.
        WHEN  96. wa_datxls-cert_type  = t_planilha-value.
        WHEN  97. wa_datxls-art        = t_planilha-value.
        WHEN  98. wa_datxls-val_class  = t_planilha-value.
        WHEN  99. wa_datxls-ml_settle  = t_planilha-value.
        WHEN 100.
          REPLACE ',' WITH '.' INTO t_planilha-value.
          wa_datxls-std_price  = t_planilha-value.
        WHEN 101.
          REPLACE ',' WITH '.' INTO t_planilha-value.
          wa_datxls-moving_pr  = t_planilha-value.
        WHEN 102. wa_datxls-price_unit   = t_planilha-value.
        WHEN 103. wa_datxls-val_cat      = t_planilha-value.
        WHEN 104. wa_datxls-val_type     = t_planilha-value.
        WHEN 105. wa_datxls-price_ctrl   = t_planilha-value.
        WHEN 106. wa_datxls-matl_usage   = t_planilha-value.
        WHEN 107. wa_datxls-mat_origin   = t_planilha-value.
        WHEN 108. wa_datxls-in_house     = t_planilha-value.
        WHEN 109. wa_datxls-orig_group   = t_planilha-value.
        WHEN 110. wa_datxls-orig_mat     = t_planilha-value.
        WHEN 111. wa_datxls-qty_struct   = t_planilha-value.
        WHEN 112. wa_datxls-variance_key = t_planilha-value.
        WHEN 113.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = t_planilha-value
            IMPORTING
              output = wa_datxls-profit_ctr.

        WHEN 114. wa_datxls-lot_size     = t_planilha-value.
        WHEN 115. wa_datxls-plndprice1   = t_planilha-value.
        WHEN 116. wa_datxls-plndprdate1  = t_planilha-value.
        WHEN 117. wa_datxls-langu        = t_planilha-value.
        WHEN 118. wa_datxls-matl_desc2   = t_planilha-value.
      ENDCASE.

      AT END OF row.
        APPEND wa_datxls TO t_datxls.
      ENDAT.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " f_processa_planilha
*&---------------------------------------------------------------------*
*&      Form  f_limpa_dados
*&---------------------------------------------------------------------*
FORM f_limpa_dados.

  CLEAR   : i_matl_type, i_ind_sector.

  CLEAR     t_material_number.
  REFRESH   t_material_number.

  CLEAR   : i_mathead, i_mara, i_marax,
                       i_marc, i_marcx,
                       i_mbew, i_mbewx,
                       i_mvke, i_mvkex,
                       i_mard, i_mardx, e_return_save.

  CLEAR   : t_makt, t_marm, t_marmx, t_mean, t_mlan.
  REFRESH : t_makt, t_marm, t_marmx, t_mean, t_mlan.

  CLEAR   : t_inspectionctrl, t_return2, t_matreturn2.
  REFRESH : t_inspectionctrl, t_return2, t_matreturn2.

  CLEAR   : i_bismt, t_matid, t_salesorg, t_distrchan.
  REFRESH :          t_matid, t_salesorg, t_distrchan.

  CLEAR   : t_valtype, t_pricectrl, t_valclass, t_mlsettle, t_matorigin.
  REFRESH : t_valtype, t_pricectrl, t_valclass, t_mlsettle, t_matorigin.

  CLEAR   : t_avaliacao, t_price.
  REFRESH : t_avaliacao, t_price.
ENDFORM.                    " f_limpa_dados

*&---------------------------------------------------------------------*
*&      Form  f_carrega_dados_bapi
*&---------------------------------------------------------------------*
FORM f_carrega_dados_bapi.

* Dados para pegar o próximo nro de material
  i_matl_type               = wa_datxls-matl_type.
  i_ind_sector              = wa_datxls-ind_sector.

* Dados para criação do material.
  i_mathead-ind_sector      = wa_datxls-ind_sector.
  i_mathead-matl_type       = wa_datxls-matl_type.

* Marca as visões do material a serem criadas
  i_mathead-basic_view      = p_basic.
  i_mathead-sales_view      = p_sales.
  i_mathead-purchase_view   = p_purch.
  i_mathead-mrp_view        = p_mrp.
  i_mathead-forecast_view   = p_forec.
  i_mathead-work_sched_view = p_work.
  i_mathead-prt_view        = p_prt.
  i_mathead-storage_view    = p_stora.
  i_mathead-warehouse_view  = p_wareh.
  i_mathead-quality_view    = p_quali.
  i_mathead-account_view    = p_accou.
  i_mathead-cost_view       = p_cost.
* i_mathead-inp_fld_check   = 'E'.

* Dados do material a nível de mandante
  MOVE-CORRESPONDING wa_datxls TO i_mara.

  i_bismt              = wa_datxls-old_mat_no.
  i_mara-pvalidfrom    = ' '.
  i_mara-svalidfrom    = ' '.

  PERFORM f_set_update_fields USING 'i_marax'.

* Dados do material a nível de centro

  MOVE-CORRESPONDING wa_datxls TO i_marc.
  i_marc-eff_o_day     = ' '.
  i_marc-d_to_ref_m    = ' '.
  i_marc-ex_cert_dt    = ' '.
  i_marc-pvalidfrom    = ' '.

  i_marcx-plant        = i_marc-plant.
  PERFORM f_set_update_fields USING 'i_marcx'.

* Dados do material a nível de depósito
  IF ( NOT p_stora IS INITIAL ).
    i_mard-plant         = i_mardx-plant    = wa_datxls-plant.
    i_mard-stge_loc      = i_mardx-stge_loc = wa_datxls-stge_loc.
    i_mard-stge_bin      = wa_datxls-stge_bin.
    i_mardx-stge_bin     = 'X'.
  ENDIF.

* Dados de avaliação do material
  BREAK brxs_basis.
  MOVE-CORRESPONDING wa_datxls TO i_mbew.
  i_mbew-val_area      = i_mbewx-val_area = wa_datxls-plant.

* Separa tipos de avaliação e seus valores correspondentes
  IF NOT wa_datxls-val_type IS INITIAL.
    SPLIT wa_datxls-val_type AT '/' INTO TABLE t_valtype.
  ENDIF.
  i_mbew-val_type      = i_mbewx-val_type = ' '.

  SPLIT wa_datxls-price_ctrl AT '/' INTO TABLE t_pricectrl.
* Ajusta preço standard ou médio conforme o controle de preço
  LOOP AT t_pricectrl.
    CLEAR t_price.
    IF t_pricectrl-value EQ 'S'.
      t_price-std_price = i_mbew-std_price.
    ELSE.
      t_price-moving_pr = i_mbew-moving_pr.
    ENDIF.
    APPEND t_price.
  ENDLOOP.

  READ TABLE t_pricectrl   INDEX 1.
  i_mbew-price_ctrl    = t_pricectrl-value.
  READ TABLE t_price       INDEX 1.
  MOVE-CORRESPONDING t_price TO i_mbew.
  DELETE t_pricectrl INDEX 1.
  DELETE t_price     INDEX 1.

  SPLIT wa_datxls-val_class  AT '/' INTO TABLE t_valclass.
  READ TABLE t_valclass   INDEX 1.
  i_mbew-val_class     = t_valclass-value.
  DELETE t_valclass INDEX 1.

  SPLIT wa_datxls-ml_settle AT '\' INTO TABLE t_mlsettle.
  READ TABLE t_mlsettle   INDEX 1.
  i_mbew-ml_settle     = t_mlsettle-value.
  DELETE t_mlsettle INDEX 1.

  SPLIT wa_datxls-mat_origin AT '/' INTO TABLE t_matorigin.
  READ TABLE t_matorigin  INDEX 1.
  i_mbew-mat_origin    = t_matorigin-value.
  DELETE t_matorigin INDEX 1.

  LOOP AT t_valclass.
    v_tabix = sy-tabix.
    READ TABLE t_pricectrl INDEX v_tabix.
    IF sy-subrc NE 0. CLEAR t_pricectrl. ENDIF.
    READ TABLE t_price     INDEX v_tabix.
    IF sy-subrc NE 0. CLEAR t_pricectrl. ENDIF.

    READ TABLE t_valtype  INDEX v_tabix.
    IF sy-subrc NE 0. CLEAR t_valtype. ENDIF.
    READ TABLE t_mlsettle  INDEX v_tabix.
    IF sy-subrc NE 0. CLEAR t_mlsettle.  ENDIF.
    READ TABLE t_matorigin INDEX v_tabix.
    IF sy-subrc NE 0. CLEAR t_matorigin. ENDIF.

    CLEAR t_avaliacao.
    t_avaliacao-val_type   = t_valtype-value.
    t_avaliacao-price_ctrl = t_pricectrl-value.
    t_avaliacao-val_class  = t_valclass-value.
    t_avaliacao-ml_settle  = t_mlsettle-value.
    t_avaliacao-mat_origin = t_matorigin-value.
    t_avaliacao-std_price  = t_price-std_price.
    t_avaliacao-moving_pr  = t_price-moving_pr.
    APPEND t_avaliacao.
  ENDLOOP.

  i_mbew-valid_from    = ' '.
  i_mbew-plndprdate2   = ' '.
  i_mbew-plndprdate3   = ' '.

  PERFORM f_set_update_fields USING 'i_mbewx'.

* Dados de vendas do material
  IF ( NOT p_sales IS INITIAL ).

    SPLIT wa_datxls-sales_org AT '/' INTO TABLE t_salesorg.
    READ TABLE t_salesorg INDEX 1.
    i_mvke-sales_org     = i_mvkex-sales_org = t_salesorg-vkorg.
    SPLIT wa_datxls-distr_chan AT '/' INTO TABLE t_distrchan.
    READ TABLE t_distrchan INDEX 1.
    i_mvke-distr_chan    = i_mvkex-distr_chan = t_distrchan-vtweg.
    i_mvke-valid_from    = ' '.
    i_mvke-sales_unit    = wa_datxls-sales_unit.
    i_mvke-item_cat      = wa_datxls-item_cat.
    i_mvke-prod_hier     = wa_datxls-prod_hier.
    i_mvke-matl_stats    = wa_datxls-matl_stats.
    i_mvke-acct_assgt    = wa_datxls-acct_assgt.

    PERFORM f_set_update_fields USING 'i_mvkex'.

  ENDIF.
*  i_mvkex-sales_unit = i_mvkex-item_cat   = i_mvkex-prod_hier   = 'X'.
*  i_mvkex-mat_pr_grp = i_mvkex-acct_assgt  = 'X'.
*
* Textos breves do material
  t_makt-langu         = sy-langu.
  t_makt-matl_desc     = wa_datxls-matl_desc.
  APPEND t_makt.

  IF NOT wa_datxls-langu IS INITIAL.
    t_makt-langu         = wa_datxls-langu.
    t_makt-matl_desc     = wa_datxls-matl_desc2.
    APPEND t_makt.
  ENDIF.

* Unidades de medida
  t_marm-alt_unit      = wa_datxls-base_uom.
  t_marm-numerator     = 1.
  t_marm-denominatr    = 1.
  t_marm-ean_upc       = wa_datxls-ean_upc.
  t_marm-gross_wt      = wa_datxls-gross_wt.
  t_marm-unit_of_wt    = wa_datxls-unit_of_wt.
  t_marm-volume        = wa_datxls-volume.
  t_marm-volumeunit    = wa_datxls-volumeunit.
  APPEND t_marm.

  t_marmx-alt_unit     = wa_datxls-base_uom.
  MOVE 'X':            TO t_marmx-numerator,
                       TO t_marmx-denominatr,
                       TO t_marmx-ean_upc,
                       TO t_marmx-gross_wt,
                       TO t_marmx-unit_of_wt,
                       TO t_marmx-volume,
                       TO t_marmx-volumeunit.

  IF t_marm-unit_of_wt IS INITIAL.
    CLEAR t_marmx-unit_of_wt.
  ENDIF.
  APPEND t_marmx.

  IF NOT wa_datxls-alt_unit IS INITIAL.

    SPLIT wa_datxls-alt_unit   AT '/' INTO TABLE t_altunit.
    SPLIT wa_datxls-numerator  AT '/' INTO TABLE t_numerator.
    SPLIT wa_datxls-denominatr AT '/' INTO TABLE t_denominatr.

    DESCRIBE TABLE t_altunit LINES v_lines.

    DO v_lines TIMES.

      READ TABLE t_altunit    INDEX sy-index.

      READ TABLE t_numerator  INDEX sy-index.
      IF sy-subrc NE 0. CONTINUE. ENDIF.

      READ TABLE t_denominatr INDEX sy-index.
      IF sy-subrc NE 0. CONTINUE. ENDIF.

      CLEAR t_marm.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input    = t_altunit-value
          language = sy-langu
        IMPORTING
          output   = t_marm-alt_unit.

      t_marm-numerator     = t_numerator-value.
      t_marm-denominatr    = t_denominatr-value.
      APPEND t_marm.

      t_marmx-alt_unit     = t_marm-alt_unit.
      APPEND t_marmx.

    ENDDO.

  ENDIF.

** Dados EAN do material
*  t_mean-unit          = wa_datxls-base_uom.
*  append t_mean.
*  t_mean-unit          = wa_datxls-alt_unit.
*  append t_mean.
*
* Dados de controle do material
  t_mlan-depcountry    = 'BR'.
  t_mlan-tax_type_1    = 'IBRX'.
  t_mlan-taxclass_1    = wa_datxls-taxclass_1.
  APPEND t_mlan.

* Dados de controle QM material
* usada na BAPI_MATINSPCTRL_SAVEREPLICA p/ gerar tipo de controle
  t_inspectionctrl-function = '009'.
  t_inspectionctrl-insptype = wa_datxls-art.
  t_inspectionctrl-plant    = wa_datxls-plant.

  SELECT SINGLE * FROM tq34
                  INTO tq34
                 WHERE art EQ t_inspectionctrl-insptype.

  IF sy-subrc EQ 0.
    t_inspectionctrl-qual_score_procedure       = tq34-qkzverf.
    t_inspectionctrl-ind_insp_with_tsk_list     = tq34-ppl.
    t_inspectionctrl-ind_spec_matspec           = tq34-spezueber.
    t_inspectionctrl-ind_spec_config            = tq34-conf.
    t_inspectionctrl-ind_spec_batch             = tq34-tls.
    t_inspectionctrl-ind_auto_assign            = tq34-app.
    t_inspectionctrl-ind_insp_by_charac         = tq34-mer.
    t_inspectionctrl-ind_post_to_insp_stock     = tq34-insmk.
    t_inspectionctrl-ind_automatic_ud           = tq34-ave.
    t_inspectionctrl-sampling_procedure         = tq34-stichprver.
    t_inspectionctrl-dyn_modif_rule             = tq34-dynregel.
    t_inspectionctrl-insp_percentage            = tq34-sproz.
    t_inspectionctrl-ind_100_percent_inspection = tq34-hpz.
    t_inspectionctrl-ind_skips_allowed          = tq34-dyn.
    t_inspectionctrl-ind_manual_sample          = tq34-mpb.
    t_inspectionctrl-ind_manual_sample_calc     = tq34-mst.
    t_inspectionctrl-ind_single_units_possible  = tq34-ein.
    t_inspectionctrl-ave_insp_duration          = tq34-mpdau.
    t_inspectionctrl-contr_insp_lot_create      = tq34-chg.
    t_inspectionctrl-qual_score_procedure       = tq34-qkzverf.
    t_inspectionctrl-allowed_scrap_share        = tq34-qpmat.
    t_inspectionctrl-ind_insptype_mat_active    = 'X'.
  ENDIF.

* Dados do sistema de depósito

  i_mlgn-whse_no     = i_mlgnx-whse_no = wa_datxls-whse_no.
  i_mlgn-stgesector  = wa_datxls-stgesector.
  i_mlgn-placement   = wa_datxls-placement.
  i_mlgn-withdrawal  = wa_datxls-withdrawal.
  i_mlgn-l_equip_1   = wa_datxls-l_equip_1.
  i_mlgn-leq_unit_1  = wa_datxls-leq_unit_1.
  i_mlgn-unittype_1  = wa_datxls-unittype_1.
  PERFORM f_set_update_fields USING 'i_mlgnx'.

ENDFORM.                    " f_carrega_dados_bapi

*&---------------------------------------------------------------------*
*&      Form  f_maintain_material
*&---------------------------------------------------------------------*
FORM f_maintain_material.
* Verifica se o material já existe no cadastro através do ID antigo
  CLEAR: v_matold, e_return_get.
  CALL FUNCTION 'GET_MATERIAL_ID'
    EXPORTING
      i_oldmat              = i_bismt
    TABLES
      x_mara                = t_matid
    EXCEPTIONS
      too_many_input_params = 1
      ean_not_found         = 2
      oldmat_not_found      = 3
      OTHERS                = 4.

  IF sy-subrc EQ 0.
    READ TABLE t_matid INDEX 1.
    v_matold   = t_matid-matnr.
  ELSEIF sy-subrc NE 3.
    PERFORM f_imprime_erros  USING TEXT-003.
    EXIT.
  ENDIF.

* Seleciona o próximo número de material
* No caso de um material não ser gerado o nro é aproveitado no próximo
  IF v_matold IS INITIAL.

* Busca id material da tabela com IDs do primeiro client 400

    CALL FUNCTION 'BAPI_STDMATERIAL_GETINTNUMBER' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        material_type   = i_matl_type
        industry_sector = i_ind_sector
      IMPORTING
        return          = e_return_get
      TABLES
        material_number = t_material_number.
    IF e_return_get-type NE 'S'.
      PERFORM f_lista_erro USING sy-msgid sy-msgno sy-msgv1
                                 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ELSE.
      READ TABLE t_material_number INDEX 1.
      v_matold = t_material_number-material.
    ENDIF.

  ENDIF.


*---> 06/07/2023 - Migração S4 - MA
*  move v_matold:           to i_mathead-material,
*                           to t_inspectionctrl-material.

  DATA(v_len) = strlen( v_matold ).

  IF v_len > 18.
    MOVE v_matold:           TO i_mathead-material_long,
                             TO t_inspectionctrl-material_long.
  ELSE.
    MOVE v_matold:           TO i_mathead-material,
                             TO t_inspectionctrl-material.
  ENDIF.
*<--- 06/07/2023 - Migração S4 - MA
  APPEND t_inspectionctrl.

* Cria material com os dados carregados da planilha
  CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      headdata             = i_mathead
      clientdata           = i_mara
      clientdatax          = i_marax
      plantdata            = i_marc
      plantdatax           = i_marcx
      storagelocationdata  = i_mard
      storagelocationdatax = i_mardx
      valuationdata        = i_mbew
      valuationdatax       = i_mbewx
      warehousenumberdata  = i_mlgn
      warehousenumberdatax = i_mlgnx
      salesdata            = i_mvke
      salesdatax           = i_mvkex
      flag_online          = 'X'
    IMPORTING
      return               = e_return_save
    TABLES
      materialdescription  = t_makt
      unitsofmeasure       = t_marm
      unitsofmeasurex      = t_marmx
*     internationalartnos  = t_mean
      taxclassifications   = t_mlan
      returnmessages       = t_matreturn2.

  IF e_return_save-type EQ 'S'.
    ASSIGN icon_checked TO <icone>.
  ENDIF.
  PERFORM f_lista_erro USING e_return_save-id
                             e_return_save-number
                             e_return_save-message_v1
                             e_return_save-message_v2
                             e_return_save-message_v3
                             e_return_save-message_v4.

  CHECK e_return_save-type EQ 'S'.

  ASSIGN icon_failure TO <icone>.

* Criar os dados de contabilidade - Tipos de avaliação do material
  IF NOT p_accou IS INITIAL.
    LOOP AT t_avaliacao.
      i_mbewx-val_type = t_avaliacao-val_type.
      MOVE-CORRESPONDING t_avaliacao TO i_mbew.
      PERFORM f_set_update_fields USING 'i_mbewx'.

      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          headdata       = i_mathead
          valuationdata  = i_mbew
          valuationdatax = i_mbewx
        IMPORTING
          return         = e_return_save.

      IF e_return_save-type NE 'S'.
        PERFORM f_lista_erro USING e_return_save-id
                                   e_return_save-number
                                   e_return_save-message_v1
                                   e_return_save-message_v2
                                   e_return_save-message_v3
                                   e_return_save-message_v4.
      ENDIF.

    ENDLOOP.
  ENDIF.


* Criar todas as organizações de vendas do material
  IF p_sales EQ 'X'.
    LOOP AT t_salesorg.
      LOOP AT t_distrchan.

        IF ( i_mvke-sales_org  EQ t_salesorg-vkorg  ) AND
           ( i_mvke-distr_chan EQ t_distrchan-vtweg ).
          CONTINUE.
        ENDIF.
        i_mvke-sales_org  = i_mvkex-sales_org  = t_salesorg-vkorg.
        i_mvke-distr_chan = i_mvkex-distr_chan = t_distrchan-vtweg.

        SELECT SINGLE * FROM tvkov
                        INTO tvkov
                        WHERE vkorg EQ i_mvke-sales_org
                          AND vtweg EQ i_mvke-distr_chan.
        CHECK sy-subrc EQ 0.

        CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            headdata   = i_mathead
            salesdata  = i_mvke
            salesdatax = i_mvkex
          IMPORTING
            return     = e_return_save.

        IF e_return_save-type NE 'S'.
          PERFORM f_lista_erro USING e_return_save-id
                                     e_return_save-number
                                     e_return_save-message_v1
                                     e_return_save-message_v2
                                     e_return_save-message_v3
                                     e_return_save-message_v4.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDIF.

* Criar os dados do tipo de controle para qualidade.
  IF p_quali EQ 'X'.
    CALL FUNCTION 'BAPI_MATINSPCTRL_SAVEREPLICA' "#EC CI_USAGE_OK[2438131]
      TABLES
        return         = t_return2
        inspectionctrl = t_inspectionctrl.

    READ TABLE t_return2 INDEX 1.
    IF ( sy-subrc EQ 0 ) AND ( t_return2-type NE 'S' ).
      PERFORM f_lista_erro USING t_return2-id
                                 t_return2-number
                                 t_return2-message_v1
                                 t_return2-message_v2
                                 t_return2-message_v3
                                 t_return2-message_v4.
    ENDIF.
  ENDIF.
ENDFORM.                    " f_maintain_material
*&---------------------------------------------------------------------*
*&      Form  f_change_matcfop
*&---------------------------------------------------------------------*
FORM f_change_matcfop.
  CLEAR v_matold.
* Verifica se o material já existe no cadastro através do ID antigo
  CALL FUNCTION 'GET_MATERIAL_ID'
    EXPORTING
      i_oldmat              = i_bismt
    TABLES
      x_mara                = t_matid
    EXCEPTIONS
      too_many_input_params = 1
      ean_not_found         = 2
      oldmat_not_found      = 3
      OTHERS                = 4.

  IF sy-subrc EQ 0.
    READ TABLE t_matid INDEX 1.
    v_matold   = t_matid-matnr.
    v_mtart    = t_matid-mtart.
  ELSE.
    PERFORM f_imprime_erros  USING TEXT-006.
    EXIT.
  ENDIF.

* Criar determinação de CFOP para o material do arquivo.
  CLEAR t_avaliacao.
  PERFORM f_determinacao_cfop USING 'X'.
  LOOP AT t_avaliacao.
    PERFORM f_determinacao_cfop USING ' '.
  ENDLOOP.

ENDFORM.                    " f_change_matcfop

*&---------------------------------------------------------------------*
*&      Form  f_determinacao_cfop
*&---------------------------------------------------------------------*
FORM f_determinacao_cfop USING VALUE(p_marc_view).
  DATA v_mess_tab(256) TYPE c.

  CLEAR   : t_bdcdata, t_messtab.
  REFRESH : t_bdcdata, t_messtab.

  IF p_marc_view EQ 'X'.
    t_avaliacao-val_type   = i_mbew-val_type.
    t_avaliacao-mat_origin = i_mbew-mat_origin.
  ENDIF.
  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'     '0060',
                              ' ' 'BDC_OKCODE'  '=AUSW',
                              ' ' 'RMMG1-MATNR'  v_matold.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'     '0070',
                              ' ' 'BDC_OKCODE'   '=SELA'.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'     '0070',
                              ' ' 'BDC_OKCODE'   '=ENTR'.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'     '0080',
                              ' ' 'BDC_OKCODE'   '=ENTR',
                              ' ' 'RMMG1-WERKS'   i_mbew-val_area,
                              ' ' 'RMMG1-BWTAR'   t_avaliacao-val_type.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'      '4004'.

  IF NOT p_marc_view IS INITIAL.
*    if v_mtart eq 'UNBW'.
    PERFORM f_bdc_field USING: ' ' 'BDC_OKCODE' '=SP10',
                               'X' 'SAPLMGMM'   '4000'.
*    else.
*      perform f_bdc_field using: ' ' 'BDC_OKCODE' '=SP10',
*                                 'X' 'SAPLMGMM'   '4004'.
*    endif.
    PERFORM f_bdc_field USING: ' ' 'BDC_OKCODE'   '=SP25',
                               ' ' 'MARC-INDUS'   i_marc-mat_cfop.
  ELSE.
    PERFORM f_bdc_field USING: ' ' 'BDC_OKCODE'   '=SP25'.
  ENDIF.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'      '4000',
                              ' ' 'BDC_OKCODE'    '=BU',
                              ' ' 'MBEW-MTUSE'     i_mbew-matl_usage,
                              ' ' 'MBEW-MTORG'   t_avaliacao-mat_origin,
                              ' ' 'MBEW-OWNPR'     i_mbew-in_house.

*  perform f_bdc_field  using: 'X' 'SAPLSPO1'    '0300',
*                              ' ' 'BDC_OKCODE'  '=YES'.
*


  CALL TRANSACTION 'MM02' USING t_bdcdata
                           MODE p_mode
                         UPDATE 'S'
                  MESSAGES INTO t_messtab.

  LOOP AT t_messtab.
    sy-msgid = t_messtab-msgid.
    sy-msgno = t_messtab-msgnr.
    sy-msgv1 = t_messtab-msgv1.
    sy-msgv2 = t_messtab-msgv2.
    sy-msgv3 = t_messtab-msgv3.
    sy-msgv4 = t_messtab-msgv4.
    CALL FUNCTION 'CUTC_GET_MESSAGE'
      EXPORTING
        msg_id      = sy-msgid
        msg_no      = sy-msgno
        msg_arg1    = sy-msgv1
        msg_arg2    = sy-msgv2
        msg_arg3    = sy-msgv3
        msg_arg4    = sy-msgv4
        language    = sy-langu
      IMPORTING
        raw_message = v_mess_tab.
    PERFORM f_imprime_erros  USING v_mess_tab.
  ENDLOOP.

ENDFORM.                    " f_determinacao_cfop

*&---------------------------------------------------------------------*
*&      Form  f_lista_erro
*&---------------------------------------------------------------------*
FORM f_lista_erro USING    p_msgid LIKE sy-msgid
                           VALUE(p_msgno)
                           VALUE(p_msgv1)
                           VALUE(p_msgv2)
                           VALUE(p_msgv3)
                           VALUE(p_msgv4).

  CLEAR v_message.
  CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
    EXPORTING
      id         = p_msgid
      number     = p_msgno
      language   = sy-langu
      textformat = 'ASC'
      message_v1 = p_msgv1
      message_v2 = p_msgv2
      message_v3 = p_msgv3
      message_v4 = p_msgv4
    IMPORTING
      message    = v_message
      return     = v_bapiret2.
  IF sy-subrc NE 0.
    v_message = 'Ocorreu um erro que não pode ser exibido!!'.
  ELSE.
    IF v_message IS INITIAL.
      v_message = v_bapiret2-message.
    ENDIF.
  ENDIF.
  PERFORM f_imprime_erros  USING v_message.

ENDFORM.                    " f_lista_erro

*&---------------------------------------------------------------------*
*&      Form  f_bdc_field
*&---------------------------------------------------------------------*
FORM f_bdc_field USING    VALUE(p_flag)
                          VALUE(p_fnam)
                          VALUE(p_fval).
  CLEAR t_bdcdata.
  IF NOT p_flag IS INITIAL.
    t_bdcdata-program  = p_fnam.
    t_bdcdata-dynpro   = p_fval.
    t_bdcdata-dynbegin = 'X'.
  ELSE.
    t_bdcdata-fnam = p_fnam.
    t_bdcdata-fval = p_fval.
  ENDIF.
  APPEND t_bdcdata.

ENDFORM.                    "f_bdc_field
*&---------------------------------------------------------------------*
*&      Form  f_create_classificacao
*&---------------------------------------------------------------------*
FORM f_create_classificacao.
  DATA v_mess_tab(256) TYPE c.


  CHECK  'ZSIMZOTCZHOSZGENZMPNZMPI' CS i_mathead-matl_type.

  CLEAR v_matold.
* Verifica se o material já existe no cadastro através do ID antigo
  CALL FUNCTION 'GET_MATERIAL_ID'
    EXPORTING
      i_oldmat              = i_bismt
    TABLES
      x_mara                = t_matid
    EXCEPTIONS
      too_many_input_params = 1
      ean_not_found         = 2
      oldmat_not_found      = 3
      OTHERS                = 4.

  IF sy-subrc EQ 0.
    READ TABLE t_matid INDEX 1.
    v_matold   = t_matid-matnr.
  ELSE.
    PERFORM f_imprime_erros  USING TEXT-006.
    EXIT.
  ENDIF.

  CLEAR   : t_bdcdata, t_messtab.
  REFRESH : t_bdcdata, t_messtab.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'     '0060',
                              ' ' 'BDC_OKCODE'   '/00',
                              ' ' 'RMMG1-MATNR'  v_matold,
                              ' ' 'RMMG1-MBRSH'  i_mathead-ind_sector,
                              ' ' 'RMMG1-MTART'  i_mathead-matl_type.

  PERFORM f_bdc_field  USING: 'X' 'SAPLMGMM'     '0070',
                              ' ' 'BDC_OKCODE'   '=ENTR',
                              ' ' 'MSICHTAUSW-KZSEL(03)' 'X'.

  PERFORM f_bdc_field  USING: 'X' 'SAPLCLCA'     '0602',
                              ' ' 'BDC_OKCODE'   '=ENTE'.

  IF 'ZSIMZOTCZHOSZGEN' CS i_mathead-matl_type.
    PERFORM f_bdc_field USING ' ' 'RMCLF-KLART'  '001'.
  ELSE.
    PERFORM f_bdc_field USING ' ' 'RMCLF-KLART'  '023'.
  ENDIF.

  PERFORM f_bdc_field  USING: 'X' 'SAPLCLFM'        '0500',
                              ' ' 'BDC_OKCODE'      '=SAVE'.

  IF 'ZSIMZOTCZHOSZGEN' CS i_mathead-matl_type.
    PERFORM f_bdc_field USING ' ' 'RMCLF-CLASS(01)' 'Z_FICHA_PRODUTO'.
  ELSEIF 'ZMPNZMPI' CS i_mathead-matl_type.
    PERFORM f_bdc_field USING ' ' 'RMCLF-CLASS(01)' 'Z_TEOR_REAL_MP2'.
  ENDIF.

  CALL TRANSACTION 'MM01' USING t_bdcdata
*                           mode p_mode
                         UPDATE 'S'
                  MESSAGES INTO t_messtab.

  LOOP AT t_messtab.
    sy-msgid = t_messtab-msgid.
    sy-msgno = t_messtab-msgnr.
    sy-msgv1 = t_messtab-msgv1.
    sy-msgv2 = t_messtab-msgv2.
    sy-msgv3 = t_messtab-msgv3.
    sy-msgv4 = t_messtab-msgv4.
    CALL FUNCTION 'CUTC_GET_MESSAGE'
      EXPORTING
        msg_id      = sy-msgid
        msg_no      = sy-msgno
        msg_arg1    = sy-msgv1
        msg_arg2    = sy-msgv2
        msg_arg3    = sy-msgv3
        msg_arg4    = sy-msgv4
        language    = sy-langu
      IMPORTING
        raw_message = v_mess_tab.
    PERFORM f_imprime_erros  USING v_mess_tab.
  ENDLOOP.


ENDFORM.                    " f_create_classificacao
*&---------------------------------------------------------------------*
*&      Form  f_set_update_fields
*&---------------------------------------------------------------------*
FORM f_set_update_fields USING    VALUE(p_tab).

  STATICS: v_structure(10) TYPE c,
           v_field         TYPE feld-name,
           v_fieldx        TYPE feld-name,
           wa_fields       TYPE dfies,
           wa_fieldsx      TYPE dfies,
           p_tab2(08)      TYPE c.

  p_tab2 = p_tab(06).

  CASE p_tab.
    WHEN 'i_marax'. v_structure = 'BAPI_MARAX'.
    WHEN 'i_marcx'. v_structure = 'BAPI_MARCX'.
    WHEN 'i_mbewx'. v_structure = 'BAPI_MBEWX'.
    WHEN 'i_mvkex'. v_structure = 'BAPI_MVKEX'.
    WHEN 'i_mardx'. v_structure = 'BAPI_MARDX'.
    WHEN 'i_mlgnx'. v_structure = 'BAPI_MLGNX'.
    WHEN OTHERS. EXIT.
  ENDCASE.

  CALL FUNCTION 'CATSXT_GET_DDIC_FIELDINFO'
    EXPORTING
      im_structure_name = v_structure
    IMPORTING
      ex_ddic_info      = t_fieldsx.

  CALL FUNCTION 'CATSXT_GET_DDIC_FIELDINFO'
    EXPORTING
      im_structure_name = v_structure(9)
    IMPORTING
      ex_ddic_info      = t_fields.

  LOOP AT t_fieldsx INTO wa_fieldsx.
    CHECK wa_fieldsx-domname EQ 'BAPIUPDATE'.
    READ TABLE t_fields INTO wa_fields WITH KEY
                                 tabname   = wa_fieldsx-tabname(9)
                                 fieldname = wa_fieldsx-fieldname.
    CHECK sy-subrc EQ 0.
    CONCATENATE p_tab2 '-' wa_fields-fieldname  INTO v_field.
    ASSIGN (v_field)  TO <field>.
    CONCATENATE p_tab  '-' wa_fieldsx-fieldname INTO v_fieldx.
    ASSIGN (v_fieldx) TO <fieldx>.
    IF NOT <field> IS INITIAL.
      <fieldx> = 'X'.
    ELSE.
      <fieldx> = ' '.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " f_set_update_fields
