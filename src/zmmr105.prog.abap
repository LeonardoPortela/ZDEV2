*&---------------------------------------------------------------------*
*& Report  ZMMR105
*&
*&---------------------------------------------------------------------*
*& Monitor de: Recebimento de XML de CT-e, Pagamento, Escrituração Fiscal,
*& Complemento, Anulação, Substituição
*&---------------------------------------------------------------------*

REPORT  zmmr105 MESSAGE-ID zcte_distri.

************************************************************************
* SELECTION CRITERIA**                                                 *
************************************************************************

* Screen data transfer - tables declaration
TABLES: zib_cte_dist_ter, zib_cte_dist_n55, zib_cte_dist_c57, j_1bnfdoc, mara, t023.

SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.
  "Informações de Status de Documentos
  SELECTION-SCREEN BEGIN OF BLOCK cteetap WITH FRAME TITLE TEXT-003.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: ck_01 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 02(10) TEXT-008 FOR FIELD ck_01.
      PARAMETERS: ck_02 RADIOBUTTON GROUP rad1.
      SELECTION-SCREEN COMMENT 16(10) TEXT-009 FOR FIELD ck_02.
      PARAMETERS: ck_03 RADIOBUTTON GROUP rad1.
      SELECTION-SCREEN COMMENT 31(10) TEXT-010 FOR FIELD ck_03.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK cteetap.

* PARAMETERS: NF-e Data
  "Informações do Conhecimento de Transporte
  SELECTION-SCREEN BEGIN OF BLOCK ctedata WITH FRAME TITLE TEXT-001.
    SELECT-OPTIONS: pproc  FOR zib_cte_dist_ter-tp_processo_cte OBLIGATORY NO INTERVALS NO-EXTENSION,
                    docnum FOR zib_cte_dist_ter-docnum_cte,
                    numrct FOR zib_cte_dist_ter-numr_cte,
                    dtemit FOR zib_cte_dist_ter-dt_emissao, "DEFAULT SY-DATUM,
                    tomado FOR zib_cte_dist_ter-cd_tomador,
                    pmodal FOR zib_cte_dist_ter-cd_modal,
                    tservi FOR zib_cte_dist_ter-cd_tipo_servico,
                    tipcte FOR zib_cte_dist_ter-cd_tipo_cte,
                    ufinic FOR zib_cte_dist_ter-inicio_uf,
                    ufterm FOR zib_cte_dist_ter-termino_uf,
                    chavec FOR zib_cte_dist_ter-cd_chave_cte.
*PARAMETER: ck_canc AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK ctedata.

  "Informações de Status CT-e
  SELECTION-SCREEN BEGIN OF BLOCK stacte WITH FRAME TITLE TEXT-141.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: ck_auto RADIOBUTTON GROUP rad6 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 02(10) TEXT-142 FOR FIELD ck_auto.
      PARAMETERS: ck_canc RADIOBUTTON GROUP rad6.
      SELECTION-SCREEN COMMENT 16(10) TEXT-143 FOR FIELD ck_canc.
      PARAMETERS: ck_ambos RADIOBUTTON GROUP rad6.
      SELECTION-SCREEN COMMENT 31(10) TEXT-144 FOR FIELD ck_ambos.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK stacte.

  "Informações dos Parceiros
  SELECTION-SCREEN BEGIN OF BLOCK cteparc WITH FRAME TITLE TEXT-002.
    SELECT-OPTIONS: etomad FOR zib_cte_dist_ter-e_tomadora NO INTERVALS  OBLIGATORY MEMORY ID buk,
                    ftomad FOR zib_cte_dist_ter-f_tomadora,
                    ptomad FOR zib_cte_dist_ter-p_emissor,
                    emcnpj FOR zib_cte_dist_ter-emit_cnpj,
                    eemiss FOR zib_cte_dist_ter-e_emissor,
                    femiss FOR zib_cte_dist_ter-f_emissor,
                    recnpj FOR zib_cte_dist_ter-reme_cnpj,
                    decnpj FOR zib_cte_dist_ter-dest_cnpj.
  SELECTION-SCREEN END OF BLOCK cteparc.
SELECTION-SCREEN END OF SCREEN 1001.

SELECTION-SCREEN BEGIN OF SCREEN 1002 AS SUBSCREEN.
  "Informações de Documentos Acessórios
  SELECTION-SCREEN BEGIN OF BLOCK cteace WITH FRAME TITLE TEXT-004.
    SELECT-OPTIONS: achvnfe FOR zib_cte_dist_n55-n55_chave_acesso,
                    anurnfe FOR j_1bnfdoc-nfenum,
                    nrdoctr FOR zib_cte_dist_n55-tknum,
                    nrdocvi FOR zib_cte_dist_n55-fknum,
                    achvcte FOR zib_cte_dist_c57-c57_chave_acesso,
                    anurcte FOR j_1bnfdoc-nfenum,
                    codprod FOR mara-matnr NO INTERVALS NO-EXTENSION,
                    gruprod FOR t023-matkl NO INTERVALS NO-EXTENSION.
  SELECTION-SCREEN END OF BLOCK cteace.

  "Informações de Faturamento
  SELECTION-SCREEN BEGIN OF BLOCK ctefat WITH FRAME TITLE TEXT-007.
    SELECT-OPTIONS: dtvenci FOR zib_cte_dist_ter-zdt_vencto,
                    dtmovim FOR zib_cte_dist_ter-zdt_mov,
                    dtchega FOR zib_cte_dist_ter-dt_chegada.
  SELECTION-SCREEN END OF BLOCK ctefat.
SELECTION-SCREEN END OF SCREEN 1002.

SELECTION-SCREEN BEGIN OF SCREEN 1003 AS SUBSCREEN.

  "Informações de Status de Autorização de Pagamento (Grupo de Material)
  SELECTION-SCREEN BEGIN OF BLOCK cteauto WITH FRAME TITLE TEXT-011.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: ck_a1 RADIOBUTTON GROUP rad2 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 02(10) TEXT-008 FOR FIELD ck_a1.
      PARAMETERS: ck_a2 RADIOBUTTON GROUP rad2.
      SELECTION-SCREEN COMMENT 16(10) TEXT-012 FOR FIELD ck_a2.
      PARAMETERS: ck_a3 RADIOBUTTON GROUP rad2.
      SELECTION-SCREEN COMMENT 31(10) TEXT-013 FOR FIELD ck_a3.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK cteauto.

  "Informações de Chegada de Documentos
  SELECTION-SCREEN BEGIN OF BLOCK ctecheg WITH FRAME TITLE TEXT-138.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: cg_t1 RADIOBUTTON GROUP rad5 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 02(10) TEXT-008 FOR FIELD cg_t1.
      PARAMETERS: cg_t2 RADIOBUTTON GROUP rad5.
      SELECTION-SCREEN COMMENT 16(10) TEXT-013 FOR FIELD cg_t2.
      PARAMETERS: cg_t3 RADIOBUTTON GROUP rad5.
      SELECTION-SCREEN COMMENT 31(10) TEXT-139 FOR FIELD cg_t3.
      PARAMETERS: cg_t4 AS CHECKBOX.
      SELECTION-SCREEN COMMENT 46(15) TEXT-140 FOR FIELD cg_t4.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK ctecheg.

  "Informações de Liberação de Pagamento de Complemento
  SELECTION-SCREEN BEGIN OF BLOCK ctecple WITH FRAME TITLE TEXT-134.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: cc_t1 RADIOBUTTON GROUP rad4.
      SELECTION-SCREEN COMMENT 02(10) TEXT-008 FOR FIELD cc_t1.
      PARAMETERS: cc_t2 RADIOBUTTON GROUP rad4.
      SELECTION-SCREEN COMMENT 16(10) TEXT-013 FOR FIELD cc_t2.
      PARAMETERS: cc_t3 RADIOBUTTON GROUP rad4 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 31(10) TEXT-012 FOR FIELD cc_t3.
      PARAMETERS: cc_t4 RADIOBUTTON GROUP rad4.
      SELECTION-SCREEN COMMENT 46(13) TEXT-132 FOR FIELD cc_t4.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK ctecple.

  "Informações de Bloqueio de Pagamento
  SELECTION-SCREEN BEGIN OF BLOCK ctetrava WITH FRAME TITLE TEXT-131.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: ck_t1 RADIOBUTTON GROUP rad3.
      SELECTION-SCREEN COMMENT 02(10) TEXT-008 FOR FIELD ck_t1.
      PARAMETERS: ck_t2 RADIOBUTTON GROUP rad3 .
      SELECTION-SCREEN COMMENT 16(10) TEXT-132 FOR FIELD ck_t2.
      PARAMETERS: ck_t3 RADIOBUTTON GROUP rad3 DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 31(13) TEXT-133 FOR FIELD ck_t3.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK ctetrava.

SELECTION-SCREEN END OF SCREEN 1003.


SELECTION-SCREEN: BEGIN OF TABBED BLOCK mytab FOR 27 LINES,
TAB (40) btn1001 USER-COMMAND push1001,
TAB (40) btn1002 USER-COMMAND push1002,
TAB (40) btn1003 USER-COMMAND push1003,
END OF BLOCK mytab.

DATA:
  "GF_AUTHORIZATION_FT_01 TYPE C, "Autorizar Pagamento
  gf_authorization_ft_02 TYPE c, "Faturar
  gf_authorization_ft_03 TYPE c, "Visualizar CT-e
  gf_authorization_ft_04 TYPE c, "Reinicializar CT-e
  gf_authorization_ft_05 TYPE c, "Gerar Fatura (Prog.Pagamento)
  gf_authorization_ft_06 TYPE c, "Estornar Fatura (Prog.Pagamento)
  gf_authorization_ft_07 TYPE c, "Gerar VT e VI
  "GF_AUTHORIZATION_FT_08 TYPE C. "Liberar Diferênça de Peso
  gf_authorization_ft_09 TYPE c, "Workflow de Documentos
  gf_authorization_ft_10 TYPE c. "Atualizar Tabela de Custo


************************************************************************
* TYPE DEFINITION
************************************************************************

TYPE-POOLS: icon.


TYPES: BEGIN OF ty_info_forne.
TYPES: bvtyp TYPE bvtyp,    "Tipo de banco do parceiro
       texto TYPE char50, "Fornecedor
       bankl TYPE char03, "Banco
       banka TYPE banka,  "Nome do Banco
       bankn TYPE bankn,  "Conta Corrente
       agenc TYPE char15. "Agencia
TYPES: END OF ty_info_forne.

TYPES: BEGIN OF ty_add_nfe.
TYPES: cd_chave_cte	    TYPE zde_chave_doc_e,
       n55_chave_acesso	TYPE zde_chave_doc_e,
       docnum_nfe       TYPE j_1bdocnum,
       n55_stat_sefaz	  TYPE j_1bstatuscode,
       bukrs            TYPE bukrs,
       branch	          TYPE j_1bbranc_,
       parid            TYPE j_1bparid, "Ajuda de Pesquisa DEBI_KRED
       butxt            TYPE butxt,
       name	            TYPE name1,
       name1            TYPE name1_gp,
       nftot            TYPE j_1bnftot,
       ntgew            TYPE ntgew_15,
       ck_incluir       TYPE char01.
TYPES: END OF ty_add_nfe.

TYPES: BEGIN OF ty_add_nf.
TYPES: cd_chave_cte     TYPE zde_chave_doc_e,
       n01_modelo_nf    TYPE j_1bmodel,
       n01_numr_serie   TYPE j_1bseries,
       n01_nr_nf        TYPE j_1bnfnum9,
       n01_data_emissao TYPE j_1bdocdat,
       n01_vl_base_icms TYPE zde_vlr15_02,
       n01_vl_icms      TYPE zde_vlr15_02,
       n01_vl_bicms_st  TYPE zde_vlr15_02,
       n01_vl_icms_st   TYPE zde_vlr15_02,
       n01_vl_produtos  TYPE zde_vlr15_02,
       n01_vl_nota      TYPE zde_vlr15_02,
       n01_codg_cfop    TYPE zde_cfop,
       n01_vl_peso      TYPE zde_vlr15_02,
       n01_pin_suframa  TYPE j_1bnfnum9,
       ck_incluir       TYPE char01.
TYPES: END OF ty_add_nf.

************************************************************************
* DATA DEFINITION
************************************************************************
DATA: BEGIN OF wa_cte_alv,
        ck_chegada_doc     TYPE char01,
        status             TYPE char04,
        p_emissor_n        TYPE name1_gp,
        zvalor_ft_peso     TYPE zde_valor_frete_ton,
        zvalor_vi_peso     TYPE	zde_valor_vi_ton,
        cd_matnr_nota	     TYPE zde_matnr_nota,
        tx_prod_nota       TYPE zde_tx_prod_nota,
        tx_grupo_merc_nota TYPE zde_tx_gb_merc_nota,
        ic_viewlog         TYPE zde_icon_view_log,
        id_viagem          TYPE ZDE_VIAGEM_ID. "173890 CS2021000253 ZMM0079 Incluir coluna id_viagem PSA
        INCLUDE STRUCTURE zib_cte_dist_ter.
DATA: END OF wa_cte_alv.

DATA: BEGIN OF wa_log_alv,
        ic_message TYPE zde_icon_st,
        ic_texto   TYPE zde_icon_estrategia.
        INCLUDE STRUCTURE zib_cte_dist_log.
DATA: END OF wa_log_alv.


DATA: BEGIN OF wa_cte_n55,
        ic_editar TYPE char04,
        ic_dadosc TYPE char04.
        INCLUDE STRUCTURE zib_cte_dist_n55.
DATA: END OF wa_cte_n55.

DATA: BEGIN OF wa_cte_n01,
        ic_editar TYPE char04.
        INCLUDE STRUCTURE zib_cte_dist_n01.
DATA: END OF wa_cte_n01.

DATA: BEGIN OF wa_cte_c57.
        INCLUDE STRUCTURE zib_cte_dist_c57.
DATA: END OF wa_cte_c57.

DATA: BEGIN OF wa_cte_vei.
        INCLUDE STRUCTURE zib_cte_dist_vei.
DATA: END OF wa_cte_vei.

DATA: BEGIN OF wa_cte_mot.
        INCLUDE STRUCTURE zib_cte_dist_mot.
DATA: END OF wa_cte_mot.


DATA: BEGIN OF wa_tela_nxx,
        "Chave
        docnum           TYPE j_1bnfdoc-docnum,
        itmnum           TYPE j_1bnflin-itmnum,
        "Visualizar
        bukrs            TYPE j_1bnfdoc-bukrs,
        branch           TYPE j_1bnfdoc-branch,
        parid            TYPE j_1bnfdoc-parid,
        nfenum           TYPE j_1bnfdoc-nfenum,
        series           TYPE j_1bnfdoc-series,
        model            TYPE j_1bnfdoc-model,
        brgew            TYPE j_1bnfdoc-brgew,
        ntgew            TYPE j_1bnfdoc-ntgew,
        gewei            TYPE j_1bnfdoc-gewei,
        zmatnr_merc      TYPE matnr,
        pc_quebra	       TYPE zde_perc_quebra,
        pc_tolerancia	   TYPE zde_perc_tolerancia,
        "Editar
        zvlr_frete       TYPE zde_vlr_frete,
        zvlr_kg_transp   TYPE zde_vlr_kg_trans,
        zvlr_kg_mercad   TYPE zde_vlr_kg_merca,
        zvlr_mercadoria  TYPE zde_vlr_merc,
        peso_origem	     TYPE zde_peso_origem,
        peso_chegada     TYPE zde_peso_chegada,
        zpeso_diferenca	 TYPE zde_peso_dif,
        zquebra	         TYPE zde_quebra,
        zvlr_quebra	     TYPE zvlr_quebra,
        zperda           TYPE zde_perda,
        zvlr_perda       TYPE zvlr_perda,
        zvlr_liq_pagar   TYPE zvlr_liq_pagar,
        ck_peso_digitado TYPE char01.
DATA: END OF wa_tela_nxx.

DATA: ok_code  LIKE sy-ucomm.

DATA: ctl_alv_cte      TYPE REF TO cl_gui_alv_grid,
      "CTL_CCCONTAINER  TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
      ctl_cccontainer2 TYPE REF TO cl_gui_container,
      ctl_cccontainer3 TYPE REF TO cl_gui_container,
      dg_splitter      TYPE REF TO cl_gui_splitter_container.
DATA: ctl_alv_cte_hist TYPE REF TO cl_gui_alv_grid.

DATA: gs_layout   TYPE lvc_s_layo,
      gs_variant  TYPE disvariant,
      gs_variant2 TYPE disvariant,
      gs_layout2  TYPE lvc_s_layo.

DATA: gs_scroll_col TYPE lvc_s_col,
      gs_scroll_row TYPE lvc_s_roid.

DATA: gs_scroll_col2 TYPE lvc_s_col,
      gs_scroll_row2 TYPE lvc_s_roid.

DATA: obj_cte TYPE REF TO zcl_cte_dist_g.
*&--------------------------------------------------------------------&*
*& Classes Locais                                                     &*
*&--------------------------------------------------------------------&*

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_event_handler_log DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

DATA: event_handler      TYPE REF TO lcl_event_handler.

DATA: event_handler_log  TYPE REF TO lcl_event_handler_log.

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION

CLASS lcl_event_handler_log IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click_log USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_event_handler IMPLEMENTATION


TYPES: BEGIN OF ty_bloq.
TYPES:   cd_chave_cte TYPE zde_chave_doc_e.
TYPES: END OF ty_bloq.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

*---------- Internal tables (and related work areas)-------------------*

DATA: manager        TYPE REF TO cl_gos_manager.

DATA: it_cte_dist    TYPE TABLE OF zib_cte_dist_ter WITH HEADER LINE,
      wa_cte_dist    TYPE zib_cte_dist_ter,
      it_cte_alv     LIKE TABLE OF wa_cte_alv WITH KEY cd_chave_cte,
      it_log_alv     LIKE TABLE OF wa_log_alv WITH KEY cd_chave_cte dt_atualizacao hr_atualizacao nr_sequencia,
      it_cte_select  TYPE TABLE OF zib_cte_dist_ter WITH HEADER LINE,
      it_cte_selectw TYPE TABLE OF zib_cte_dist_ter WITH HEADER LINE,
      it_chaves_bloq TYPE TABLE OF ty_bloq WITH HEADER LINE,
      wa_cte_select  TYPE zib_cte_dist_ter,
      it_cte_n55     LIKE TABLE OF wa_cte_n55,
      it_cte_n55_sel LIKE TABLE OF wa_cte_n55,
      it_cte_n01     LIKE TABLE OF wa_cte_n01,
      it_cte_n01_sel LIKE TABLE OF wa_cte_n01,
      it_cte_c57     LIKE TABLE OF wa_cte_c57,
      it_cte_vei     LIKE TABLE OF wa_cte_vei,
      it_cte_mot     LIKE TABLE OF wa_cte_mot,
      wa_cte_nit     TYPE zib_cte_dist_nit,
      it_cte_nit     LIKE TABLE OF wa_cte_nit,
      it_cte_cvl     TYPE TABLE OF zib_cte_dist_cvl WITH HEADER LINE,
      it_cte_vga     TYPE TABLE OF zib_cte_dist_vga WITH HEADER LINE,
      it_cte_d55     TYPE TABLE OF zib_cte_dist_d55 WITH HEADER LINE,
      it_cte_d01     TYPE TABLE OF zib_cte_dist_d01 WITH HEADER LINE,
      it_cte_d55_sel TYPE TABLE OF zib_cte_dist_d55 WITH HEADER LINE,
      it_cte_d01_sel TYPE TABLE OF zib_cte_dist_d01 WITH HEADER LINE,
      it_cte_dup     TYPE TABLE OF zib_cte_dist_dup WITH HEADER LINE,
      it_cte_cpl     TYPE TABLE OF zib_cte_dist_cpl WITH HEADER LINE,
      wa_info_forne  TYPE ty_info_forne,
      it_doc_eap     TYPE TABLE OF zib_cte_dist_eap WITH HEADER LINE.


DATA: it_selected_rows TYPE lvc_t_row,
      owner            TYPE soud-usrnam,
      sofd_dat         TYPE sofdd,
      wa_selected_rows TYPE lvc_s_row.

DATA: it_exclude_fcode TYPE ui_functions,
      wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: estrutura TYPE TABLE OF ty_estrutura,
      v_report  LIKE          sy-repid.

DATA: it_fieldcatalog TYPE lvc_t_fcat,
      wa_fieldcatalog TYPE lvc_s_fcat.
DATA: it_fieldcatalog2 TYPE lvc_t_fcat,
      wa_fieldcatalog2 TYPE lvc_s_fcat.

DATA: jobname  TYPE btcjob,
      jobcount TYPE btcstepcnt.

INITIALIZATION.

  "135  Conhecimento de Transporte
  "136  Documentos Acessórios
  "137  Status de Documento

  btn1001 = TEXT-135.
  btn1002 = TEXT-136.
  btn1003 = TEXT-137.
  mytab-prog = sy-repid.
  mytab-dynnr = 1001.
  mytab-activetab = 'PUSH1001'.
  CLEAR: jobname, jobcount.
  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""


  IF sy-batch EQ abap_true.
    TRY .


        CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
          IMPORTING
            jobname         = jobname
            stepcount       = jobcount
          EXCEPTIONS
            no_runtime_info = 1
            OTHERS          = 2.
      CATCH cx_sy_dyn_call_illegal_type.

    ENDTRY.

    IF jobname NE 'ZMM0079_FRETE_FERROVIARIO'.
      TRY .
          zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
        CATCH zcx_job.
          e_qtd = 1.
      ENDTRY.

      IF e_qtd GT 1.
        LEAVE PROGRAM.
      ENDIF.

      CALL METHOD zcl_cte_dist_g=>atualiza_cte.
      CALL METHOD zcl_cte_dist_g=>gerar_pagamento_automatico.
      LEAVE PROGRAM.
    ENDIF.




  ENDIF.
  """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

  "GF_AUTHORIZATION_FT_01 = ABAP_FALSE.
  gf_authorization_ft_02 = abap_false.
  gf_authorization_ft_03 = abap_false.
  gf_authorization_ft_04 = abap_false.
  gf_authorization_ft_05 = abap_false.
  gf_authorization_ft_06 = abap_false.
  gf_authorization_ft_07 = abap_false.
  "GF_AUTHORIZATION_FT_08 = ABAP_FALSE.
  gf_authorization_ft_09 = abap_false.
  gf_authorization_ft_10 = abap_false.

*  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '01'.
*  IF SY-SUBRC IS INITIAL.
*    GF_AUTHORIZATION_FT_01 = ABAP_TRUE.
*  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '02'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_02 = abap_true.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '03'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_03 = abap_true.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '04'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_04 = abap_true.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '05'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_05 = abap_true.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '06'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_06 = abap_true.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '07'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_07 = abap_true.
  ENDIF.

*  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '08'.
*  IF SY-SUBRC IS INITIAL.
*    GF_AUTHORIZATION_FT_08 = ABAP_TRUE.
*  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '09'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_09 = abap_true.
  ENDIF.

  AUTHORITY-CHECK OBJECT 'ZACTFTTER' ID 'ZACTFTTER' FIELD '10'.
  IF sy-subrc IS INITIAL.
    gf_authorization_ft_10 = abap_true.
  ENDIF.

AT SELECTION-SCREEN.


  CASE sy-dynnr.
    WHEN 1000.
      CASE sy-ucomm.
        WHEN 'PUSH1001'.
          mytab-dynnr = 1001.
        WHEN 'PUSH1002'.
          mytab-dynnr = 1002.
        WHEN 'PUSH1003'.
          mytab-dynnr = 1003.
      ENDCASE.
  ENDCASE.

START-OF-SELECTION.

  IF obj_cte IS INITIAL.
    CREATE OBJECT obj_cte.
  ENDIF.

  PERFORM pesquisar_pagamentos.

END-OF-SELECTION.

  IF sy-batch EQ abap_true.
    PERFORM f_imprimir_dados. " U.S #61236 ->Adicionado uma ALV simples para retornar informações quando executado pelo JOB em Backgroud. / AOENNING.
  ELSE.
    CALL SCREEN '0100'.
  ENDIF.

***********************************************************************
* PBO Modules
***********************************************************************

******************** S C R E E N  0100 ********************************

  INCLUDE zmmr105_0100.

  INCLUDE zmmr105_0103.

  INCLUDE zmmr105_0104.

  INCLUDE zmmr105_0105.

  INCLUDE zmmr105_0201.

  INCLUDE zmmr105_0301.

  INCLUDE zmmr105_0106.

  INCLUDE zmmr105_0303.

  INCLUDE zmmr105_0401.

  INCLUDE zmmr105_0107.

  INCLUDE zmmr105_0108.

  INCLUDE zmmr105_0109.

  INCLUDE zmmr105_0110.

  INCLUDE zmmr105_0111.

  INCLUDE zmmr105_0112.

  INCLUDE zmmr105_0501.

  INCLUDE zmmr105_0113.

  INCLUDE zmmr105_0114.

  INCLUDE zmmr105_0119.

  INCLUDE zmmr105_0120.

  INCLUDE zmmr105_0304.

  INCLUDE zmmr105_0502.
