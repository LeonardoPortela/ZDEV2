*======================================================================*
* PROJETO            : HCM                                             *
* PROGRAMA           : ZHCMR_PA0014                                    *
* TRANSACAO          : ZHCM_PA0009                                     *
* DESCRICAO          : Relatório Funcionários Ativos                   *
*======================================================================*
* AUTOR              : Camila Brand                                    *
* Solicitante        : Jadeilson Ferreira                              *
* DATA               : 02/05/2017                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
REPORT zhcmr_pa0109.

***********************************************
***********************************************
* Initialization
***********************************************
INITIALIZATION.
***********************************************
* Start of selection
***********************************************

*======================================================================*
*** Nodes BDL
*======================================================================*
  NODES: peras.

*======================================================================*
*** Tabelas
*======================================================================*
  TABLES: pernr, pa0465, pa0001.



*======================================================================*
* Types
*======================================================================*
  TYPES: BEGIN OF ty_saida,

           bukrs           TYPE p0001-bukrs,        "Empresa
           bukrs_text      TYPE butxt,              "Descrição Empresa
           werks           TYPE p0001-werks,        "Area RH
           werks_text      TYPE t500p-name1,        "Descrição Area RH
           cgc_nr(18)      TYPE c,                  "CGC FILIAL
           kostl           TYPE p0001-kostl,        "Centro cst
           cdesc           TYPE char20,             "Descrição C. Custo
           pernr           TYPE p0000-pernr,        "Nº pess.
           cname           TYPE p0002-cname,        "Nome do empregado ou candidato
           situacao(40)    TYPE c,                  "SITUAÇÃO "Responder como Ativo, Férias, Aposentado, etc.
           begda           TYPE p0000-begda,        "Vál.desde
           stell           TYPE p0001-stell,        "Cargo
           uniorg          TYPE hrp1000-stext,      "Descricao Cargo
*           cbo_nr             TYPE t7brcb-cbo,         "CBO - COMENTADO US 103380 - SCABANA
*           cbode              TYPE t7br20-cbode,       "Desc CBO - COMENTADO US 103380 - SCABANA
           ptext           TYPE t503t-ptext,                "Desc Grupo Empregado
           setrisc         TYPE hrp9665-setrisc,            "Setor
           setrisc_txt     TYPE zhrst_setor_risc-setrisc_txt, "Cód. do Setor
           plans           TYPE p0001-plans,        "Posição
           orgeh           TYPE p0001-orgeh,        "UnidOrg
           orgtx           TYPE hrp1000-stext, "T527X-ORGTX,        "UnidOrg Desc
           diretoria       TYPE hrp1000-stext,
           uniorg_gerencia TYPE hrp1000-stext,
           bet01           TYPE p0008-bet01,        "Salário Atual
*           salfop             TYPE p0008-bet01,        "Adicional fora de operação COMENTADO US 103380 - SCABANA
*           percc              TYPE p0008-bet01,        "Periculosidade COMENTADO US 103380 - SCABANA
*           insalub            TYPE p0008-bet01,        "Insalubridade COMENTADO US 103380 - SCABANA
*           dar05(8)           TYPE c,                  "Adicional tempo de casa COMENTADO US 103380 - SCABANA
*           valtc              TYPE p0008-bet01,        "Valor adicional tempo de casa COMENTADO US 103380 - SCABANA
*           trfgr              TYPE p1005-trfg1, "GS" - COMENTADO US 103380 - SCABANA
*           trfst              TYPE p1005-trfs1,  "Alvo" COMENTADO US 103380 - SCABANA
*           mostd              TYPE pa0007-mostd,       "Carga Horária IT0007 COMENTADO US 103380 - SCABANA
*           divgv              TYPE p0008-divgv,        "Carga Horária IT0008 COMENTADO US 103380 - SCABANA
*           ztext              TYPE q0008-ztext,        "Desc. Carga Horaria IT0008 COMENTADO US 103380 - SCABANA
*           cttyp(20)          TYPE c,                  "Tipo Contrato COMENTADO US 103380 - SCABANA
*           ctedt              TYPE p0016-ctedt,        "FIM  Contrato COMENTADO US 103380 - SCABANA
           vincemp(20)     TYPE c,                  "Vinculo Empregaticio
*           estab(40)          TYPE c,                  "Estabilidade PCD COMENTADO US 103380 - SCABANA
*           estab_begda        TYPE sy-datum,           "Início E. PCD COMENTADO US 103380 - SCABANA
*           estab_endda        TYPE sy-datum,           "Fim E. PCD COMENTADO US 103380 - SCABANA
*           estab_cipa(40)     TYPE c,                  "Estabilidade CIPA COMENTADO US 103380 - SCABANA
*           estab_cipa_begda   TYPE sy-datum,           "Início E. CIPA COMENTADO US 103380 - SCABANA
*           estab_cipa_endda   TYPE sy-datum,           "Fim E. CIPA COMENTADO US 103380 - SCABANA
*           pcd(20)            TYPE c,                  "PCD Responder com tipo de PCS, visual, fisico, etc. COMENTADO US 103380 - SCABANA
           endda           TYPE p0000-endda,        "Válido até
           motdesl(40)     TYPE c,                  "Motivo Desligamento
*           infdem(40)         TYPE c,                  "Informação demissional COMENTADO US 103380 - SCABANA
*           stat_tmp(40)       TYPE c,                  "Bate ou não bate ponto  - ZTERF COMENTADO US 103380 - SCABANA
*           mofid              TYPE t001p-mofid,        "Calendário de feriados COMENTADO US 103380 - SCABANA
*           sindcat(40)        TYPE c,                  "Sindicato COMENTADO US 103380 - SCABANA
*           locatu(50)         TYPE c,                  "Local de atuação COMENTADO US 103380 - SCABANA
           abkrs           TYPE p0001-abkrs,        "ÁrPFPg
           abkrs_text      TYPE abktx,              "Texto área processamento da folha pagamento
*           per_exp01          TYPE pa0041-dat03,        "1 PER EXPERIENCIA COMENTADO US 103380 - SCABANA
*           per_exp02          TYPE pa0041-dat04,       "2 PER EXPERIENCIA COMENTADO US 103380 - SCABANA
*           val_alim           TYPE zhcmt_bn_0001-va_p_mvra,       "VALE ALIMENTAÇÃO COMENTADO US 103380 - SCABANA
*           val_refe           TYPE zhcmt_bn_0001-vr_p_mvr0, "     "VaLE REFEIÇÃO COMENTADO US 103380 - SCABANA
*           boptim             TYPE q0167-eecst,        "ASSITENCIA MEDICA COMENTADO US 103380 - SCABANA
*           depcvm             TYPE pa0167-depcv,       "ASSITENCIA MEDICA COMENTADO US 103380 - SCABANA
*           boptio             TYPE q0167-eecst,        "ASSISTENCIA ODONT COMENTADO US 103380 - SCABANA
*           depcvo             TYPE pa0167-depcv,       "ASSISTENCIA ODONT COMENTADO US 103380 - SCABANA
*           nhcnr           TYPE pa0397-nhcnr,       "Cartão Nacional de Saúde COMENTADO US 103380 - SCABANA
*           eepct              TYPE pa0169-eepct,       "PREV PRIVADA COMENTADO US 103380 - SCABANA
*           zins_vol           TYPE pa0169-zins_vol ,   "PREV VOLUNTARIA COMENTADO US 103380 - SCABANA
*           eecst              TYPE t5ubi-eecst,        "Seguro de Vida COMENTADO US 103380 - SCABANA
*PBI 59878 - 28.07.2021 - JT - inicio
           segvida         TYPE char1,              "tem Seguro de Vida?
*PBI 59878 - 28.07.2021 - JT - fim
           cpf_nr          TYPE p0465-cpf_nr,       "CPF
           email           TYPE pa0105-usrid_long,  "E-MAIL
           gbdat(10)       TYPE c,                  "DATA NASCIMENTO
           locnasc(50)     TYPE c,                  "Local de Nascimento
*           racacor(10)     TYPE c,                  "RACA/COR COMENTADO US 103380 - SCABANA
*           fcnam              TYPE p0021-fcnam,        "Nome da mãe. COMENTADO US 103380 - SCABANA
*           pis_nr          TYPE p0465-pis_nr,       "PIS COMENTADO US 103380 - SCABANA
*           ident_nr        TYPE p0465-ident_nr,     "NUMERO RG COMENTADO US 103380 - SCABANA
*           dt_emis         TYPE p0465-dt_emis,      "DATA EMISSÃO RG
*           rgorg           TYPE p0465-doc_issuer,   "ORGÃO RG
*           es_emis         TYPE p0465-es_emis,      "UF RG
*           ctps_nr         TYPE p0465-ctps_nr,      "NUMERO CTPS COMENTADO US 103380 - SCABANA
*           ctps_serie      TYPE p0465-ctps_serie,   "NUMERO CTPS COMENTADO US 103380 - SCABANA
*           dt_emis_ctps    TYPE p0465-dt_emis,      "DATA EMISSÃO CTPS COMENTADO US 103380 - SCABANA
*           emis_ctps       TYPE p0465-es_emis,      "UF CTPS COMENTADO US 103380 - SCABANA
*           drive_nr        TYPE p0465-drive_nr,     "Nr. Cart. Habilit. COMENTADO US 103380 - SCABANA
*           cnhorg          TYPE p0465-doc_issuer,   "Órgão emissor CNH COMENTADO US 103380 - SCABANA
*           drive_cat       TYPE p0465-drive_cat,    "Categoria Habilit. COMENTADO US 103380 - SCABANA
*           dt_emis_dr      TYPE p0465-dt_emis,      "Data Emissão COMENTADO US 103380 - SCABANA
*           es_emis_dr      TYPE p0465-es_emis,      "UF de Expedição COMENTADO US 103380 - SCABANA
*           endda_dr        TYPE p0465-endda,        "Data Vencto Habilitação COMENTADO US 103380 - SCABANA
*           mil_nr             TYPE p0465-mil_nr,       "N. Reservista  --- 0007 COMENTADO US 103380 - SCABANA
*           elec_nr         TYPE p0465-elec_nr,      "Nr. Título Eleitor   --0005 COMENTADO US 103380 - SCABANA
*           elec_zone       TYPE p0465-elec_zone,    "Zona Tít. Eleitor COMENTADO US 103380 - SCABANA
*           elec_sect       TYPE p0465-elec_sect,    "Seção Eleitoral COMENTADO US 103380 - SCABANA
*           dt_emis_elec    TYPE p0465-dt_emis,      "Data Emissão COMENTADO US 103380 - SCABANA
*           es_emis_elec    TYPE p0465-es_emis,      "UF Título Eleitor COMENTADO US 103380 - SCABANA
           gesch           TYPE p0002-gesch,        "SEXO
           estcv(20)       TYPE c,                  "ESTADO CIVIL
           escol(20)       TYPE c,                  "Ecolariedade
           stras           TYPE p0006-stras,        "RUA
           hsnmr           TYPE p0006-hsnmr,        "NUMERO
           posta           TYPE p0006-posta,        "COMPLEMENTO
           ort02           TYPE p0006-ort02,        "BAIRRO
           ort01           TYPE p0006-ort01,        "CIDADE
           state           TYPE p0006-state,        "ESTADO
           pstlz           TYPE p0006-pstlz,        "CEP
*-CS2021000425 - 07.05.2021 - JT - inicio
           stras_emp       TYPE p0006-stras,        "RUA
           hsnmr_emp       TYPE p0006-hsnmr,        "NUMERO
           posta_emp       TYPE p0006-posta,        "COMPLEMENTO
           ort02_emp       TYPE p0006-ort02,        "BAIRRO
           ort01_emp       TYPE p0006-ort01,        "CIDADE
           state_emp       TYPE p0006-state,        "ESTADO
           pstlz_emp       TYPE p0006-pstlz,        "CEP
*-CS2021000425 - 07.05.2021 - JT - inicio
           volma(40)       TYPE c,                  "Ultimo estado de residencia.
           num01           TYPE p0006-num01,        "TELEFONE
*           chvbco          TYPE p0009-bankl,        "Chave do Banco COMENTADO US 103380 - SCABANA
*** Inicio - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*           bgrup           TYPE bnka-bgrup,         "Grupo de Bancos COMENTADO US 103380 - SCABANA
*** Fim    - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*           ccbco           TYPE p0009-bankn,        "Conta COMENTADO US 103380 - SCABANA
           gestimed(80)    TYPE c,                  "Gestor Imediato
           email_imed      TYPE p0105-usrid_long,   "Email Gestor Imediato - PBI - 68737
           gestmed(80)     TYPE c,                  "Gestor Mediato
           email_med       TYPE p0105-usrid_long,   "Email Gestor Mediato - PBI - 68737
*           fgtsd              TYPE p0398-fgtsd ,       "DATA DE OPÇÃO DO FGTS - COMENTADO US 103380 - SCABANA
*           cattr(40)          TYPE c ,                 "CATEGORIA DO TRABALHADOR - COMENTADO US 103380 - SCABANA
*           notice_partial     TYPE  p0661-notice_partial, " Av.P. eSocial COMENTADO US 103380 - SCABANA
*           emfsl              TYPE p0057-emfsl,        "Cód. Sindicato COMENTADO US 103380 - SCABANA
*           endda_sd           TYPE p0057-endda,        "Validade COMENTADO US 103380 - SCABANA
*           setrisc            TYPE hrp9665-setrisc,    "Cód. Setor de Risco - COMENTADO US 103380 - SCABANA
*           setor_risco        TYPE hrp9665-stext,      "Setor de Risco - COMENTADO US 103380 - SCABANA
           aufnr           TYPE pa0027-auf01,       "Nº ordem -
           ktext           TYPE aufk-ktext,         "Desc. Ordem -
           uname           TYPE p0001-uname,        "User Admissão
*           GSBER        TYPE P0001-GSBER,         "Divisão
*           PERSK        TYPE P0001-PERSK,         "SgEmp
*           doc_nr             TYPE pa0465-doc_nr,       "Mat.E-social COMENTADO US 103380 - SCABANA
           tp_casa(6)      TYPE c, "char6, "Tempo de Casa  "80255
           idade(6)        TYPE c.  "char6 . "idade"80255

*           estab_dir_sind(40) TYPE c,            "Estabilidade Dirigente Sindical - COMENTADO US 103380 - SCABANA
*           estab_begda_ds     TYPE sy-datum,     "Início Estabilidade Dirigente Sindical - COMENTADO US 103380 - SCABANA
*           estab_endda_ds     TYPE sy-datum,     "Fim Estabilidade Dirigente Sindical - COMENTADO US 103380 - SCABANA
*           estab_vesp_ap(40)  TYPE c,            "Estabilidade Vespera Aposent - COMENTADO US 103380 - SCABANA
*           estab_begda_va     TYPE sy-datum,     "Início Estabilidade Vespera Aposent - COMENTADO US 103380 - SCABANA
*           estab_endda_va     TYPE sy-datum.     "Fim Estabilidade Vespera Aposent - COMENTADO US 103380 - SCABANA
*           estab_actrab(40)   TYPE c,            "Estabilidade Acidente de Trabalho - 12 meses - COMENTADO US 103380 - SCABANA
*           estab_begda_at     TYPE sy-datum,     "Início Acidente de Trabalho - 12 meses - COMENTADO US 103380 - SCABANA
*           estab_endda_at     TYPE sy-datum.     "Fim Acidente de Trabalho - 12 meses - COMENTADO US 103380 - SCABANA
*
  TYPES:             END OF ty_saida.

*======================================================================*
*** Data * Workarea
*======================================================================*
  DATA: it_saida     TYPE STANDARD TABLE OF ty_saida,
        wa_saida     TYPE ty_saida,
        it_saida_aux TYPE STANDARD TABLE OF ty_saida,
        wa_saida_aux TYPE ty_saida,
        wa_0000      TYPE p0000,
        wa_0001      TYPE p0001,
        wa_0002      TYPE p0002,
        wa_0004      TYPE p0004,
        wa_0006      TYPE p0006,
        wa_0006_emp  TYPE p0006,
        wa_0007      TYPE p0007,
        wa_0008      TYPE p0008,
        wa_0009      TYPE p0009,
        wa_0016      TYPE p0016,
        wa_0021      TYPE p0021,
        wa_0027      TYPE p0027,
        wa_0030      TYPE p0030,
        wa_0040      TYPE p0040,
        wa_0041      TYPE p0041,
        wa_0057      TYPE p0057,
        wa_0167      TYPE p0167,
        wa_0168      TYPE p0168,
        wa_0169      TYPE p0169,
        wa_0105      TYPE p0105,
        wa_0398      TYPE p0398,
        wa_0465      TYPE p0465,
        wa_0598      TYPE p0598,
        wa_0625      TYPE p0625,
        wa_0661      TYPE p0661,
        wa_9003      TYPE p9003,
        wa_9004      TYPE p9004,
        wa_p0465     TYPE p0465,
        it_gest      LIKE zhcms_ret_sup9002 OCCURS 0,
        wa_gest      LIKE LINE OF it_gest,
        it_1005      TYPE TABLE OF hrp1005,
        wa_1005      LIKE LINE OF it_1005.

  DATA: it_absence  LIKE bapipakey OCCURS 0,
        wa_absence  LIKE LINE OF it_absence,
        return      LIKE bapireturn1,
        v_data      TYPE sy-datum,
        v_begda     TYPE sy-datum,
        v_endda     TYPE sy-datum,
        v_begda_aux TYPE begda,
        v_endda_aux TYPE endda,
        it_p0465    TYPE TABLE OF p0465.

  RANGES: rg_afast FOR pa2001-subty.

  DATA: it_setleaf LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
        wa_setleaf TYPE setleaf.

  DATA: wa_branch TYPE bapibranch-branch,
        cgc_nr    LIKE bapibranch-cgc_number,
        comp_name LIKE bapibranch-name.

  DATA ls_py_0001 TYPE zhcmt_py_0001.

  DATA: lt_bn_0001 TYPE TABLE OF zhcmt_bn_0001,
        l_0001     LIKE LINE OF lt_bn_0001.

  DATA: it_t5ubh TYPE TABLE OF t5ubh,
        it_t5ubi TYPE TABLE OF t5ubi,
        wa_t5ubh LIKE LINE  OF it_t5ubh,
        wa_t5ubi LIKE LINE  OF it_t5ubi.

  DATA: subrc        TYPE sysubrc,
        error_table  LIKE rpbenerr OCCURS 0,
        health_plans LIKE rpben_da OCCURS 5 WITH HEADER LINE,
        save_plans   LIKE rpben_dc OCCURS 5 WITH HEADER LINE.

  CONSTANTS: no_msg TYPE sy-msgty VALUE 'N',
             true   TYPE boolean VALUE 'X',
             false  TYPE boolean VALUE ' '.


  DATA:   v_qnt  TYPE i.


  DATA: text_tab_local    TYPE hrpad_text_tab,
        wa_text_tab_local LIKE LINE OF text_tab_local,
        message_handler   TYPE REF TO  if_hrpa_message_handler,
        no_auth_check     TYPE  boole_d,
        is_ok             TYPE  boole_d.

  DATA : pskey TYPE pskey.

  DATA : text_tab    TYPE hrpad_text_tab,
         wa_text_tab LIKE LINE OF text_tab.

  DATA: lva_dt_admis   TYPE sy-datum,
        lva_data_saida TYPE sy-datum.


*&---------------------------------------------------------------------*
*&      TABLES REPORT
*&---------------------------------------------------------------------*
  DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
        dg_splitter_1      TYPE REF TO cl_gui_splitter_container,
        dg_parent_1        TYPE REF TO cl_gui_container,
        dg_splitter_2      TYPE REF TO cl_gui_splitter_container,
        dg_parent_2        TYPE REF TO cl_gui_container,
        dg_parent_2a       TYPE REF TO cl_gui_container,
        dg_parent_alv      TYPE REF TO cl_gui_container,
        picture            TYPE REF TO cl_gui_picture,
        gs_layout          TYPE lvc_s_layo,
        gs_variant         TYPE disvariant,
        ctl_alv            TYPE REF TO cl_gui_alv_grid,
        it_fieldcatalog    TYPE lvc_t_fcat,
        wa_fieldcatalog    TYPE lvc_s_fcat,
        gs_scroll_col      TYPE lvc_s_col,
        gs_scroll_row      TYPE lvc_s_roid,
        it_exclude_fcode   TYPE ui_functions,
        wa_exclude_fcode   LIKE LINE OF it_exclude_fcode,
        dg_dyndoc_id       TYPE REF TO cl_dd_document,
        table_element      TYPE REF TO cl_dd_table_element,
        column             TYPE REF TO cl_dd_area,
        table_element2     TYPE REF TO cl_dd_table_element,
        column_1           TYPE REF TO cl_dd_area,
        column_2           TYPE REF TO cl_dd_area,
        dg_html_cntrl      TYPE REF TO cl_gui_html_viewer.


  DATA: lo_sap_hcm TYPE REF TO zcl_hcm_util.                "2000016066

*======================================================================*
*** Infotipos
*======================================================================*
  INFOTYPES: 0000 NAME p0000.
  INFOTYPES: 0001 NAME p0001.
  INFOTYPES: 0002 NAME p0002.
  INFOTYPES: 0004 NAME p0004.
  INFOTYPES: 0006 NAME p0006.
  INFOTYPES: 0007 NAME p0007.
  INFOTYPES: 0008 NAME p0008.
  INFOTYPES: 0009 NAME p0009.
  INFOTYPES: 0016 NAME p0016.
  INFOTYPES: 0021 NAME p0021.
  INFOTYPES: 0027 NAME p0027.
  INFOTYPES: 0030 NAME p0030.
  INFOTYPES: 0040 NAME p0040.
  INFOTYPES: 0041 NAME p0041.
  INFOTYPES: 0057 NAME p0057.
  INFOTYPES: 0105 NAME p0105.
  INFOTYPES: 0167 NAME p0167.
  INFOTYPES: 0168 NAME p0168.
  INFOTYPES: 0169 NAME p0169.
  INFOTYPES: 0398 NAME p0398.
  INFOTYPES: 0465 NAME p0465.
  "INFOTYPES: 0598 NAME P0598.
  INFOTYPES: 0625 NAME p0625.
  INFOTYPES: 0661 NAME p0661.
  INFOTYPES: 9003 NAME p9003.
  INFOTYPES: 9004 NAME p9004.
*
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
*  SELECT-OPTIONS: p_pis  FOR pa0465-pis_nr. COMENTADO US 103380 - SCABANA
    SELECT-OPTIONS: p_cpf  FOR pa0465-cpf_nr.
    SELECT-OPTIONS: p_dte  FOR pa0001-begda .
    SELECT-OPTIONS: p_dts  FOR pa0001-endda.
  SELECTION-SCREEN END OF BLOCK b1.

  CLEAR: it_setleaf.
  SELECT *
    FROM setleaf
    INTO TABLE it_setleaf
     WHERE setname EQ 'MAGGI_HCM_AFAST'.

  LOOP AT it_setleaf INTO wa_setleaf.
    rg_afast-sign   = 'I'.
    rg_afast-option = 'EQ'.
    rg_afast-low    = wa_setleaf-valfrom(4).
    APPEND rg_afast.
    CLEAR: rg_afast.
  ENDLOOP.

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      zm_handle_hotspot_report
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD: zm_handle_hotspot_report.

    PERFORM user_command_0100 USING e_row_id e_column_id es_row_no.

  ENDMETHOD.
ENDCLASS.

CLASS dir_ger DEFINITION.
  PUBLIC SECTION.
    METHODS:
      get_diretoria IMPORTING i_orgeh            TYPE pa0001-orgeh
                              i_pernr            TYPE pa0001-pernr OPTIONAL
                              i_dt_dem           TYPE sy-datum
                    RETURNING VALUE(v_diretoria) TYPE hrp1000-stext,

      get_area_atuacao IMPORTING i_uniorg     TYPE orgeh
                                 i_uniorg_txt TYPE stext
                                 i_pernr      TYPE pernr OPTIONAL
                                 i_dt_dem     TYPE sy-datum
                       RETURNING VALUE(stext) TYPE stext.
ENDCLASS.
CLASS dir_ger IMPLEMENTATION.

  METHOD get_diretoria.
    DATA(v_busca) = 'ERRO'.
    DATA(v_objid) = i_orgeh.
    DATA(v_count) = 1.

    IF  i_dt_dem  IS NOT INITIAL.
      DATA(v_data) = i_dt_dem.
    ELSE.
      v_data = '99991231'.
    ENDIF.

    WHILE v_busca = 'ERRO'.

      SELECT SINGLE h1~objid,h1~sobid, h0~stext
      FROM hrp1001 AS h1
      INNER JOIN hrp1000 AS h0
      ON h0~objid = h1~objid
      INTO @DATA(lv_sobid)
      WHERE
        h1~relat = '002'      AND
        h1~sclas = 'O'        AND
        h1~otype = 'O'        AND
        h1~plvar = '01'       AND
        h1~endda >= @v_data   AND
        h1~objid = @v_objid   AND
        h1~rsign  = 'A'       AND
        h0~plvar = '01'       AND
        h0~otype = 'O'        AND
        h0~begda <= @v_data   AND
        h0~endda >= @v_data   AND
        h0~langu = @sy-langu.

      v_diretoria = COND #( WHEN lv_sobid-stext CS 'DIRETORIA'
                              OR lv_sobid-stext CS 'Diretoria'
                              OR lv_sobid-stext CS 'CONSELHO'
                              OR lv_sobid-stext CS 'Conselho'
                              OR lv_sobid-stext CS 'Presidencia'
                              OR lv_sobid-stext CS 'Presidência'
                              OR lv_sobid-stext CS 'PRESIDENCIA' THEN lv_sobid-stext ).

      IF v_diretoria IS INITIAL.
        v_objid = lv_sobid-sobid.
        CLEAR: lv_sobid.
      ELSE.
        v_busca = 'OK'.
      ENDIF.

      v_count = v_count + 1.

      IF v_count > 15.
        EXIT.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.
  METHOD get_area_atuacao.

    DATA(v_check)   = 'FALSE'.
    DATA(v_uniorg)  = i_uniorg.
    DATA(v_count)   = 1.

    IF i_uniorg_txt+0(12) CS 'Gerencia'    OR
       i_uniorg_txt+0(12) CS 'Gerência'    OR
       i_uniorg_txt+0(12) CS 'GERENCIA'    OR
       i_uniorg_txt+0(12) CS 'GERÊNCIA'    OR
       i_uniorg_txt+0(12) CS 'Diretoria'   OR
       i_uniorg_txt+0(12) CS 'DIRETORIA'   OR
       i_uniorg_txt+0(12) CS 'Presidencia' OR
       i_uniorg_txt+0(12) CS 'Conselho'.

      v_check = 'TRUE'.
      stext = i_uniorg_txt.

    ELSE.

      IF  i_dt_dem  IS NOT INITIAL.
        DATA(v_data) = i_dt_dem.
      ELSE.
        v_data = '99991231'.
      ENDIF.

      WHILE v_check = 'FALSE'.

        SELECT SINGLE
           h1~sobid,
           h0~stext
         FROM hrp1001 AS h1
         INNER JOIN hrp1000 AS h0
          ON h1~sobid = h0~objid
         INTO @DATA(w_orgeh)
         WHERE
           h1~otype = 'O'        AND
           h1~objid = @v_uniorg  AND
           h1~plvar = '01'       AND
           h1~rsign = 'A'        AND
           h1~endda >= @v_data   AND
           h1~begda < @sy-datum  AND
           h0~plvar = '01'       AND
           h0~otype = 'O'        AND
           h0~begda < @sy-datum  AND
           h0~endda >= @v_data   AND
           h0~langu = @sy-langu.

        IF w_orgeh-stext+0(12) CS 'Gerencia'    OR
           w_orgeh-stext+0(12) CS 'Gerência'    OR
           w_orgeh-stext+0(12) CS 'GERENCIA'    OR
           w_orgeh-stext+0(12) CS 'GERÊNCIA'    OR
           w_orgeh-stext+0(12) CS 'Diretoria'   OR
           w_orgeh-stext+0(12) CS 'DIRETORIA'   OR
           w_orgeh-stext+0(12) CS 'Presidencia' OR
           w_orgeh-stext+0(12) CS 'Conselho'.

          v_check = 'TRUE'.

        ELSE.
          v_uniorg = w_orgeh-sobid.
        ENDIF.

        v_count = v_count + 1.

        IF v_count > 10.
          EXIT.
        ENDIF.

      ENDWHILE.

      stext = w_orgeh-stext.

    ENDIF.
  ENDMETHOD.

ENDCLASS.

AT SELECTION-SCREEN OUTPUT. "80968 Limitar filtros de data
  DATA fcode TYPE TABLE OF sy-ucomm.

  PERFORM set_filtro_data.

  CLEAR fcode.
  APPEND 'FC01'  TO fcode.
  APPEND 'FC02'  TO fcode.
  APPEND 'FC03'       TO fcode.
  APPEND 'FC04'  TO fcode.
  APPEND 'FC05'  TO fcode.
  APPEND 'DYNS'  TO fcode.
  " APPEND 'ALLS' TO fcode.

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = '%_00' "sy-pfkey
*     P_PROGRAM = ' '
    TABLES
      p_exclude = fcode.

START-OF-SELECTION.

*  IF p_pis IS NOT INITIAL.
*
*    SELECT * FROM pa0465 INTO TABLE @DATA(it_p0465)
*      WHERE pis_nr IN @p_pis.
*
*    IF sy-subrc = 0.
*      SORT it_p0465 BY endda DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM  it_p0465 COMPARING pernr.
*
*      LOOP AT  it_p0465 INTO DATA(wa_p0465).
*        pnppernr-sign = 'I'.
*        pnppernr-option = 'EQ'.
*        pnppernr-low = wa_p0465-pernr.
*        APPEND pnppernr.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.
*
*  IF p_cpf IS NOT INITIAL.
*    SELECT * FROM pa0465 INTO TABLE it_p0465
*    WHERE cpf_nr IN p_cpf.
*
*    IF sy-subrc = 0.
*      SORT it_p0465 BY endda DESCENDING.
*      DELETE ADJACENT DUPLICATES FROM it_p0465 COMPARING pernr.
*
*      LOOP AT it_p0465 INTO wa_p0465.
*        pnppernr-sign = 'I'.
*        pnppernr-option = 'EQ'.
*        pnppernr-low = wa_p0465-pernr.
*        APPEND pnppernr.
*      ENDLOOP.
*    ENDIF.
*  ENDIF.

GET peras.

* Comentário ref a US 103380 - SCABANA

*  IF p_pis IS NOT INITIAL.
*
*    READ TABLE p0000[] INTO wa_0000 INDEX 1.
*
*    IF  wa_0000-pernr IS NOT INITIAL.
*
*      SELECT * FROM pa0465 INTO TABLE @DATA(it_p0465)
*        WHERE pis_nr IN @p_pis
*         AND pernr EQ @wa_0000-pernr.
*
*      IF sy-subrc = 0.
*        SORT it_p0465 BY endda DESCENDING.
*        DELETE ADJACENT DUPLICATES FROM  it_p0465 COMPARING pernr.
*
*        LOOP AT  it_p0465 INTO DATA(wa_p0465).
*          pnppernr-sign = 'I'.
*          pnppernr-option = 'EQ'.
*          pnppernr-low = wa_p0465-pernr.
*          APPEND pnppernr.
*        ENDLOOP.
*      ELSE.
*        CLEAR: p0000[], wa_0000.
*      ENDIF.
*    ENDIF.
*  ENDIF.

  IF p_cpf IS NOT INITIAL.

*    IF pnppernr IS NOT INITIAL
*      OR pnpbtrtl IS NOT INITIAL
*      OR pnpkostl IS NOT INITIAL
*      OR pnporgeh IS NOT INITIAL
*      OR pnppernr IS NOT INITIAL
*      OR pnppersg IS NOT INITIAL
*      OR pnppersk IS NOT INITIAL
*      OR pnpplans IS NOT INITIAL
*      OR pnpstat2 IS NOT INITIAL
*      OR pnpstell IS NOT INITIAL
*      OR pnpwerks IS NOT INITIAL.


    READ TABLE p0000[] INTO wa_0000 INDEX 1.

    IF  wa_0000-pernr IS NOT INITIAL.

      SELECT * FROM pa0465 INTO TABLE it_p0465
      WHERE cpf_nr IN p_cpf
         AND pernr EQ wa_0000-pernr.

      IF sy-subrc = 0.
        SORT it_p0465 BY endda DESCENDING.
        DELETE ADJACENT DUPLICATES FROM it_p0465 COMPARING pernr.

        LOOP AT it_p0465 INTO wa_p0465.
          pnppernr-sign = 'I'.
          pnppernr-option = 'EQ'.
          pnppernr-low = wa_p0465-pernr.
          APPEND pnppernr.
        ENDLOOP.
      ELSE.
        CLEAR: p0000[], wa_0000.
      ENDIF.
    ENDIF.
  ENDIF.



  IF  ( p_dte IS NOT INITIAL ) .

    READ TABLE p0000[] INTO wa_0000 INDEX 1.

    IF p_dte IS NOT INITIAL.
      CALL FUNCTION 'HR_ENTRY_DATE'
        EXPORTING
          persnr               = wa_0000-pernr
        IMPORTING
          entrydate            = lva_dt_admis
        EXCEPTIONS
          entry_date_not_found = 1
          pernr_not_assigned   = 2
          OTHERS               = 3.

      IF lva_dt_admis BETWEEN p_dte-low AND  p_dte-high.

      ELSE.
        CLEAR: p0000[], wa_0000.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_dts IS NOT INITIAL.
    READ TABLE p0000[] INTO wa_0000 INDEX 1.
    CALL FUNCTION 'RP_GET_FIRE_DATE'
      EXPORTING
        persnr   = wa_0000-pernr
      IMPORTING
        firedate = lva_data_saida.

    IF lva_data_saida BETWEEN p_dts-low AND p_dts-high.

    ELSE.
      CLEAR: p0000[], wa_0000.
    ENDIF.

  ENDIF.

*      LOOP AT t_result INTO DATA(wa_result).
*        pnppernr-sign = 'I'.
*        pnppernr-option = 'EQ'.
*        pnppernr-low = wa_result-pernr.
*        APPEND pnppernr.
*      ENDLOOP.

* US - 83772 - CBRAND - Fim


  PERFORM seleciona_dados.

* Get objects
  "GET PERAS.

END-OF-SELECTION.



  IF it_saida IS NOT INITIAL.
    CLEAR: it_fieldcatalog[].
    PERFORM alv_preenche_cat USING:

       'BUKRS'             'Empresa'                           '04'     ''      '' '' '',
       'BUKRS_TEXT'        'Descricao Empresa'                 '20'     ''      '' '' '',
       'WERKS'             'Cod Filial'                        '04'     ''      '' '' 'L',
       'WERKS_TEXT'        'Nome Filial'                       '20'     ''      '' '' '',
       'CGC_NR'            'CNPJ Filial'                       '15'     ''      '' '' '',
       'KOSTL'             'Centro Custo'                      '12'     ''      '' '' '',
       'CDESC'             'Desc. Centro Custo'                '20'     ''      '' '' '',
       'ORGEH'             'UnidOrg'                           '20'     ''      '' '' '',
       'ORGTX'             'Desc.UnidOrg'                      '30'     ''      '' '' '',
       'DIRETORIA'         'Diretoria'                         '30'     ''      '' '' '',
       'UNIORG_GERENCIA'   'Gerencia'                          '30'     ''      '' '' '',
       'PERNR'             'Matricula'                         '10'     ''      '' '' '',
       'CNAME'             'Nome'                              '30'     ''      '' '' '',
       'SITUACAO'          'Situação'                          '10'     ''      '' '' '',
       'BEGDA'             'Dt Admissão'                       '10'     ''      '' '' '',
       'STELL'             'Cargo'                             '15'     ''      '' '' '',
       'UNIORG'            'Descricao Cargo'                   '50'     ''      '' '' '',
*       'CBO_NR'            'CBO'                               '15'     ''      '' '' '',- COMENTADO US 103380 - SCABANA
*       'CBODE'             'Descrição CBO'                     '40'     ''      '' '' '',- COMENTADO US 103380 - SCABANA
       'PTEXT'             'Categoria Cargo'                   '12'     ''      '' '' '',
       'SETRISC'           'Setor'                             '12'     ''      '' '' '',
       'SETRISC_TXT'       'Descricao Setor'                   '12'     ''      '' '' '',

       'PLANS'             'Posicao'                           '10'     ''      '' '' 'L',
*       'BET01'             'Salário Base'                      '10'     ''      '' '' '', - COMENTADO US 103380 - SCABANA
*       'TRFGR'             'GS'                                '10'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'TRFST'             'Alvo'                              '10'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'SALFOP'            'Adicional Categ. Fluvial'          '04'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'PERCC'             'Periculosidade'                    '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'INSALUB'           'Insalubridade'                     '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'DAR05'             'Adicional Tempo Casa'              '40'     ''      '' '' '', COMENTADO US 103380 - SCABANA
       'TP_CASA'           'Tempo de Casa'                     '05'     ''      '' '' '',
       'IDADE'             'Idade'                             '05'     ''      '' '' '',
*       'VALTC'             'Valor Adc. Tempo Casa'             '40'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'CTTYP'             'Tipo Contrato'                     '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'CTEDT'             'Fim Contrato'                      '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'MOSTD'             'Carga Horária IT0007'              '07'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'DIVGV'             'Carga Horária IT0008'              '07'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ZTEXT'             'Desc. Carga Horaria IT0008'        '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
       'VINCEMP'           'Vinc.Empregaticio'                 '20'     ''      '' '' '',
*       'ESTAB'             'Estabilidade PCD'                  '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_BEGDA'       'Início E. PCD'                     '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_ENDDA'       'Fim E. PCD'                        '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_CIPA'        'Estabilidade CIPA'                 '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'ESTAB_CIPA_BEGDA'  'Início E. CIPA'                    '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_CIPA_ENDDA'  'Fim E. CIPA'                       '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'PCD'               'PCD'                               '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_DIR_SIND'    'Est. Dirigente Sindical'           '24'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_BEGDA_DS'    'Inicio Est. DS'                    '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_ENDDA_DS'    'Fim Est. DS'                       '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_VESP_AP'     'Est. Vespera Aposentar'            '24'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_BEGDA_VA'    'Inicio Est. VA'                    '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_ENDDA_VA'    'Fim Est. VA'                       '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_ACTRAB'      'Est. Acidente Trab.'               '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_BEGDA_AT'    'Inicio Est. AT'                    '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ESTAB_ENDDA_AT'    'Fim Est. AT'                       '15'     ''      '' '' '',COMENTADO US 103380 - SCABANA
       'ENDDA'             'Dt. Demissão'                      '14'     ''      '' '' '',
*       'MOTDESL'           'Motivo Desligamento'               '11'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'INFDEM'            'Informação Demissional'            '12'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'STAT_TMP'          'Registra Ponto?'                   '10'     ''      '' '' 'L',COMENTADO US 103380 - SCABANA
*       'MOFID'             'Calendario'                        '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'SINDCAT'           'Sindicato'                         '10'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'EMFSL'             'Cód. Sindicato'                    '12'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'ENDDA_SD'          'Validade'                          '10'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'LOCATU'            'Local de Atuação'                  '04'     ''      '' '' '',COMENTADO US 103380 - SCABANA
       'ABKRS'             'ArPFPg'                            '8'     ''      '' '' '',
       'ABKRS_TEXT'        'Txt. ArPFPg'                       '20'     ''      '' '' '',
*       'PER_EXP01'         '1 Per  Experiencia'                '07'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'PER_EXP02'         '2 Per  Experiencia'                '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'VAL_ALIM'          'Vale Alimentacao'                  '16'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'VAL_REFE'          'Vale Refeição'                     '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'BOPTIM'            'Assistência Médica'                '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'DEPCVM'            'Ass. Médica Dep.'                  '17'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'BOPTIO'            'Assistencia Odont'                 '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'DEPCVO'            'Ass. Odont Dep.'                   '10'     ''      '' '' 'L',COMENTADO US 103380 - SCABANA
*       'NHCNR'             'Cartão Nacional de Saúde'          '15'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'EEPCT'             'Prev  Privada'                     '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ZINS_VOL'          'Prev  Voluntaria'                  '10'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*PBI 59878 - 28.07.2021 - JT - inicio
*      'EECST'             'Seguro de Vida'                    '13'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'SEGVIDA'           'Seguro de Vida'                    '13'     ''      '' '' '',
*PBI 59878 - 28.07.2021 - JT - fim
       'CPF_NR'            'CPF'                               '04'     ''      '' '' '',
       'EMAIL'             'Email'                             '20'     ''      '' '' '',
       'GBDAT'             'Data Nascimento'                   '07'     ''      '' '' '',
       'LOCNASC'           'Local Nascimento'                  '20'     ''      '' '' '',
*       'RACACOR'           'Raca/Cor'                          '16'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'FCNAM'             'Nome da Mãe'                       '30'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'PIS_NR'            'Pis'                               '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'IDENT_NR'          'Nº RG'                             '14'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'DT_EMIS'           'Data Emis  RG'                     '11'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'RGORG'             'Orgao Rg'                          '12'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ES_EMIS'           'UF RG'                             '10'     ''      '' '' 'L',COMENTADO US 103380 - SCABANA
*       'CTPS_NR'           'Nº CTPS'                           '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'CTPS_SERIE'        'Série Carteira de Trabalho'        '10'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'DT_EMIS_CTPS'      'Data Emissão CTPS'                 '04'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'EMIS_CTPS'         'UF CTPS'                           '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA

*       'DRIVE_NR'          'Nr.Cart.Habilit.'                  '16'     ''      '' '' 'L',COMENTADO US 103380 - SCABANA
*       'CNHORG'            'Órgão emissor CNH'                 '17'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'DRIVE_CAT'         'Categoria Habilit.'                '18'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'DT_EMIS_DR'        'Data Emissão CNH'                  '16'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ES_EMIS_DR'        'UF de Expedição CNH'               '19'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'ENDDA_DR'          'Data Vencto Habilitação'           '23'     ''      '' '' '', COMENTADO US 103380 - SCABANA

*       'MIL_NR'            'N. Reservista'                     '20'     ''      '' '' '',COMENTADO US 103380 - SCABANA

*       'ELEC_NR'           'Nr. Título Eleitor'                '16'     ''      '' '' 'L',COMENTADO US 103380 - SCABANA
*       'ELEC_ZONE'         'Zona Tít. Eleitor'                 '17'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'ELEC_SECT'         'Seção Eleitoral'                   '18'     ''      '' '' '',COMENTADO US 103380 - SCABANA
*       'DT_EMIS_ELEC'      'Data Emissão Tít.'                 '16'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*       'ES_EMIS_ELEC'      'UF Título Eleitor'                 '19'     ''      '' '' '', COMENTADO US 103380 - SCABANA

       'GESCH'             'Sexo'                              '07'     ''      '' '' '',
       'ESTCV'             'Estado Civil'                      '20'     ''      '' '' '',
       'ESCOL'             'Escolaridade'                      '16'     ''      '' '' '',
*
       'STRAS'             'Rua'                               '20'     ''      '' '' '',
       'HSNMR'             'Nº'                                '14'     ''      '' '' '',
       'POSTA'             'Complemento'                       '11'     ''      '' '' '',
       'ORT02'             'Bairro'                            '12'     ''      '' '' '',
       'ORT01'             'Cidade'                            '10'     ''      '' '' 'L',
       'STATE'             'Estado'                            '20'     ''      '' '' '',
       'PSTLZ'             'CEP'                               '10'     ''      '' '' '',
*
*-CS2021000425 - 07.05.2021 - JT - inicio
       'STRAS_EMP'         'Rua Filial'                        '20'     ''      '' '' '',
       'HSNMR_EMP'         'Nº Filial'                         '14'     ''      '' '' '',
       'POSTA_EMP'         'Complemento Filial'                '11'     ''      '' '' '',
       'ORT02_EMP'         'Bairro Filial'                     '12'     ''      '' '' '',
       'ORT01_EMP'         'Cidade Filial'                     '10'     ''      '' '' 'L',
       'STATE_EMP'         'Estado Filial'                     '20'     ''      '' '' '',
       'PSTLZ_EMP'         'CEP Filial'                        '10'     ''      '' '' '',
*-CS2021000425 - 07.05.2021 - JT - inicio
*
       'VOLMA'            'Último Estado de Residência'       '50'     ''      '' '' '',
       'NUM01'            'Telefone'                          '04'     ''      '' '' '',
*       'CHVBCO'           'Chave do Banco'                    '20'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*** Inicio - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*       'BGRUP'            'Grupo de Bancos'                   '02'     ''      '' '' '', COMENTADO US 103380 - SCABANA
*** Fim    - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*       'CCBCO'            'Conta Bancaria'                    '07'     ''      '' '' '', COMENTADO US 103380 - SCABANA
       'GESTIMED'         'Gestor Imediato'                   '20'     ''      '' '' '',
       'EMAIL_IMED'       'E-mail Gestor Imediato'            '30'     ''      '' '' '',
       'GESTMED'          'Gestor Mediato'                    '16'     ''      '' '' '',
       'EMAIL_MED'        'E-mail Gestor Mediato'             '30'     ''      '' '' ''.
*       'FGTSD'            'Data Op. FGTS'                     '13'     ''      '' '' '', - COMENTADO US 103380 - SCABANA
**       'CATTR'            'Cat. Trabalhador'                  '40'     ''      '' '' '', - COMENTADO US 103380 - SCABANA
*       'NOTICE_PARTIAL'   'Av.P. eSocial'                     '15'     ''      '' '' '', - COMENTADO US 103380 - SCABANA
*       'SETRISC'          'Cód. Setor'                        '15'     ''      '' '' '',- COMENTADO US 103380 - SCABANA
*       'SETOR_RISCO'      'Setor'                             '35'     ''      '' '' '',- COMENTADO US 103380 - SCABANA
*       'AUFNR'            'Nº Ordem'                          '10'     'X'      'X' '' '', - COMENTADO US 103380 - SCABANA
*       'KTEXT'            'Desc. Ordem'                       '40'     ''       '' '' '',- COMENTADO US 103380 - SCABANA
*       'UNAME'            'User Admissão'                     '13'     ''       '' '' ''. COMENTADO US 103380 - SCABANA
*       'DOC_NR'           ' Mat.E-social'                     '13'     ''       '' '' ''. COMENTADO US 103380 - SCABANA

    PERFORM saida_tela.
  ENDIF.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona os dados para os funcionários com Prev. Privada
*----------------------------------------------------------------------*
FORM seleciona_dados .

  DATA: vdesc_pcd         TYPE t543s-sbtxt,
        vdesc_cattr       TYPE t7brct-catrx,
        v_data            TYPE endda,
        v_diretoria       TYPE hrp1000-stext,
        v_uniorg_gerencia TYPE stext,
*** Inicio - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*        vl_bgrup          TYPE bnka-bgrup, COMENTADO US 103380 - SCABANA
*** Fim    - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
        vdesc_volma       TYPE t591s-stext.

*** Inicio - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
  CONSTANTS cs_banks TYPE bnka-banks VALUE 'BR'.
*** Fim    - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021

  DATA(obj_dir_ger) = NEW dir_ger( ).

  SORT: p0000 BY aedtm DESCENDING,
        p0001 BY endda DESCENDING,
        p0002 BY endda DESCENDING,
        p0004 BY endda DESCENDING,
        p0006 BY endda DESCENDING,
        p0007 BY endda DESCENDING,
        p0008 BY endda DESCENDING,
        p0009 BY endda DESCENDING,
        p0016 BY endda DESCENDING,
        p0021 BY endda DESCENDING,
        p0030 BY endda DESCENDING,
        p0041 BY endda DESCENDING,
        p0057 BY endda DESCENDING,
        p0168 BY endda DESCENDING,
        p0169 BY endda DESCENDING,
        p0398 BY endda DESCENDING,
        "P0598 BY ENDDA DESCENDING,
        p0465 BY endda DESCENDING,
        p0625 BY endda DESCENDING,
        p0661 BY endda DESCENDING.

  FREE: wa_0006_emp.

  READ TABLE p0000[] INTO wa_0000 INDEX 1.
  READ TABLE p0001[] INTO wa_0001 INDEX 1.
  READ TABLE p0002[] INTO wa_0002 INDEX 1.
  READ TABLE p0004[] INTO wa_0004 INDEX 1.
  READ TABLE p0006[] INTO wa_0006 INDEX 1.
*-CS2021000425 - 07.05.2021 - JT - inicio
  LOOP AT p0006 INTO wa_0006_emp WHERE anssa = '6'
                                   AND endda >= '99991231'.
  ENDLOOP.
*-CS2021000425 - 07.05.2021 - JT - fim
  READ TABLE p0007[] INTO wa_0007 INDEX 1.
  READ TABLE p0008[] INTO wa_0008 INDEX 1.
  READ TABLE p0009[] INTO wa_0009 INDEX 1.
  READ TABLE p0016[] INTO wa_0016 INDEX 1.
  READ TABLE p0021[] INTO wa_0021 WITH KEY famsa = '12'. "MÃE
  READ TABLE p0027[] INTO wa_0027 INDEX 1.
  READ TABLE p0041[] INTO wa_0041 INDEX 1.
  READ TABLE p0057[] INTO wa_0057 INDEX 1.
  READ TABLE p0105[] INTO wa_0105 WITH KEY subty = 'MAIL' .
  READ TABLE p0398[] INTO wa_0398 INDEX 1.
  "READ TABLE P0598[] INTO WA_0598 INDEX 1.
  READ TABLE p0625[] INTO wa_0625 INDEX 1.
  READ TABLE p0661[] INTO wa_0661 INDEX 1.

  "Não pega o travel.
  IF wa_0000 IS NOT INITIAL.

    IF  wa_0001-abkrs NE 'BA'.

      wa_saida-pernr  = wa_0000-pernr.
      wa_saida-cname  = wa_0002-cname.


      LOOP AT p0465[]  INTO wa_0465.
        CASE wa_0465-subty.
          WHEN '0001'.
            CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
              EXPORTING
                input  = wa_0465-cpf_nr
              IMPORTING
                output = wa_saida-cpf_nr.
          WHEN '0002'. "COMENTADO US 103380 - SCABANA
*            wa_saida-ident_nr = wa_0465-ident_nr.
*            wa_saida-dt_emis  = wa_0465-dt_emis.
*            wa_saida-rgorg    = wa_0465-doc_issuer .
*            wa_saida-es_emis  = wa_0465-es_emis . "UF de Expedição

          WHEN '0003'. "COMENTADO US 103380 - SCABANA
*            wa_saida-ctps_nr       = wa_0465-ctps_nr.
*            wa_saida-ctps_serie    = wa_0465-ctps_serie.
*            wa_saida-dt_emis_ctps  = wa_0465-dt_emis.
*            wa_saida-emis_ctps     = wa_0465-es_emis.

          WHEN '0005'. "COMENTADO US 103380 - SCABANA
*            wa_saida-elec_nr       = wa_0465-elec_nr.    "Nr. Título Eleitor
*            wa_saida-elec_zone     = wa_0465-elec_zone.  "Zona Tít. Eleitor
*            wa_saida-elec_sect     = wa_0465-elec_sect.   "Seção Eleitoral
*            wa_saida-dt_emis_elec  = wa_0465-dt_emis. "Data Emissão
*            wa_saida-es_emis_elec  = wa_0465-es_emis. "UF Título Eleitor

          WHEN '0006'. "COMENTADO US 103380 - SCABANA
*            wa_saida-pis_nr = wa_0465-pis_nr.

*          WHEN '0007'. COMENTADO US 103380 - SCABANA
*            wa_saida-mil_nr = wa_0465-mil_nr. "N. Reservista

          WHEN '0010'.
*            wa_saida-drive_nr    =  wa_0465-drive_nr. COMENTADO US 103380 - SCABANA
*            wa_saida-cnhorg      =  wa_0465-doc_issuer. COMENTADO US 103380 - SCABANA
*            wa_saida-drive_cat   =  wa_0465-drive_cat. COMENTADO US 103380 - SCABANA
*            wa_saida-dt_emis_dr  =  wa_0465-dt_emis. COMENTADO US 103380 - SCABANA
*            wa_saida-es_emis_dr  =  wa_0465-es_emis.
*            wa_saida-endda_dr    =  wa_0465-endda. "Data Vencto Habilitação COMENTADO US 103380 - SCABANA
          WHEN '0015'.
*            wa_saida-nhcnr       = wa_0465-ident_nr. COMENTADO US 103380 - SCABANA
        ENDCASE.
*        wa_saida-doc_nr = wa_0465-doc_nr. COMENTADO US 103380 - SCABANA
      ENDLOOP.
*COMENTADO US 103380 - SCABANA
*      IF wa_saida-drive_nr IS INITIAL.
*        " Verifica se esta vencida e pega a ultima.
*        SELECT * FROM pa0465 INTO TABLE @DATA(it_p0465_dr)
*             WHERE pernr = @wa_saida-pernr
*             AND subty EQ '0010'.
*
*        IF sy-subrc = 0.
*
*          SORT it_p0465_dr BY endda.
*
*          READ TABLE it_p0465_dr INTO DATA(wa_0465_dr) INDEX 1.
*
*          wa_saida-drive_nr    =  wa_0465_dr-drive_nr.
*          wa_saida-cnhorg      =  wa_0465_dr-doc_issuer.
*          wa_saida-drive_cat   =  wa_0465_dr-drive_cat.
**          wa_saida-dt_emis_dr  =  wa_0465_dr-dt_emis. COMENTADO US 103380 - SCABANA
*          wa_saida-es_emis_dr  =  wa_0465_dr-es_emis.
*          wa_saida-endda_dr    =  wa_0465_dr-endda. "Data Vencto Habilitação
*        ENDIF.
*
*      ENDIF. COMENTADO US 103380 - SCABANA


      wa_saida-email  = wa_0105-usrid_long.

      "Data de Nascimento
      CONCATENATE  wa_0002-gbdat+6(2) '/' wa_0002-gbdat+4(2) '/' wa_0002-gbdat(4) INTO wa_saida-gbdat.

      "Local de Nascimento
      CONCATENATE wa_0002-gbort '-' wa_0002-gbdep INTO wa_saida-locnasc.

      "Idade 80255

      DATA: lv_idade_m TYPE int4.
      DATA: lv_idade_a TYPE int4.
      DATA: lvc_idade_m TYPE char4.
      DATA: lvc_idade_a TYPE char4.

      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from    = wa_0002-gbdat
*         I_KEY_DAY_FROM =
          i_date_to      = sy-datum
*         I_KEY_DAY_TO   =
          i_flg_separate = 'X'
        IMPORTING
          "  E_DAYS      = LV_IDADE
          e_months       = lv_idade_m
          e_years        = lv_idade_a.

      lvc_idade_m = lv_idade_m.
      lvc_idade_a = lv_idade_a.
      CONDENSE lvc_idade_m NO-GAPS.
      CONDENSE lvc_idade_a NO-GAPS.

      CONCATENATE   lvc_idade_a ','  lvc_idade_m INTO  wa_saida-idade .

      "Cor/Raça
      DATA: t_dd07v TYPE TABLE OF dd07v,
            s_dd07v TYPE dd07v.

      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'PBR_RACE'
          text            = 'X'
        TABLES
          values_tab      = t_dd07v
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.


      LOOP AT t_dd07v INTO s_dd07v.
        IF s_dd07v-domvalue_l EQ wa_0625-race.
*          wa_saida-racacor = s_dd07v-ddtext. COMENTADO US 103380 - SCABANA
        ENDIF.
      ENDLOOP.

*      wa_saida-fcnam = wa_0021-fcnam. COMENTADO US 103380 - SCABANA

      "SEXO
      IF wa_0002-gesch = '1'.
        wa_saida-gesch  = 'M'.
      ELSE.
        wa_saida-gesch = 'F'.
      ENDIF.

      SELECT SINGLE ftext
        FROM t502t
         INTO wa_saida-estcv
         WHERE famst = wa_0002-famst
         AND sprsl = 'PT'.

      CLEAR: vdesc_pcd.
      SELECT SINGLE sbtxt
        FROM t543s
         INTO vdesc_pcd
         WHERE sbgru = wa_0004-sbgru
         AND sprsl = 'PT'.

*      CONCATENATE wa_0004-sbgru '-' vdesc_pcd INTO wa_saida-pcd. COMENTADO US 103380 - SCABANA

      SELECT SINGLE escdd
       FROM t7br7t
      INTO wa_saida-escol
      WHERE escol = wa_0398-escol
      AND spras = 'PT'.

      "Vinculo Empregaticio
      SELECT SINGLE emtxt
        FROM t7brmt
        INTO wa_saida-vincemp
        WHERE empid = wa_0398-empid
        AND spras = 'PT'.

      "Opção FGTS
*      wa_saida-fgtsd = wa_0398-fgtsd.

      " Categoria trabalhador - COMENTADO US 103380 - SCABANA
*      CLEAR: vdesc_cattr.
*      SELECT SINGLE catrx
*       FROM t7brct
*        INTO vdesc_cattr
*        WHERE cattr = wa_0398-cattr
*        AND spras = 'PT'.
*
*      CONCATENATE wa_0398-cattr '-' vdesc_cattr INTO wa_saida-cattr. - COMENTADO US 103380 - SCABANA

      "SELECT SINGLE dptype, endda, dptxt FROM pa0598 INTO @DATA(w_0598) WHERE pernr = @wa_saida-pernr.

      IF pnpbegda IS INITIAL AND pnpendda IS INITIAL.

        CONCATENATE sy-datum(4)  sy-datum+4(2) '01' INTO v_begda_aux.
        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
          EXPORTING
            day_in            = v_begda_aux
          IMPORTING
            last_day_of_month = v_endda_aux
          EXCEPTIONS
            day_in_no_date    = 1
            OTHERS            = 2.

      ELSE.

        MOVE pnpbegda TO v_begda_aux.
        MOVE pnpendda TO v_endda_aux.

      ENDIF.

      SELECT dptype, endda, dptxt, begda, pernr, subty
        FROM pa0598
        INTO TABLE @DATA(lt_pa0598)
        WHERE pernr = @wa_saida-pernr
        AND   subty IN ( '9004', '9003', '9005', '9007', '9008' )
        AND   endda >= @v_endda_aux.
      IF sy-subrc IS INITIAL.
        SORT lt_pa0598 BY pernr subty.
      ENDIF.

*      IF w_0598-dptype = '9003'.
*        w_0598-endda = w_0598-endda + 365.
*      ENDIF.

      DATA: data_filtro TYPE dats.

      IF pnpdisbd IS NOT INITIAL.
        data_filtro = pnpdisbd.
      ELSE.
        data_filtro = sy-datum.
      ENDIF.

*      IF w_0598-endda  GE data_filtro.
*        "Estabilidade
*        CONCATENATE w_0598-dptype  '-' w_0598-dptxt INTO wa_saida-estab.
*      ENDIF.

      READ TABLE lt_pa0598 INTO DATA(ls_0598) WITH KEY pernr = wa_saida-pernr
                                                       subty = '9004' BINARY SEARCH.

*      IF sy-subrc IS INITIAL. COMENTADO US 103380 - SCABANA
*        CONCATENATE ls_0598-dptype '-' ls_0598-dptxt INTO wa_saida-estab.
*
*        wa_saida-estab_begda = ls_0598-begda.
*        wa_saida-estab_endda = ls_0598-endda.
*      ENDIF. COMENTADO US 103380 - SCABANA

      READ TABLE lt_pa0598 INTO ls_0598 WITH KEY pernr = wa_saida-pernr
                                                 subty = '9003' BINARY SEARCH.

*      IF sy-subrc IS INITIAL.COMENTADO US 103380 - SCABANA
*        CONCATENATE ls_0598-dptype '-' ls_0598-dptxt INTO wa_saida-estab_cipa.
*
*        wa_saida-estab_cipa_begda = ls_0598-begda.
*        wa_saida-estab_cipa_endda = ls_0598-endda.
*      ENDIF.COMENTADO US 103380 - SCABANA


***** US - 97288 - Inicio - CBRAND
      READ TABLE lt_pa0598 INTO ls_0598 WITH KEY pernr = wa_saida-pernr
                                                 subty = '9005' BINARY SEARCH.

*      IF sy-subrc IS INITIAL. - COMENTADO US 103380 - SCABANA
*        CONCATENATE ls_0598-dptype '-' ls_0598-dptxt INTO wa_saida-estab_vesp_ap.
*        wa_saida-estab_begda_va = ls_0598-begda.
*        wa_saida-estab_endda_va = ls_0598-endda.
*      ENDIF. - COMENTADO US 103380 - SCABANA

      READ TABLE lt_pa0598 INTO ls_0598 WITH KEY pernr = wa_saida-pernr
                                                 subty = '9007' BINARY SEARCH.

*      IF sy-subrc IS INITIAL. - COMENTADO US 103380 - SCABANA
*        CONCATENATE ls_0598-dptype '-' ls_0598-dptxt INTO wa_saida-estab_dir_sind.
*        wa_saida-estab_begda_ds = ls_0598-begda.
*        wa_saida-estab_endda_ds = ls_0598-endda.
*      ENDIF. - COMENTADO US 103380 - SCABANA

      READ TABLE lt_pa0598 INTO ls_0598 WITH KEY pernr = wa_saida-pernr
                                                subty = '9008' BINARY SEARCH.

*      IF sy-subrc IS INITIAL. COMENTADO US 103380 - SCABANA
*        CONCATENATE ls_0598-dptype '-' ls_0598-dptxt INTO wa_saida-estab_actrab.
*        wa_saida-estab_begda_at = ls_0598-begda.
*        wa_saida-estab_endda_at = ls_0598-endda.
*      ENDIF. COMENTADO US 103380 - SCABANA
***** US - 97288 - Fim - CBRAND

      wa_saida-stras = wa_0006-stras.
      wa_saida-hsnmr = wa_0006-hsnmr.
      wa_saida-posta = wa_0006-posta.
      wa_saida-ort02 = wa_0006-ort02.
      wa_saida-ort01 = wa_0006-ort01.
      wa_saida-state = wa_0006-state.
      wa_saida-pstlz = wa_0006-pstlz.
      wa_saida-num01 = wa_0006-num01.

*-CS2021000425 - 07.05.2021 - JT - inicio
      wa_saida-stras_emp = wa_0006_emp-stras.
      wa_saida-hsnmr_emp = wa_0006_emp-hsnmr.
      wa_saida-posta_emp = wa_0006_emp-posta.
      wa_saida-ort02_emp = wa_0006_emp-ort02.
      wa_saida-ort01_emp = wa_0006_emp-ort01.
      wa_saida-state_emp = wa_0006_emp-state.
      wa_saida-pstlz_emp = wa_0006_emp-pstlz.
*-CS2021000425 - 07.05.2021 - JT - fim

      "DATA ADMISSAO
      wa_saida-begda = wa_0041-dat01.

      "Tempo de Casa 80255
      DATA: lv_tpcasa_m TYPE int4.
      DATA: lv_tpcasa_a TYPE int4.
      DATA: lvc_tpcasa_m TYPE char4.
      DATA: lvc_tpcasa_a TYPE char4.
      DATA: lva_date_casa TYPE sy-datum.


*** PBI - 84583 - Inicio - CSB
*      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
*        EXPORTING
*          i_date_from    = wa_saida-begda
**         I_KEY_DAY_FROM =
*          i_date_to      = sy-datum
**         I_KEY_DAY_TO   =
*          i_flg_separate = 'X'
*        IMPORTING
*          "e_days      = lv_tpcasa
*          e_months       = lv_tpcasa_m
*          e_years        = lv_tpcasa_a.
*
*      lvc_tpcasa_m = lv_tpcasa_m.
*      lvc_tpcasa_a = lv_tpcasa_a.
*      CONDENSE lvc_tpcasa_m NO-GAPS.
*      CONDENSE lvc_tpcasa_a NO-GAPS.
*
*      CONCATENATE lvc_tpcasa_a ',' lvc_tpcasa_m INTO wa_saida-tp_casa.
*** PBI - 84583 - Fim - CSB

      "CONCATENATE  WA_0041-DAT01+6(2) '/' WA_0041-DAT01+4(2) '/' WA_0041-DAT01(4) INTO WA_SAIDA-BEGDA.
      "1 PER EXPERIENCIA
*      wa_saida-per_exp01 = wa_0041-dat03. COMENTADO US 103380 - SCABANA
      "CONCATENATE  WA_0041-DAT03+6(2) '/' WA_0041-DAT03+4(2) '/' WA_0041-DAT03(4) INTO WA_SAIDA-PER_EXP01 .
      "2 PER EXPERIENCIA
*      wa_saida-per_exp02 =  wa_0041-dat04. COMENTADO US 103380 - SCABANA
      "CONCATENATE  WA_0041-DAT04+6(2) '/' WA_0041-DAT04+4(2) '/' WA_0041-DAT04(4) INTO WA_SAIDA-PER_EXP02.

      IF wa_0000-stat2 = '0'. "Demitido
        "DATA DE DEMISSÃO
        CALL FUNCTION 'RP_GET_FIRE_DATE'
          EXPORTING
            persnr   = wa_0000-pernr
          IMPORTING
            firedate = wa_saida-endda.

        lva_date_casa = wa_saida-endda.
        wa_saida-situacao = 'Demitido'.

        "Busca tipo de demissão.
        SELECT SINGLE mgtxt FROM t530t
          INTO wa_saida-motdesl
        WHERE massg = wa_0000-massg
          AND massn = wa_0000-massn
        AND sprsl = sy-langu.

        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
            pernr           = wa_0001-pernr
            infty           = '9004'
            begda           = '19000101'
            endda           = '99991231'
          TABLES
            infty_tab       = p9004
          EXCEPTIONS
            infty_not_found = 1
            OTHERS          = 2.

        IF sy-subrc = 0.
          SORT: p9004 BY endda DESCENDING.
          READ TABLE p9004[] INTO wa_9004 INDEX 1.
          "Informação Demissional
*          wa_saida-infdem = wa_9004-dptxt. COMENTADO US 103380 - SCABANA
        ENDIF.

      ELSE.
        wa_saida-endda = wa_0041-endda.
        wa_saida-situacao = 'Ativo'.
        lva_date_casa = sy-datum.
      ENDIF.

*** PBI - 84583 - Inicio - CSB

      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          i_date_from    = wa_saida-begda
*         I_KEY_DAY_FROM =
          i_date_to      = lva_date_casa
*         I_KEY_DAY_TO   =
          i_flg_separate = 'X'
        IMPORTING
          "e_days      = lv_tpcasa
          e_months       = lv_tpcasa_m
          e_years        = lv_tpcasa_a.

      lvc_tpcasa_m = lv_tpcasa_m.
      lvc_tpcasa_a = lv_tpcasa_a.
      CONDENSE lvc_tpcasa_m NO-GAPS.
      CONDENSE lvc_tpcasa_a NO-GAPS.

      CONCATENATE lvc_tpcasa_a ',' lvc_tpcasa_m INTO wa_saida-tp_casa.

*** PBI - 68727 - Inicio - CSB
*      CLEAR: v_begda, v_endda.
*     IF pnpbegda IS INITIAL AND pnpendda IS INITIAL.
*        CONCATENATE sy-datum(4)  sy-datum+4(2) '01' INTO v_begda.
*        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*          EXPORTING
*            day_in            = v_begda
*          IMPORTING
*            last_day_of_month = v_endda
*          EXCEPTIONS
*            day_in_no_date    = 1
*            OTHERS            = 2.
*      ELSE.
*
*        v_begda =   pnpbegda.
*        v_endda =   pnpendda.
*      ENDIF.

*      CLEAR: return, it_absence.
*      CALL FUNCTION 'BAPI_ABSENCE_GETLIST'
*        EXPORTING
*          employeenumber   = pernr-pernr
*          timeintervallow  = v_begda
*          timeintervalhigh = v_endda
*        IMPORTING
*          return           = return
*        TABLES
*          absenceempkey    = it_absence
*        EXCEPTIONS
*          OTHERS           = 01.

*** PBI - 68727 - Fim - CSB

      CLEAR: v_begda, v_endda.
      "80968 >> Caso opção de filtro seja HOJE considerar sy-datum

      IF  ( pnpbegda IS INITIAL AND pnpendda IS INITIAL ) AND pnptimed NE 'M' .

        v_begda_aux = sy-datum.
        v_endda_aux = sy-datum.

        "CONCATENATE sy-datum(4)  sy-datum+4(2) '01' INTO v_begda_aux.
*        CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*          EXPORTING
*            day_in            = v_begda_aux
*          IMPORTING
*            last_day_of_month = v_endda_aux
*          EXCEPTIONS
*            day_in_no_date    = 1
*            OTHERS            = 2.

      ELSE.
        IF  pnpendda IS NOT INITIAL AND pnptimed NE 'M'    ." DATA FIXADA ENDATA É IGUAL

          MOVE pnpendda TO v_begda_aux.
          MOVE pnpendda TO v_endda_aux.

        ELSEIF pnptimed EQ 'M' .

          CONCATENATE sy-datum(4)  sy-datum+4(2) '01' INTO v_begda_aux.
          CALL FUNCTION 'LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = v_begda_aux
            IMPORTING
              last_day_of_month = v_endda_aux
            EXCEPTIONS
              day_in_no_date    = 1
              OTHERS            = 2.
        ENDIF.
*        ELSE.
*
*          MOVE pnpbegda TO v_begda_aux.
*          MOVE pnpendda TO v_endda_aux.
*
*        ENDIF.
      ENDIF.

*** CBRAND - 02.08.2023 - Descontinuação - BAPI_ABSENCE_GETLIST - S4 - Inicio
      CLEAR: return, it_absence.
*      CALL FUNCTION 'BAPI_ABSENCE_GETLIST'
*        EXPORTING
*          employeenumber   = pernr-pernr
*          timeintervallow  = v_begda_aux
*          timeintervalhigh = v_endda_aux
*        IMPORTING
*          return           = return
*        TABLES
*          absenceempkey    = it_absence
*        EXCEPTIONS
*          OTHERS           = 01.

      CALL FUNCTION 'HR_INFOTYPE_GETLIST'
        EXPORTING
          infty            = '2001'
          number           = pernr-pernr
          timeintervallow  = v_begda_aux
          timeintervalhigh = v_endda_aux
        IMPORTING
          return           = return
        TABLES
          key              = it_absence.




      SORT it_absence BY validend DESCENDING.

      IF return IS INITIAL. "Encontrou afastamento no mês.
        CLEAR: wa_absence.
        LOOP AT  it_absence INTO wa_absence.
          "BUSCAR A SITUACAO DO EMPREGADO.
          "IF wa_absence-subtype IN rg_afast AND ( wa_absence-validbegin <= v_endda AND wa_absence-validend > v_endda ). "A Afastamento tem que ser maior o processamento do mês.
          " IF wa_absence-subtype IN rg_afast AND ( wa_absence-validbegin >= v_begda_aux AND wa_absence-validend >= v_endda_aux ). "A Afastamento tem que ser maior o processamento do mês.
          IF wa_absence-subtype IN rg_afast AND ( wa_absence-validbegin <= v_begda_aux AND wa_absence-validend >= v_endda_aux ). "80968
            CLEAR: wa_saida-situacao.
            SELECT SINGLE atext
              FROM t554t
                INTO wa_saida-situacao
              WHERE sprsl = sy-langu
                AND moabw = '37'
                AND awart =  wa_absence-subtype.
          ENDIF.
        ENDLOOP.
      ENDIF.



      wa_saida-bukrs  = wa_0001-bukrs.

      CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
        EXPORTING
          bukrs      = wa_saida-bukrs
          langu      = sy-langu
        IMPORTING
          bukrs_text = wa_saida-bukrs_text.

      wa_saida-werks  = wa_0001-werks.

      CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
        EXPORTING
          werks      = wa_saida-werks
        IMPORTING
          werks_text = wa_saida-werks_text.

      CLEAR: wa_branch.
      IF wa_0001-werks IS NOT INITIAL .
        CALL FUNCTION 'HR_BR_GET_FILIAL_PER_AREA'
          EXPORTING
            p_werks = wa_0001-werks
            p_btrtl = wa_0001-btrtl
          IMPORTING
            branch  = wa_branch.
      ENDIF.

      "Endereço e CGC
      CLEAR: cgc_nr,comp_name.
      CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
        EXPORTING
          company_code      = wa_0001-bukrs
          branch            = wa_branch
          date              = '99991231'
        IMPORTING
          cgc               = cgc_nr
          comp_name         = comp_name
        EXCEPTIONS
          branch_not_found  = 1
          address_not_found = 2
          company_not_found = 3
          OTHERS            = 4.

      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
        EXPORTING
          input  = cgc_nr
        IMPORTING
          output = wa_saida-cgc_nr.


      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = wa_0001-pernr
          infty           = '9003'
          begda           = '19000101'
          endda           = '99991231'
        TABLES
          infty_tab       = p9003
        EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.

      IF sy-subrc = 0.
        SORT: p9003 BY endda DESCENDING.
        READ TABLE p9003[] INTO wa_9003 INDEX 1.
*        wa_saida-locatu = wa_9003-dptxt. COMENTADO US 103380 - SCABANA
      ENDIF.

      CLEAR: t_dd07v.
      CALL FUNCTION 'GET_DOMAIN_VALUES'
        EXPORTING
          domname         = 'ZTERF'
          text            = 'X'
        TABLES
          values_tab      = t_dd07v
        EXCEPTIONS
          no_values_found = 1
          OTHERS          = 2.

      CLEAR: s_dd07v.
      LOOP AT t_dd07v INTO s_dd07v.
*        IF s_dd07v-domvalue_l EQ wa_0007-zterf.
*          wa_saida-stat_tmp = s_dd07v-ddtext. "Bate ou não bate ponto  - ZTERF - COMENTADO US 103380 - SCABANA
*        ENDIF.
      ENDLOOP.

      "Calendário de feriado - COMENTADO US 103380 - SCABANA
*      SELECT SINGLE mofid  FROM t001p
*        INTO wa_saida-mofid
*      WHERE werks = wa_0001-werks
*        AND btrtl = wa_0001-btrtl.

*      wa_saida-mostd    = wa_0007-mostd.  "Carga Horária COMENTADO US 103380 - SCABANA

      "Sindicato - COMENTADO US 103380 - SCABANA
*      SELECT SINGLE emfna  FROM t521b
*        INTO wa_saida-sindcat
*      WHERE emfsl = wa_0057-emfsl
*        AND endda = '99991231'.


      "ÁCC
      wa_saida-kostl  = wa_0001-kostl.

      SELECT SINGLE ktext  FROM cskt
        INTO wa_saida-cdesc
      WHERE kokrs = wa_0001-kokrs
        AND kostl = wa_0001-kostl
        AND spras = sy-langu
        AND datbi >= sy-datum.

      wa_saida-abkrs  = wa_0001-abkrs.

      IF ( wa_saida-abkrs IS NOT INITIAL ).
        SELECT SINGLE atext FROM t549t INTO wa_saida-abkrs_text
           WHERE sprsl = sy-langu
             AND abkrs = wa_saida-abkrs.
      ENDIF.

      SELECT SINGLE ptext  FROM t503t
        INTO wa_saida-ptext
      WHERE sprsl = 'P'
        AND persk = wa_0001-persk.

      wa_saida-stell  = wa_0001-stell.

*-CS2021001206 - 16.03.2022 - JT - inicio
*      SELECT stltx
*        FROM t513s
*        INTO wa_saida-uniorg
*          UP TO 1 ROWS
*       WHERE sprsl  = sy-langu
*         AND stell  = wa_0001-stell
*         AND begda <= wa_0001-begda
*         AND endda >= wa_0001-begda.
*      ENDSELECT.


      SELECT stext
      FROM hrp1000
      INTO wa_saida-uniorg
        UP TO 1 ROWS
     WHERE langu  = sy-langu
       AND plvar  = '01'
       AND otype   = 'C'
       AND objid  = wa_0001-stell
       AND begda <= wa_0001-begda
       AND endda >= wa_0001-begda.
      ENDSELECT.

* PBI - 69522 - Inicio - CBRAND
*      IF pnpbegda IS NOT INITIAL
*      AND pnpendda IS NOT INITIAL.
*        SELECT SINGLE stext FROM hrp1000
*        INTO wa_saida-uniorg
*         WHERE plvar = '01'
*          AND otype = 'C'
*          AND objid = wa_0001-stell
*          AND langu = sy-langu
*            AND begda <= pnpendda
*            AND endda >= pnpbegda."sy-datum.
*        "AND endda = '99991231'.
*      ELSE.
*        IF pnpbegda IS NOT INITIAL
*          AND pnpendda IS INITIAL.
*
*          SELECT SINGLE stext FROM hrp1000
*            INTO wa_saida-uniorg
*             WHERE plvar = '01'
*              AND otype = 'C'
*              AND objid = wa_0001-stell
*              AND langu = sy-langu
*                AND begda <= pnpbegda
*                AND endda >= pnpbegda."sy-datum.
*        ELSE.
*          SELECT SINGLE stext FROM hrp1000
*            INTO wa_saida-uniorg
*             WHERE plvar = '01'
*              AND otype = 'C'
*              AND objid = wa_0001-stell
*              AND langu = sy-langu
*              AND endda >= sy-datum.
*        ENDIF.
*      ENDIF.
* PBI - 69522 - Fim - CBRAND
*-CS2021001206 - 16.03.2022 - JT - fim

      "CBO - - COMENTADO US 103380 - SCABANA
*      SELECT SINGLE cbo
*        FROM t7brcb
*        INTO wa_saida-cbo_nr
*        WHERE plans = wa_0001-plans
*        AND   endda GE sy-datum.
*
*      SELECT SINGLE cbode
*        FROM t7br20
*        INTO wa_saida-cbode
*        WHERE cbo = wa_saida-cbo_nr.

      IF ( wa_0001-plans = '99999999' ).

        DATA(v_endda) = sy-datum.
        v_endda = ( wa_0001-begda - 1 ).

        SELECT SINGLE
          p1~plans
        FROM
          pa0001 AS p1
        INTO
          wa_saida-plans
        WHERE
          p1~pernr = wa_0001-pernr  AND
          p1~endda = v_endda.

      ELSE.
        wa_saida-plans  = wa_0001-plans.
      ENDIF.


*** US - 115649 - CBRAND - Inicio
      IF pnpbegda IS NOT INITIAL AND pnpendda IS NOT INITIAL.
        SELECT SINGLE setrisc  FROM hrp9665
           INTO wa_saida-setrisc
           WHERE plvar = '01'
             AND otype = 'S'
             AND objid = wa_saida-plans
             AND begda <= pnpendda
             AND endda >= pnpbegda.

* Descrição do setor de risco
        SELECT setrisc_txt INTO wa_saida-setrisc_txt FROM zhrst_setor_risc UP TO 1 ROWS
         WHERE setrisc EQ wa_saida-setrisc
          AND begda <= pnpendda
          AND endda >= pnpbegda
          ORDER BY endda DESCENDING begda DESCENDING.
        ENDSELECT.


      ELSE.
        SELECT SINGLE setrisc  FROM hrp9665
         INTO wa_saida-setrisc
         WHERE plvar = '01'
           AND otype = 'S'
           AND objid = wa_saida-plans
           AND endda >= sy-datum.

* Descrição do setor de risco
        SELECT setrisc_txt INTO wa_saida-setrisc_txt FROM zhrst_setor_risc UP TO 1 ROWS
         WHERE setrisc EQ wa_saida-setrisc
          AND endda >= sy-datum
          ORDER BY endda DESCENDING begda DESCENDING.
        ENDSELECT.
      ENDIF.
*** US - 115649 - CBRAND - Fim


      "Setor - COMENTADO US 103380 - SCABANA
*      IF ( wa_saida-plans IS NOT INITIAL ).
*        IF pnpbegda IS NOT INITIAL AND pnpendda IS NOT INITIAL.
*          SELECT SINGLE setrisc stext FROM hrp9665
*        INTO (wa_saida-setrisc, wa_saida-setor_risco)
*        WHERE plvar = '01'
*          AND otype = 'S'
*          AND objid = wa_saida-plans
*          AND begda <= pnpendda
*          AND endda >= pnpbegda."sy-datum.
*        ELSE.
*          SELECT SINGLE setrisc stext FROM hrp9665
*          INTO (wa_saida-setrisc, wa_saida-setor_risco)
*          WHERE plvar = '01'
*            AND otype = 'S'
*            AND objid = wa_saida-plans
*            AND endda >= sy-datum.
*
*        ENDIF.
*      ENDIF. - COMENTADO US 103380 - SCABANA

      wa_saida-orgeh  = wa_0001-orgeh.

* PBI - 69522 - Inicio - CBRAND
      IF pnpbegda IS NOT INITIAL AND pnpendda IS NOT INITIAL.
        "SELECT SINGLE ORGTX FROM T527X INTO WA_SAIDA-ORGTX WHERE ORGEH = WA_SAIDA-ORGEH.
        SELECT SINGLE stext FROM hrp1000 INTO wa_saida-orgtx
          WHERE plvar = '01' AND
                otype = 'O'  AND
                objid = wa_saida-orgeh  AND
                begda <= pnpendda AND
                endda >= pnpbegda AND
                langu = sy-langu  AND
                infty = '1000'.
      ELSE.
        IF pnpbegda IS NOT INITIAL
              AND pnpendda IS INITIAL.
          SELECT SINGLE stext FROM hrp1000 INTO wa_saida-orgtx
            WHERE plvar = '01' AND
            otype = 'O'  AND
            objid = wa_saida-orgeh  AND
            begda <= pnpbegda AND
            endda >= pnpbegda AND
            langu = sy-langu  AND
            infty = '1000'.
        ELSE.
          SELECT SINGLE stext FROM hrp1000 INTO wa_saida-orgtx
            WHERE plvar = '01' AND
                  otype = 'O'  AND
                  objid = wa_saida-orgeh  AND
                  endda >= sy-datum AND
                  langu = sy-langu  AND
                  infty = '1000'.
        ENDIF.
      ENDIF.
* PBI - 69522 - Fim - CBRAND

*** US - 84585 - CBRAND - Inicio

* INICIO - STEFANINI - FT - 2000016066
      CLEAR: v_diretoria, v_uniorg_gerencia.
      CREATE OBJECT lo_sap_hcm.
      lo_sap_hcm->get_diretoria( EXPORTING i_pernr = wa_saida-pernr
                                 IMPORTING t_saida = DATA(git_saida_d) ).

      READ TABLE git_saida_d INTO DATA(lwa_diretoria) INDEX 1.
      v_diretoria = lwa_diretoria-orgetxt.
*      v_diretoria       = obj_dir_ger->get_diretoria( i_orgeh = wa_saida-orgeh i_pernr = wa_saida-pernr i_dt_dem = lva_date_casa ).
* FINAL - STEFANINI - FT - 2000016066

      v_uniorg_gerencia = obj_dir_ger->get_area_atuacao( i_uniorg = wa_saida-orgeh i_uniorg_txt = wa_saida-orgtx i_dt_dem = lva_date_casa ).

      wa_saida-diretoria       = v_diretoria.
      wa_saida-uniorg_gerencia = v_uniorg_gerencia.

*** US - 84585 - CBRAND - Fim


      v_data = sy-datum.
      CALL FUNCTION 'ZHCMF_RET_SUPERIOR_9002'
        EXPORTING
          pernr          = wa_0001-pernr
        TABLES
          t_saida        = it_gest
        CHANGING
          c_data_posicao = v_data.

      IF ( it_gest[] IS NOT INITIAL ).
        " Gestor Imediato
        TRY.
            DATA(v_name_gestimed) =  it_gest[ tp_gest = 'I' ]-cname.
            DATA(v_email_imed)   =  it_gest[ tp_gest = 'I' ]-email.
            wa_saida-gestimed = COND #( WHEN v_name_gestimed IS INITIAL THEN 'NO_MANAGER' ELSE v_name_gestimed ).
            wa_saida-email_imed = COND #( WHEN v_email_imed IS INITIAL THEN 'NO_MANAGER' ELSE v_email_imed ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

        " Gestor Mediato
        TRY.
            DATA(v_name_gestmed) =  it_gest[ tp_gest = 'M' ]-cname.
            DATA(v_email_med) = it_gest[ tp_gest = 'M' ]-email.
            wa_saida-gestmed =  COND #( WHEN v_name_gestmed IS INITIAL  THEN 'NO_MANAGER' ELSE v_name_gestmed ).
            wa_saida-email_med =  COND #( WHEN v_email_med IS INITIAL  THEN 'NO_MANAGER' ELSE v_email_med ).
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.

      ELSE.
        wa_saida-gestmed  = 'NO_MANAGER'.
        wa_saida-gestimed = 'NO_MANAGER'.
      ENDIF.


*** Inicio - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
      "COMENTADO US 103380 - SCABANA
*      SELECT SINGLE bgrup
*        FROM bnka
*        INTO vl_bgrup
*       WHERE banks = cs_banks
*         AND bankl = wa_0009-bankl.

*      wa_saida-bgrup  = vl_bgrup. COMENTADO US 103380 - SCABANA
*** Fim    - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021

*      wa_saida-chvbco = wa_0009-bankl. COMENTADO US 103380 - SCABANA
*      wa_saida-ccbco  = wa_0009-bankn. COMENTADO US 103380 - SCABANA

      wa_saida-bet01  = wa_0008-bet01.

      "CS2021000832 INCLUSÃO DE COLUNAS NA TRANSAÇÃO ZHCM_PA0009 - BG - INICIO

*      wa_saida-divgv = wa_0008-divgv. *COMENTADO US 103380 - SCABANA

      "busca a descrição da carga horaria COMENTADO US 103380 - SCABANA
*      DATA p_text TYPE c.
*      SELECT SINGLE
*        frequ
*      FROM
*        t710a
*      INTO
*        p_text
*      WHERE
*        molga = '37' AND
*        slreg = wa_0008-trfgb AND
*        slgrp = wa_0008-trfgr AND
*        endda <= wa_0008-endda AND
*        begda >= wa_0008-begda.
*      CASE p_text. COMENTADO US 103380 - SCABANA
*        WHEN 1.
*          wa_saida-ztext = 'mensalmente'.
*        WHEN 2.
*          wa_saida-ztext = 'Semi-mensal'.
*        WHEN 3.
*          wa_saida-ztext = 'semanalmente'.
*        WHEN 4.
*          wa_saida-ztext = 'Bi-semanal'.
*        WHEN 5.
*          wa_saida-ztext = 'De 4 em 4 semanas'.
*        WHEN 6.
*          wa_saida-ztext = 'anualmente'.
*        WHEN 7.
*          wa_saida-ztext = 'Trimestral'.
*        WHEN 8.
*          wa_saida-ztext = 'Cada hora'.
*        WHEN 9.
*          wa_saida-ztext = 'diariamente'.
*        WHEN 'A'.
*          wa_saida-ztext = 'semestralmente'.
*        WHEN OTHERS.
*          wa_saida-ztext = ' '.
*      ENDCASE. COMENTADO US 103380 - SCABANA
      "CS2021000832 INCLUSÃO DE COLUNAS NA TRANSAÇÃO ZHCM_PA0009 - BG - FIM

      "Grupo Salarial
      CLEAR: it_1005, wa_1005.
*      SELECT *
*        FROM hrp1005
*        INTO TABLE it_1005
*       WHERE otype = 'S'
*         AND objid = wa_0001-plans
*         AND plvar = '01' .

      SELECT *
      FROM hrp1005
      INTO TABLE it_1005
     WHERE otype = 'C'
       AND objid = wa_0001-stell
       AND endda > sy-datum
       AND plvar = '01' .

      READ TABLE it_1005 INTO wa_1005 INDEX 1.

*      wa_saida-trfgr  = wa_1005-trfg1+3(2). COMENTADO US 103380 - SCABANA
*      wa_saida-trfst  = wa_1005-trfs1. COMENTADO US 103380 - SCABANA

      "WA_SAIDA-TRFGR  = WA_0008-TRFGR.
      "WA_SAIDA-TRFST  = WA_0008-TRFS1.

      "Periculosidade COMENTADO US 103380 - SCABANA
*      IF  wa_0398-percc IS NOT INITIAL.
*        wa_saida-percc =  ( wa_saida-bet01 * 30 ) / 100.
*      ENDIF. COMENTADO US 103380 - SCABANA
      "CS2021000832 INCLUSÃO DE COLUNAS NA TRANSAÇÃO ZHCM_PA0009 - BG - INICIO

      "Incluido campo de insalubridade COMENTADO US 103380 - SCABANA
*      IF  wa_0398-insac IS NOT INITIAL.
*        DATA sal_minimo TYPE t511p-betrg.
*        IF pnpbegda IS NOT INITIAL AND pnpendda IS NOT INITIAL.
*          SELECT SINGLE
*            betrg
*          FROM
*            t511p
*          INTO
*            sal_minimo
*          WHERE
*            molga = '37'  AND
*            konst = 'MIINS' AND
*            begda <= pnpendda AND
*            endda >= pnpbegda.
*
*        ELSE.
*          SELECT SINGLE
*        betrg
*      FROM
*        t511p
*      INTO
*        sal_minimo
*      WHERE
*        molga = '37'  AND
*        konst = 'MIINS' AND
*        begda <= sy-datum AND
*        endda >= sy-datum.
*        ENDIF.
*
*        wa_saida-insalub =  ( sal_minimo * 20 ) / 100.
*      ENDIF. - COMENTADO US 103380 - SCABANA
      "CS2021000832 INCLUSÃO DE COLUNAS NA TRANSAÇÃO ZHCM_PA0009 - BG - FIM

      "Contrato - COMENTADO US 103380 - SCABANA
*      IF ( wa_0016-cttyp IS NOT INITIAL ).
*
*        SELECT SINGLE cttxt
*          FROM t547s
*          INTO @DATA(wl_contrato)
*          WHERE sprsl EQ 'P'
*            AND cttyp EQ @wa_0016-cttyp.
*
*        wa_saida-cttyp = |{ wa_0016-cttyp }-{ wl_contrato }|.
*        wa_saida-ctedt = wa_0016-ctedt.
*
*      ENDIF. - COMENTADO US 103380 - SCABANA

      "Nº Ordem PA0027 -
      IF ( wa_0027-auf01 IS NOT INITIAL ).

        SELECT SINGLE * FROM aufk
          INTO @DATA(lwa_aufk)
          WHERE aufnr = @wa_0027-auf01.
        IF ( sy-subrc = 0 ).
          wa_saida-aufnr = wa_0027-auf01.
          wa_saida-ktext = lwa_aufk-ktext.
        ENDIF.

      ENDIF.


      "Salario fora de opração.
      IF wa_0001-persk = 'I2'.
        SELECT SINGLE * FROM zhcmt_py_0001 INTO ls_py_0001 WHERE bukrs = wa_0001-bukrs
                                           AND   stell = wa_0001-stell
                                           AND endda GE wa_0001-endda
                                           AND begda LE wa_0001-begda.
*        IF sy-subrc = 0.
*          wa_saida-salfop = ( ls_py_0001-rubr_total - wa_saida-bet01 ) .
*        ENDIF. COMENTADO US 103380 - SCABANA

      ENDIF.

      IF wa_0001-persk = 'I2'.

        DATA: lv_etapa TYPE betrg,
              vl_betrg TYPE betrg.

        CLEAR: ls_py_0001.
        SELECT SINGLE * FROM zhcmt_py_0001 INTO ls_py_0001 WHERE bukrs = wa_0001-bukrs
                                           AND   stell = wa_0001-stell
                                           AND endda GE wa_0001-endda
                                           AND begda LE wa_0001-begda.
        IF sy-subrc = 0.
          lv_etapa  = ( ls_py_0001-rubr_1804 + wa_saida-bet01 ).
        ENDIF.

*        CASE wa_0041-dar05 . COMENTADO US 103380 - SCABANA
*          WHEN '51'. "Quinquenio 5%
*            wa_saida-dar05 = '5%'.
*            vl_betrg = ( 5 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '52'. "Quinquenio 10%
*            wa_saida-dar05 = '10%'.
*            vl_betrg = ( 10 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '53'. "Quinquenio 15%
*            wa_saida-dar05 = '15%'.
*            vl_betrg = ( 15 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '54'. "Quinquenio 20%
*            wa_saida-dar05 = '20%'.
*            vl_betrg = ( 20 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '55'. "Quinquenio 25%
*            wa_saida-dar05 = '25%'.
*            vl_betrg = ( 25 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '56'. "Quinquenio 30%
*            wa_saida-dar05 = '30%'.
*            vl_betrg = ( 30 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '57'. "Quinquenio 35%
*            wa_saida-dar05 = '35%'.
*            vl_betrg = ( 35 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '58'. "Quinquenio 40%
*            wa_saida-dar05 = '40%'.
*            vl_betrg = ( 40 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '59'. "Quinquenio 45%
*            wa_saida-dar05 = '45%'.
*            vl_betrg = ( 45 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '60'. "Quinquenio 50%
*            wa_saida-dar05 = '50%'.
*            vl_betrg = ( 50 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '80'. "Trienio 5%
*            wa_saida-dar05 = '5%'.
*            vl_betrg = ( 5 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '81'. "Trienio 10%
*            wa_saida-dar05 = '10%'.
*            vl_betrg = ( 10 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '82'. "Trienio 15%
*            wa_saida-dar05 = '15%'.
*            vl_betrg = ( 15 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '83'. "Trienio 20%
*            wa_saida-dar05 = '20%'.
*            vl_betrg = ( 20 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '84'. "Trienio 25%
*            wa_saida-dar05 = '25%'.
*            vl_betrg = ( 25 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '85'. "Trienio 30%
*            wa_saida-dar05 = '30%'.
*            vl_betrg = ( 30 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '86'. "Trienio 35%
*            wa_saida-dar05 = '35%'.
*            vl_betrg = ( 35 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '87'. "Trienio 40%
*            wa_saida-dar05 = '40%'.
*            vl_betrg = ( 40 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '88'. "Trienio 45%
*            wa_saida-dar05 = '45%'.
*            vl_betrg = ( 45 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '89'. "Trienio 50%
*            wa_saida-dar05 = '50%'.
*            vl_betrg = ( 50 *  lv_etapa  ) / 100.
*            wa_saida-valtc = vl_betrg.
*        ENDCASE.
*      ELSE.
*        CASE wa_0041-dar05 .
*          WHEN '51'. "Quinquenio 5%
*            wa_saida-dar05 = '5%'.
*            vl_betrg = ( 5 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '52'. "Quinquenio 10%
*            wa_saida-dar05 = '10%'.
*            vl_betrg = ( 10 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '53'. "Quinquenio 15%
*            wa_saida-dar05 = '15%'.
*            vl_betrg = ( 15 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '54'. "Quinquenio 20%
*            wa_saida-dar05 = '20%'.
*            vl_betrg = ( 20 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '55'. "Quinquenio 25%
*            wa_saida-dar05 = '25%'.
*            vl_betrg = ( 25 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '56'. "Quinquenio 30%
*            wa_saida-dar05 = '30%'.
*            vl_betrg = ( 30 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '57'. "Quinquenio 35%
*            wa_saida-dar05 = '35%'.
*            vl_betrg = ( 35 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '58'. "Quinquenio 40%
*            wa_saida-dar05 = '40%'.
*            vl_betrg = ( 40 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '59'. "Quinquenio 45%
*            wa_saida-dar05 = '45%'.
*            vl_betrg = ( 45 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '60'. "Quinquenio 50%
*            wa_saida-dar05 = '50%'.
*            vl_betrg = ( 50 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '80'. "Trienio 5%
*            wa_saida-dar05 = '5%'.
*            vl_betrg = ( 5 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '81'. "Trienio 10%
*            wa_saida-dar05 = '10%'.
*            vl_betrg = ( 10 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '82'. "Trienio 15%
*            wa_saida-dar05 = '15%'.
*            vl_betrg = ( 15 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '83'. "Trienio 20%
*            wa_saida-dar05 = '20%'.
*            vl_betrg = ( 20 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '84'. "Trienio 25%
*            wa_saida-dar05 = '25%'.
*            vl_betrg = ( 25 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '85'. "Trienio 30%
*            wa_saida-dar05 = '30%'.
*            vl_betrg = ( 30 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '86'. "Trienio 35%
*            wa_saida-dar05 = '35%'.
*            vl_betrg = ( 35 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '87'. "Trienio 40%
*            wa_saida-dar05 = '40%'.
*            vl_betrg = ( 40 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '88'. "Trienio 45%
*            wa_saida-dar05 = '45%'.
*            vl_betrg = ( 45 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*          WHEN '89'. "Trienio 50%
*            wa_saida-dar05 = '50%'.
*            vl_betrg = ( 50 * wa_saida-bet01 ) / 100.
*            wa_saida-valtc = vl_betrg.
*        ENDCASE. COMENTADO US 103380 - SCABANA

      ENDIF.

      DELETE p0030[] WHERE volma <> '12'.
      IF p0030[] IS NOT INITIAL.
        READ TABLE p0030[] INTO wa_0030 INDEX 1.
*BUG 62005 - Inicio - CSB
        CLEAR: text_tab, wa_text_tab, pskey.

        pskey-pernr = wa_0030-pernr.
        pskey-infty = '0030'.
        pskey-subty = '12'.
        pskey-endda = wa_0030-endda.
        pskey-begda = wa_0030-begda.

        CALL METHOD cl_hrpa_text_cluster=>read
          EXPORTING
            tclas         = 'A'
            pskey         = pskey
            no_auth_check = ''
          IMPORTING
            text_tab      = text_tab.

        IF text_tab IS NOT INITIAL.
          LOOP AT text_tab  INTO wa_text_tab.
            IF wa_text_tab IS NOT INITIAL.
              CASE sy-tabix.
                WHEN 1.
                  wa_saida-volma = wa_text_tab.

                WHEN 2.
                  CONCATENATE  wa_saida-volma '-' wa_text_tab INTO  wa_saida-volma SEPARATED BY space.
                WHEN 3.
                  CONCATENATE  wa_saida-volma '-' wa_text_tab INTO  wa_saida-volma SEPARATED BY space.
              ENDCASE.
            ENDIF.
          ENDLOOP.
        ENDIF.

*        CLEAR: text_tab_local, wa_text_tab_local.
*        CALL FUNCTION 'HR_ECM_READ_TEXT_INFOTYPE'
*          EXPORTING
*            pernr           = wa_0001-pernr
*            infty           = '0030'
*            subty           = wa_0030-volma
*            objps           = wa_0001-objps
*            begda           = wa_0030-begda
*            endda           = wa_0030-endda
*            seqnr           = wa_0001-seqnr
*            no_auth_check   = no_auth_check
*            message_handler = message_handler
*          IMPORTING
*            text_tab        = text_tab_local
*            is_ok           = is_ok.
*
*        LOOP AT text_tab_local  INTO wa_text_tab_local.
*          CASE sy-tabix.
*            WHEN 1.
*              wa_saida-volma = wa_text_tab_local.
*            WHEN 2.
*              CONCATENATE  wa_saida-volma '-' wa_text_tab_local INTO  wa_saida-volma SEPARATED BY space.
*            WHEN 3.
*              CONCATENATE  wa_saida-volma '-' wa_text_tab_local INTO  wa_saida-volma SEPARATED BY space.
*          ENDCASE.
*
*        ENDLOOP.
*BUG 62005 - Fim - CSB
      ENDIF.

      DELETE p0040 WHERE endda <> '99991231'.

      IF p0040[] IS NOT INITIAL.

        CLEAR: lt_bn_0001, l_0001.

        SELECT * FROM zhcmt_bn_0001 INTO TABLE lt_bn_0001.
        DELETE  lt_bn_0001 WHERE endda <> '99991231'.

        READ TABLE lt_bn_0001 INTO l_0001 WITH KEY bukrs = wa_0001-bukrs werks = wa_0001-werks .

        LOOP AT p0040 INTO wa_0040.
*          COMENTADO US 103380 - SCABANA
*          CASE wa_0040-leihg .
*            WHEN '01'. "ALIMENTAÇÃO
*              wa_saida-val_alim = l_0001-va_p_mvra.
*            WHEN '02'. "REFEICAO
*              wa_saida-val_refe = l_0001-vr_p_mvr0.
*          ENDCASE. COMENTADO US 103380 - SCABANA
        ENDLOOP.

      ENDIF.

* INFOTYPE 0167 - HEALTH PLANS
      REFRESH health_plans.
      CALL FUNCTION 'HR_BEN_READ_HEALTH_PLANS'
        EXPORTING
          pernr          = wa_0001-pernr
          barea          = 'BR'
          begda          = '00810101'
          endda          = '99991231'
          logicview      = ' '
          include_locked = false
          reaction       = no_msg
        IMPORTING
          subrc          = subrc
        TABLES
          ex_heal_plans  = health_plans
          error_table    = error_table.

      DELETE health_plans WHERE endda(4) <> '9999'.

      LOOP AT health_plans.
        v_qnt = 0.
        IF health_plans-pltyp = 'DENT'.
          IF health_plans-depcv  CS 'EE+'.
            v_qnt = health_plans-depcv+3(1).
*            wa_saida-boptio = health_plans-eecst. COMENTADO US 103380 - SCABANA
*            wa_saida-depcvo = v_qnt. COMENTADO US 103380 - SCABANA
          ELSE.
*            wa_saida-boptio = health_plans-eecst.
*            wa_saida-depcvo = 0.
          ENDIF.
        ELSE.
          IF health_plans-depcv  CS 'EE+'. "Tem dependentes.
            v_qnt = health_plans-depcv+3(1).
*            wa_saida-boptim =  health_plans-eecst * v_qnt . COMENTADO US 103380 - SCABANA
*            wa_saida-depcvm =   v_qnt . COMENTADO US 103380 - SCABANA
          ELSE.
*            wa_saida-boptim = health_plans-eecst . COMENTADO US 103380 - SCABANA
*            wa_saida-depcvm =   '0'. COMENTADO US 103380 - SCABANA
          ENDIF.
        ENDIF.
      ENDLOOP.

*** PBI - 59878 - Inicio
      READ TABLE p0168 INTO wa_0168 INDEX 1.
      IF sy-subrc = 0.
        wa_saida-segvida = abap_true.

        SELECT * FROM t5ubh INTO TABLE it_t5ubh
          WHERE barea = wa_0168-barea
           AND  bplan = wa_0168-bplan.

        READ TABLE it_t5ubh INTO wa_t5ubh INDEX 1.

        SELECT * FROM t5ubi INTO TABLE it_t5ubi
          WHERE barea = wa_0168-barea
           AND  bplan = wa_0168-bplan
           AND  bcost = wa_t5ubh-bcost.

        READ TABLE it_t5ubi INTO wa_t5ubi INDEX 1.

*        wa_saida-eecst = wa_t5ubi-eecst. COMENTADO US 103380 - SCABANA
      ENDIF.

***  PBI - 59878 - Fim

      READ TABLE p0169 INTO wa_0169 INDEX 1.

*      wa_saida-eepct    = wa_0169-eepct. COMENTADO US 103380 - SCABANA
*      wa_saida-zins_vol = wa_0169-zins_vol. COMENTADO US 103380 - SCABANA

      CLEAR: p0661.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
          pernr           = wa_0001-pernr
          infty           = '0661'
          begda           = '19000101'
          endda           = '99991231'
        TABLES
          infty_tab       = p0661
        EXCEPTIONS
          infty_not_found = 1
          OTHERS          = 2.

*COMENTADO US 103380 - SCABANA
*      IF sy-subrc = 0.
*        SORT: p0661 BY endda DESCENDING.
*        READ TABLE p0661[] INTO wa_0661 INDEX 1.
*        wa_saida-notice_partial = wa_0661-notice_partial.
*        CLEAR: wa_0661.
*      ENDIF. COMENTADO US 103380 - SCABANA

*      wa_saida-emfsl = wa_0057-emfsl. COMENTADO US 103380 - SCABANA
*      wa_saida-endda_sd = wa_0057-endda.

***  PBI - 59878 - Inicio
      SELECT SINGLE uname FROM  pa0001
        INTO wa_saida-uname
      WHERE pernr = wa_saida-pernr
        AND begda = wa_saida-begda.
***  PBI - 59878 - Fim

      APPEND wa_saida TO it_saida.

      CLEAR:  wa_saida,
              wa_0000,
              wa_0001,
              wa_0002,
              wa_0004,
              wa_0006,
              wa_0007,
              wa_0008,
              wa_0009,
              wa_0040,
              wa_0041,
              wa_0027,
              wa_0021,
              lwa_aufk,
              wa_0057,
              wa_0105,
              wa_0167,
              wa_0169,
              wa_0398,
              wa_0465,
              wa_0598,
              wa_0625,
              wa_9003,
              wa_9004,
              it_gest,
              ls_py_0001.

    ENDIF.
  ENDIF.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND_0100
*&---------------------------------------------------------------------*
FORM user_command_0100 USING e_row_id TYPE lvc_s_row
                             p_e_column_id TYPE lvc_s_col
                             p_es_eow_no TYPE lvc_s_roid.


  READ TABLE it_saida[] INTO DATA(lwa_saida) INDEX e_row_id-index.

  CHECK ( sy-subrc = 0 ).

  CASE p_e_column_id-fieldname.
    WHEN 'AUFNR'.
      IF lwa_saida-aufnr IS NOT INITIAL.
        SET PARAMETER ID 'ANR' FIELD lwa_saida-aufnr.
        CALL TRANSACTION 'KO03' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  EXPORTA_EXCEL
*&---------------------------------------------------------------------*
*       Exporta a tabela de saída no formato Excel
*----------------------------------------------------------------------*
FORM exporta_excel .

  DATA valor(30).
  DATA: p_local   TYPE string,
        path(250).
  DATA: t_alvdata TYPE REF TO data.

  DATA:
    BEGIN OF t_fieldnames OCCURS 0,
      name TYPE char30,
    END OF t_fieldnames.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = 'C:\'
      mask             = ',*.XLS,'
      mode             = 'S'
      title            = 'Local de Gravação'
    IMPORTING
      filename         = path
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

  IF sy-subrc IS INITIAL.

    CONCATENATE path '.XLS' INTO p_local.

    t_fieldnames-name    = 'Empresa'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Descricao Empresa'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Cod Filial'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Nome Filial'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'CNPJ Filial'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Centro cst'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Descricao C. Custo'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Matricula'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Nome'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Situação'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Dt.Admissão'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Cargo'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Descricao Cargo'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'CBO'.- COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Descricao CBO'.- COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'Categoria Cargo'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Setor'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Descricao Setor'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Posicao'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'UnidOrg'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Desc.UnidOrg'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Salário Base'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Adicional Categ. Fluvial'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Periculosidade'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Insalubridade'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Tipo Contrato'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Fim Contrato'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Carga Horária IT0007'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Carga Horária IT0008'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Desc. Carga Horaria IT0008'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'Vinc. Empregaticio'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Estabilidade PCD'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Início E. PCD'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Fim E. PCD'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Estabilidade CIPA'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Início E. CIPA'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Fim E. CIPA'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'PCD'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'Dt. Demissão'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Motivo Desligamento'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Informação Demissional'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Registra Ponto?'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Calendario'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Sindicato'.
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Local de Atuação'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'ArPFPg'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = '1 Per. Experiencia'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = '2 Per. Experiencia'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Vale Alimentacao'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Vale Refeição'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Assistência Médica'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Assistência Médica'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Assistencia Odont.'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Assistencia Odont.'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Prev. Privada'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Prev. Voluntaria'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'CPF'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Email'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Data Nascimento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Local Nascimento'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Raca/Cor'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Pis'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Nº RG'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Data Emis. RG'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Orgao Rg'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'UF RG'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Nº CTPS'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Série Carteira de Trabalho'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Data Emissão CTPS'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*    t_fieldnames-name    = 'UF CTPS'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'Sexo'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Estado Civil'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Escolariedade'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Rua'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Nº'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Complemento'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Bairro'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Cidade'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Estado'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'CEP'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Telefone'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Chave do Banco'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
**** Inicio - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*    t_fieldnames-name    = 'Grupo de Bancos'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
*** Fim    - CS2021001029 Incluir Dígito Agência - Relatório de Ativos - JMONTEIRO - 11/10/2021
*    t_fieldnames-name    = 'Conta Bancaria'.COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'Gestor Imediato'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Gestor Mediato'.
    APPEND t_fieldnames.
*    t_fieldnames-name    = 'Mat.E-social'. COMENTADO US 103380 - SCABANA
*    APPEND t_fieldnames.
    t_fieldnames-name    = 'Tempo de Casa'.
    APPEND t_fieldnames.
    t_fieldnames-name    = 'Idade'.
    APPEND t_fieldnames.


    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename            = p_local
        filetype            = 'DAT' "'DAT' "'DBF'
      TABLES
        data_tab            = it_saida[]
        fieldnames          = t_fieldnames[]
      EXCEPTIONS
        file_open_error     = 1
        file_write_error    = 2
        invalid_filesize    = 3
        invalid_table_width = 4
        invalid_type        = 5
        no_batch            = 6
        unknown_error       = 7
        OTHERS              = 8.

    IF sy-subrc = 0.
      MESSAGE 'Arquivos gerados com sucesso' TYPE 'S'.
    ELSE.
      MESSAGE 'Arquivo processado com erro' TYPE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM alv_preenche_cat  USING    VALUE(p_campo)
                                VALUE(p_desc)
                                VALUE(p_tam)
                                VALUE(p_zero)
                                VALUE(p_hot)
                                VALUE(p_sum)
                                VALUE(p_just).

  wa_fieldcatalog-fieldname  = p_campo.
  wa_fieldcatalog-coltext    = p_desc.
  wa_fieldcatalog-scrtext_l  = p_desc.
  wa_fieldcatalog-scrtext_m  = p_desc.
  wa_fieldcatalog-scrtext_s  = p_desc.

  wa_fieldcatalog-outputlen  = p_tam.
  wa_fieldcatalog-hotspot    = p_hot.
  wa_fieldcatalog-no_zero    = p_zero.
  wa_fieldcatalog-do_sum     = p_sum.
  wa_fieldcatalog-just       = p_just.
  wa_fieldcatalog-col_opt    = 'X'.

*PBI 59878 - 28.07.2021 - JT - inicio
  IF wa_fieldcatalog-fieldname = 'SEGVIDA'.
    wa_fieldcatalog-checkbox = abap_true.
  ELSE.
    wa_fieldcatalog-checkbox = abap_false.
  ENDIF.
*PBI 59878 - 28.07.2021 - JT - fim

  APPEND wa_fieldcatalog TO it_fieldcatalog.


ENDFORM.                    " ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  SAIDA_TELA
*&---------------------------------------------------------------------*
FORM saida_tela .
  CALL SCREEN 0100.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_COLUMN_NAMES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_column_names
  USING ir_columns TYPE REF TO cl_salv_columns.

  DATA: lr_column TYPE REF TO cl_salv_column,
        lt_column TYPE salv_t_column_ref,
        ls_column TYPE salv_s_column_ref,
        lv_coltxt TYPE scrtext_m.

  lt_column = ir_columns->get( ).

  LOOP AT lt_column INTO ls_column.
    lv_coltxt = ls_column-columnname.
    REPLACE ALL OCCURRENCES OF '_' IN lv_coltxt WITH ` `.
    ls_column-r_column->set_medium_text( lv_coltxt ).
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: url(255)                TYPE c,
        data_ini(10)            TYPE c,
        data_fim(10)            TYPE c,
        p_text                  TYPE sdydo_text_element,
        sdydo_text_element(255),
        p_text_table            TYPE sdydo_text_table.

  SET PF-STATUS 'PF0100'.
  SET TITLEBAR  'TL0100'.

  IF g_custom_container IS INITIAL.
* create a container for the tree control
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = 'CONTAINER'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc <> 0.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CREATE OBJECT dg_splitter_1
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_1.

    CALL METHOD dg_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_alv.

    CREATE OBJECT dg_splitter_2
      EXPORTING
        parent  = dg_parent_1
        rows    = 1
        columns = 2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_2.

    CALL METHOD dg_splitter_2->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = dg_parent_2a.

    CALL METHOD dg_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 16.

    CALL METHOD dg_splitter_2->set_column_width
      EXPORTING
        id    = 1
        width = 40.

    CREATE OBJECT picture
      EXPORTING
        parent = dg_parent_2a.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

*   Fill info for layout variant
    PERFORM fill_gs_variant.

    gs_layout-sel_mode   = 'A'.
    CLEAR: it_exclude_fcode, it_exclude_fcode[].

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = dg_parent_alv.

    SET HANDLER lcl_event_receiver=>zm_handle_hotspot_report FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        is_variant           = gs_variant
        it_toolbar_excluding = it_exclude_fcode
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_fieldcatalog
        it_outtab            = it_saida[].

    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.

    CALL METHOD dg_dyndoc_id->initialize_document.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 1
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element.

    CALL METHOD table_element->add_column
      IMPORTING
        column = column.

    CALL METHOD table_element->set_column_style
      EXPORTING
        col_no    = 1
        sap_align = 'CENTER'
        sap_style = cl_dd_document=>heading.

    p_text = TEXT-002.

    CALL METHOD column->add_text
      EXPORTING
        text      = p_text
        sap_style = 'HEADING'.

    CALL METHOD dg_dyndoc_id->add_table
      EXPORTING
        no_of_columns = 2
        border        = '0'
        width         = '100%'
      IMPORTING
        table         = table_element2.

    CALL METHOD table_element2->add_column
      EXPORTING
        sap_style   = 'SAP_BOLD'
        style_class = 'SAP_BOLD'
      IMPORTING
        column      = column_1.

    CLEAR: p_text_table.
    "SDYDO_TEXT_ELEMENT = 'Escalas: '.
    "APPEND SDYDO_TEXT_ELEMENT TO P_TEXT_TABLE.

    IF pnpbegda IS INITIAL AND pnpendda IS INITIAL.
      sdydo_text_element = 'Período: Data Atual' .
    ELSE.


      CONCATENATE  pnpbegda+6(2) '.' pnpbegda+4(2) '.' pnpbegda(4)  INTO DATA(data_inicio).
      CONCATENATE  pnpendda+6(2) '.' pnpendda+4(2) '.' pnpendda(4)  INTO DATA(data_end).
      CONCATENATE 'Período:' data_inicio 'á' data_end INTO sdydo_text_element SEPARATED BY space.

    ENDIF.

    "sdydo_text_element = 'Período: '.
    APPEND sdydo_text_element TO p_text_table.

    CALL METHOD column_1->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD table_element2->add_column
      IMPORTING
        column = column_2.

    CALL METHOD table_element2->set_column_style
      EXPORTING
        col_no       = 2
        sap_align    = 'LEFT'
        sap_fontsize = cl_dd_document=>medium.

    CLEAR: p_text_table.

    CONCATENATE  sy-datum+6(2) '.' sy-datum+4(2) '.' sy-datum(4)  INTO data_ini.
    "CONCATENATE  P_BEGDA-HIGH+6(2) '.' P_BEGDA-HIGH+4(2) '.' P_BEGDA-HIGH(4) INTO DATA_FIM.


    CONCATENATE 'Data de Processamento:' data_ini INTO sdydo_text_element SEPARATED BY space.
    APPEND sdydo_text_element TO p_text_table.

    CALL METHOD column_2->add_text
      EXPORTING
        text_table = p_text_table
        fix_lines  = 'X'.

    CALL METHOD dg_dyndoc_id->merge_document.

    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_2.

    dg_dyndoc_id->html_control = dg_html_cntrl.

    CALL METHOD dg_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = dg_parent_2
      EXCEPTIONS
        html_display_error = 1.

  ENDIF.

  CALL METHOD ctl_alv->get_scroll_info_via_id
    IMPORTING
      es_col_info = gs_scroll_col
      es_row_no   = gs_scroll_row.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0100'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

ENDFORM.                    " FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0190   text
*      <--P_URL  text
*----------------------------------------------------------------------*
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  DATA: BEGIN OF graphic_table OCCURS 0,
          line(255) TYPE x,
        END OF graphic_table.
  DATA: l_graphic_xstr TYPE xstring.
  DATA: graphic_size   TYPE i.
  DATA: l_graphic_conv TYPE i.
  DATA: l_graphic_offs TYPE i.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.

ENDFORM.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  SET_FILTRO_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_filtro_data .

  DATA: lv_id_data TYPE vrm_id VALUE 'PNPTIMED',
        lt_datas   TYPE STANDARD TABLE OF vrm_value,
        wa_datas   TYPE vrm_value.

  wa_datas-key = 'D'.
  wa_datas-text = 'Hoje'.
  APPEND wa_datas TO lt_datas.
  wa_datas-key = 'K'.
  wa_datas-text = 'Data Fixada'.
  APPEND wa_datas TO lt_datas.
  wa_datas-key = 'I'.
  wa_datas-text = 'Outro Período'.
  APPEND wa_datas TO lt_datas.
  wa_datas-key = 'M'.
  wa_datas-text = 'Mês Atual'.
  APPEND wa_datas TO lt_datas.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_id_data
      values = lt_datas
*   EXCEPTIONS
*     ID_ILLEGAL_NAME       = 1
*     OTHERS = 2
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.





ENDFORM.
