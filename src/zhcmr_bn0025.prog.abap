*&------------------------------------------------------------------------------&*
*&                         Consultoria                                          &*
*&------------------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                            &*
*& Autor....: RICARDO PEREIRA                                                   &*
*& Data.....: 09/08/2024                                                        &*
*& Descrição: Relatório Custos Total Plano de Saúde                             &*
*& Transação:                                                                   &*
*&------------------------------------------------------------------------------&*
*& Projeto  :                                                                   &*
*& Código Espec.Funcional/Técnica:                                              &*
*&------------------------------------------------------------------------------&*
*&                    Histórico de Modificações                                 &*
*& Autor           Request      Data         Descrição                          &*
*& ABAP            DEVK9A23WE   09/08/2024   Relatório assistência médica PSA   &*
*&------------------------------------------------------------------------------&*
REPORT zhcmr_bn0025.

"Tables
TABLES: pa0009, pa0001, pa0167, pa0000, s031.

"Types
TYPES: BEGIN OF ty_output,
         empresa                  TYPE pa0001-bukrs,
         desc_empresa             TYPE t001-butxt, "bukrs_text,
         cnpj                     TYPE j_1bbranch-stcd1, "cgc,
         filial                   TYPE pa0001-werks,
         desc_filial              TYPE t001w-name1, "werks_text,
         centro_custo             TYPE pa0001-kostl,
         desc_centro_custo        TYPE cskt-ktext,
         unid_organizacional      TYPE pa0001-orgeh,
         desc_unid_organizacional TYPE hrp1000-stext,
         diretoria                TYPE pa0001-orgeh,
         desc_diretoria           TYPE hrp1000-stext,
         cargo                    TYPE pa0001-stell,
         desc_cargo               TYPE hrp1000-stext,
         matricula                TYPE pa0001-pernr,
         nome                     TYPE pa0002-cname,
         cpf                      TYPE pa0465-cpf_nr,
         situacao                 TYPE t529u-text1,
         data_admissao            TYPE string, " formato 00/00/0000
         data_desligamento        TYPE string, " formato 00/00/0000
         sexo                     TYPE t77pad_gender_t-gender_text,
         tipo_plano               TYPE string, "pa0167-subty, " nome fixado
         tipo_beneficio           TYPE string, "pa0167-bplan, " nome fixado
         cobertura_dep            TYPE pa0167-depcv,
         custo_empregado          TYPE t5ubi-eecst,
         custo_empregador         TYPE t5ubi-ercst,
         custo_prestador          TYPE t5ubi-accst,
         copart_0015              TYPE pa0015-betrg,
         copart_folha             TYPE pa0015-betrg,
         dt_pagto                 TYPE string, "BUG - 150325 - CBRAND
       END OF ty_output.

"Tabela Interna
DATA: it_final TYPE TABLE OF ty_output.

"Work Area
DATA: wa_final TYPE ty_output.

"Field-Symbol
FIELD-SYMBOLS: <fs_alv> TYPE ty_output.

"Variáveis
DATA: lv_n_pess  TYPE string,
      lv_un_org  TYPE string,
      lv_cargo   TYPE string,
      lv_mesanoi TYPE string,
      lv_mesanof TYPE string.

"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      lines                       TYPE sy-tabix,
      wa_selected_rows            TYPE lvc_s_row,
      it_selected_rows            TYPE lvc_t_row.

DATA: gt_fieldcatalog  TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY. "BUG - 150325 - CBRAND

" Classe
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA:  event_receiver   TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .


    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD hotspot_click.
    CASE e_column_id.
      WHEN 'STATUS_PROCESSAMENTO'.
    ENDCASE.

    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.

  METHOD get_ucomm.
    CASE sy-ucomm.
      WHEN 'PROCESSAR'.

        CALL METHOD gob_gui_alv_grid->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        IF ( lines IS INITIAL ).
          MESSAGE TEXT-e01 TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.

        PERFORM fm_processar.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
* BUG - 144013 - Inicio - CBRAND
*p_mesano FOR s031-spmon DEFAULT sy-datum OBLIGATORY ,
*p_n_pess FOR pa0009-pernr  ,
* BUG - 144013 - Fim - CBRAND
                  p_mesano FOR s031-spmon DEFAULT sy-datum OBLIGATORY NO INTERVALS NO-EXTENSION,
                  p_n_pess FOR pa0009-pernr  MATCHCODE OBJECT prem,
                  p_empres FOR pa0001-bukrs,
*                  p_arearh FOR pa0167-barea,
                  p_arearh FOR pa0001-werks,
                  p_stt_oc FOR pa0000-stat2,
                  p_ce_cus FOR pa0001-kostl,
                  p_un_org FOR pa0001-orgeh,
                  p_cargo  FOR pa0001-stell.
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_mesano-low.
  PERFORM monat_f4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_mesano-high.
  PERFORM monat_f4.

INITIALIZATION.

AT SELECTION-SCREEN. "PAI

AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_at_selection_screen.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.

*&---------------------------------------------------------------------*
*&      Form  FM_PROCESSAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_processar .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen .
  LOOP AT SCREEN.
    CASE abap_true.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  CALL SCREEN 0100.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros.
  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

  IF p_mesano-high = '000000'.
    p_mesano-high = p_mesano-low.

    DATA: lv_data_sel  TYPE sy-datum.
    CONCATENATE p_mesano-low '01' INTO lv_mesanoi.
    CONCATENATE p_mesano-low '01' INTO lv_data_sel.
    CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
      EXPORTING
        day_in            = lv_data_sel
      IMPORTING
        last_day_of_month = lv_data_sel
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    lv_mesanof = lv_data_sel.
  ENDIF.

** Seleção na Tabela PA0001
  DATA: wa_pa0001 TYPE pa0001.

  SELECT *
    INTO wa_pa0001
    FROM pa0001
    WHERE pernr IN p_n_pess
      AND bukrs IN p_empres
      AND werks IN p_arearh
      AND kostl IN p_ce_cus
      AND orgeh IN p_un_org
      AND stell IN p_cargo
      AND begda <= lv_mesanof "last_day_of_month(p_mesano)
      AND endda >= lv_mesanoi "first_day_of_month(p_mesano)
      AND plans <> '99999999'.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.

* Seleção na Tabela j_1bbranch
    DATA: wa_1bbranch TYPE j_1bbranch.

    SELECT SINGLE *
      INTO wa_1bbranch
      FROM j_1bbranch
      WHERE bukrs = wa_pa0001-bukrs.

* Seleção na Tabela j_1bbranch
    DATA: lv_name TYPE t500p-name1.

    CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
      EXPORTING
        werks      = wa_pa0001-werks
      IMPORTING
        werks_text = lv_name.

* Seleção na Tabela PA0000
    DATA: wa_pa0000 TYPE pa0000,
          lv_stat2  TYPE pa0000-stat2.

    SELECT *
      FROM pa0000
      INTO wa_pa0000
      WHERE pernr = wa_pa0001-pernr
      AND begda <= lv_mesanof "last_day_of_month(pn_mesano)
      AND endda >= lv_mesanoi "first_day_of_month(pn_mesano).
      ORDER BY endda ASCENDING.
    ENDSELECT.

    IF p_stt_oc-low IS INITIAL.
*        lv_stat2 = .
    ELSE.
      lv_stat2 = p_stt_oc-low.
    ENDIF.

    IF  wa_pa0000-stat2 = lv_stat2 OR lv_stat2 = ''.
* Seleção na Tabela PA0002
      DATA: wa_pa0002 TYPE pa0002.

      SELECT SINGLE *
        INTO wa_pa0002
        FROM pa0002
        WHERE pernr = wa_pa0001-pernr.
*      AND begda <= p_mesano "last_day_of_month(pn_mesano)
*      AND endda >= p_mesano. "first_day_of_month(pn_mesano).

* Filtrar por Status da Ocupação
      IF wa_pa0000-stat2 <> lv_stat2 AND lv_stat2 <> ''.
        " Pular este registro, se não atender ao status de ocupação desejado
      ELSE.
        " Continuar com o processamento

* Busca da Data de Admissão
        DATA: wa_entry_dates TYPE p0000-begda,
              wa_entry_dt    TYPE string.


        CALL FUNCTION 'HR_ENTRY_DATE'
          EXPORTING
            persnr    = wa_pa0001-pernr
          IMPORTING
            entrydate = wa_entry_dates.

        CONCATENATE wa_entry_dates+6(2) '/' wa_entry_dates+4(2) '/' wa_entry_dates(4) INTO wa_entry_dt.

* Busca da Data de Desligamento
        DATA: wa_firedate TYPE p0000-endda,
              wa_fire_dt  TYPE string.

        CALL FUNCTION 'RP_GET_FIRE_DATE'
          EXPORTING
            persnr   = wa_pa0001-pernr
          IMPORTING
            firedate = wa_firedate.

        CONCATENATE wa_firedate+6(2) '/' wa_firedate+4(2) '/' wa_firedate(4) INTO wa_fire_dt.

* Eliminar Registros com Data de Desligamento Antes do Mês/Ano Informado
        DATA: lv_mesano TYPE sy-datum.
        CONCATENATE p_mesano-low '01' INTO lv_mesano.

        IF wa_firedate = '00000000'.
          wa_firedate = '99991231'.
        ENDIF.

        " Pular este registro se a data de desligamento for anterior ao mês/ano
        IF wa_firedate > lv_mesano.
* Busca do Nome da Situação
          DATA wa_txtSit TYPE text1.

          SELECT SINGLE text1
            INTO wa_txtSit
            FROM t529u
            WHERE sprsl = sy-langu
              AND statn = '2'
              AND statv = wa_pa0000-stat2.

* Busca do Nome da Empresa
          DATA wa_txtEmp TYPE butxt.

          CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
            EXPORTING
              bukrs      = wa_pa0001-bukrs
              langu      = sy-langu
            IMPORTING
              bukrs_text = wa_txtEmp.


* Busca do CNPJ da Empresa
          DATA wa_cgc TYPE bapibranch-cgc_number.

          CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
            EXPORTING
              company_code = wa_pa0001-bukrs
              branch       = wa_pa0001-werks
            IMPORTING
              cgc          = wa_cgc.

* Busca do Nome da Filial
          DATA wa_werks_text TYPE t500p-name1.

          CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
            EXPORTING
              werks      = wa_pa0001-werks
            IMPORTING
              werks_text = wa_werks_text.

* Busca do Nome do Centro de Custo
          DATA wa_ccusto TYPE ktext.

          SELECT SINGLE ktext
            INTO wa_ccusto
            FROM cskt
            WHERE kokrs = wa_pa0001-kokrs
              AND kostl = wa_pa0001-kostl
              AND spras = sy-langu
              AND datbi >= sy-datum.

* Busca do Nome da Unidade Organizacional
          DATA wa_unidorg TYPE stext.

          SELECT SINGLE stext
            INTO wa_unidorg
            FROM hrp1000
            WHERE plvar = '01'
              AND otype = 'O'
              AND objid = wa_pa0001-orgeh
              AND begda <= lv_mesanoi "first_day_of_month(pn_mesano)
              AND endda >= lv_mesanoi "last_day_of_month(pn_mesano)
              AND langu = sy-langu.

* Busca do Diretoria
          DATA lt_diretoria TYPE zhcms_dir_ger_t.
          DATA(lo_util) = NEW zcl_hcm_util( ).
          lo_util->get_diretoria(
            EXPORTING
              I_pernr        = wa_pa0001-pernr
              i_data_posicao = lv_data_sel "BUG- 150325 - CBRAND
            IMPORTING
              t_saida        = lt_diretoria ).

          " Passar uma data opcional: lv_mesanof

* Busca do Nome do Cargo
          DATA wa_cargo TYPE hrp1000.
          SELECT SINGLE *
            INTO wa_cargo
            FROM hrp1000
            WHERE plvar = '01'
              AND otype = 'C'
              AND objid = wa_pa0001-stell
              AND begda <= lv_mesanoi "first_day_of_month(pn_mesano)
              AND endda >= lv_mesanoi "last_day_of_month(pn_mesano)
              AND langu = sy-langu.

* Busca do Nome do Empregado e Sexo
          DATA: wa_cname TYPE  cname,
                wa_gesch TYPE gesch.

          SELECT SINGLE cname, gesch
            INTO (@wa_cname, @wa_gesch)
            FROM pa0002
            WHERE pernr = @wa_pa0001-pernr
              AND endda >= @lv_mesanoi. "first_day_of_month(pn_mesano).

          DATA wa_gender_text TYPE t77pad_gender_t.

          SELECT SINGLE *
            INTO wa_gender_text
            FROM t77pad_gender_t
            WHERE spras = sy-langu
              AND molga = '01'
              AND gender = wa_pa0002-gesch.

* Busca do CPF
          DATA wa_cpf_nr TYPE pa0465.

          SELECT SINGLE *
            INTO wa_cpf_nr
            FROM pa0465
            WHERE pernr = wa_pa0001-pernr
              AND subty = '0001'.

* Busca dos Dados do Plano de Saúde
          DATA: ls_pa0167 TYPE pa0167,
                lt_pa0167 TYPE TABLE OF pa0167.

          SELECT *
            INTO TABLE lt_pa0167
            FROM pa0167
            WHERE pernr = wa_pa0001-pernr
              AND subty IN ('MED1', 'MED2', 'MED3')
              AND begda <= lv_mesanoi "first_day_of_month(pn_mesano)
              AND endda >= lv_mesanoi. "last_day_of_month(pn_mesano).


* Busca do Custo
          DATA: wa_t5ubi TYPE t5ubi,
                lv_subty TYPE string,
                lv_bplan TYPE string.


          LOOP AT lt_pa0167 INTO ls_pa0167.

            IF ls_pa0167-subty = 'MED1'.
              lv_subty = 'Bradesco'.
* BUG - 150325 - CBRAND - Inicio
              IF ls_pa0167-bopti = 'NP04'.
                lv_bplan = 'Enfermaria'.
              ENDIF.

              IF ls_pa0167-bopti = 'NP06'.
                lv_bplan = 'Apartamento'.
              ENDIF.

*              IF ls_pa0167-bplan = 'NP04'.
*                lv_bplan = 'Enfermaria'.
*              ENDIF.
*
*              IF ls_pa0167-bplan = 'NP06'.
*                lv_bplan = 'Apartamento'.
*              ENDIF.
* BUG - 150325 - CBRAND - Fim
            ENDIF.

            IF ls_pa0167-subty = 'MED2'.
              lv_subty = 'Unimed'.

* BUG - 150325 - CBRAND - Inicio

              IF ls_pa0167-bopti = 'APAR'.
                lv_bplan = 'Apartamento'.
              ENDIF.

              IF ls_pa0167-bopti = 'ENFE'.
                lv_bplan = 'Enfermaria'  .
              ENDIF.

*              IF ls_pa0167-bplan = 'MED3'.
*                lv_bplan = 'Apartamento'.
*              ENDIF.
*
*              IF ls_pa0167-bplan = 'MED4'.
*                lv_bplan = 'Enfermaria'  .
*              ENDIF.
* BUG - 150325 - CBRAND - Fim
            ENDIF.

            IF ls_pa0167-subty = 'MED3'.
              lv_subty = 'Garantia Saúde'.
* BUG - 150325 - CBRAND - Inicio

              IF ls_pa0167-bopti = 'APAR'.
                lv_bplan = 'GS Apartamento'.
              ENDIF.

              IF ls_pa0167-bopti = 'ENFE'.
                lv_bplan = 'GS Enfermaria'.
              ENDIF.

*              IF ls_pa0167-bplan = 'MEE1'.
*                lv_bplan = 'GS Apartamento'.
*              ENDIF.
*
*              IF ls_pa0167-bplan = 'MEE2'.
*                lv_bplan = 'GS Enfermaria'.
*              ENDIF.
* BUG - 150325 - CBRAND - Fim
            ENDIF.

* Se bplan = 'np04' ou 'np06', substituir por 'MED1' na seleção abaixo.
* BUG - 150325 - CBRAND - Inicio
            "            IF ls_pa0167-bplan = 'np04' OR ls_pa0167-bplan = 'np06'.
            IF ls_pa0167-bopti = 'NP04' OR ls_pa0167-bopti = 'NP06'.
              ls_pa0167-bplan = 'MED1'.
            ENDIF.
* BUG - 150325 - CBRAND - Fim

* Busca Custo do Plano de saúde
            SELECT SINGLE *
              INTO wa_t5ubi
              FROM t5ubi
              WHERE barea = ls_pa0167-barea
                AND bplan = ls_pa0167-bplan
                AND bcost = ls_pa0167-depcv
                AND begda <= lv_mesanoi "first_day_of_month(pn_mesano)
                AND endda >= lv_mesanof. "last_day_of_month(pn_mesano).

            MODIFY lt_pa0167 FROM ls_pa0167 INDEX sy-tabix.

          ENDLOOP.

* Busca de Coparticipações e Soma dos Valores
          DATA: ls_pa0015     TYPE pa0015,
                lt_pa0015     TYPE TABLE OF pa0015,
                lv_soma_betrg TYPE string.

          SELECT *
            FROM pa0015
            INTO ls_pa0015
            WHERE pernr = wa_pa0001-pernr
              AND subty IN ('1044', '3907', '3911', '3912')
                AND begda >= lv_mesanoi "first_day_of_month(pn_mesano)
                AND endda <= lv_mesanof. "last_day_of_month(pn_mesano).

            IF ls_pa0015-subty = '1044'.
              lv_soma_betrg = lv_soma_betrg + ls_pa0015-betrg.
            ENDIF.
            IF ( ls_pa0015-subty = '3907' OR ls_pa0015-subty = '3911' OR ls_pa0015-subty = '3912' ).
              lv_soma_betrg = lv_soma_betrg - ls_pa0015-betrg.
            ENDIF.

          ENDSELECT.

* BUG - 150325 - CBRAND - Inicio
** buscar rubricas atreladas na conta 421108 - C011
          SELECT * FROM t52el
            INTO TABLE @DATA(lit_T52EL)
            WHERE molga = '37'
             AND symko = 'C011' .

**** CLUSTER FOLHA
          DATA: cl_read_payroll TYPE REF TO cl_hr_br_read_payroll,
                lit_rgdir       TYPE TABLE OF pc261,
                lwa_rgdir       TYPE pc261,
                lit_payroll     TYPE TABLE OF paybr_result,
                lwa_payroll     TYPE          paybr_result,
                lit_rt          TYPE TABLE OF pc207,
                lwa_rt          TYPE pc207,
                lva_soma_betrg  TYPE string,
                lva_soma_folha  TYPE string,
                lva_betrg       TYPE p0008-bet01,
                lva_begda       TYPE sy-datum,
                lva_endda       TYPE sy-datum,
                lva_paydt       TYPE string.
* Objeto
          CLEAR: cl_read_payroll.
          CREATE OBJECT cl_read_payroll
            EXPORTING
              iv_pernr = wa_pa0001-pernr.

* Lista de Clusters
          REFRESH: lit_rgdir.

          lva_begda  = lv_mesanoi.
          lva_endda  = lv_mesanof.
          CALL METHOD cl_read_payroll->get_rgdir
            EXPORTING
              iv_begda = lva_begda
              iv_endda = lva_endda
            IMPORTING
              et_rgdir = lit_rgdir.

          DELETE lit_rgdir WHERE ocrsn <>  ''.

          CLEAR:lwa_rgdir.
          READ TABLE lit_rgdir INTO lwa_rgdir INDEX 1.

          CONCATENATE  lwa_rgdir-paydt+6(2) '/'  lwa_rgdir-paydt+4(2) '/'  lwa_rgdir-paydt(4) INTO lva_paydt.

          REFRESH lit_payroll.
          CALL METHOD cl_read_payroll->get_pay_result_table
            EXPORTING
              it_rgdir        = lit_rgdir
            IMPORTING
              et_paybr_result = lit_payroll.

          REFRESH: lit_rt.

          CLEAR: lwa_payroll.
          LOOP AT lit_payroll INTO lwa_payroll.
            APPEND LINES OF lwa_payroll-inter-rt TO lit_rt.
          ENDLOOP.

          LOOP AT lit_t52el INTO DATA(lwa_t52el).
            READ TABLE lit_rt INTO lwa_rt WITH KEY lgart = lwa_t52el-lgart .
            IF sy-subrc = 0.
              IF lwa_t52el-sign = '-'.
                lva_betrg =  lva_betrg + ( lwa_rt-betrg * -1 ).
              ELSE.
                lva_betrg =  lva_betrg +  lwa_rt-betrg .
              ENDIF.
            ENDIF.
            CLEAR: lwa_t52el, lwa_rt.
          ENDLOOP.

          lva_soma_folha = lva_betrg.
* BUG - 150325 - CBRAND - Fim

          LOOP AT lt_diretoria INTO DATA(ls_diretoria).
          ENDLOOP.

*Preenchimento da Tabela Interna it_final
          wa_final-empresa                  = wa_1bbranch-bukrs.
          wa_final-desc_empresa             = wa_1bbranch-name.
          wa_final-cnpj                     = wa_cgc. "wa_1bbranch-stcd1.
          wa_final-filial                   = wa_pa0001-werks.
          wa_final-desc_filial              = lv_name .
          wa_final-centro_custo             = wa_pa0001-kostl.
          wa_final-desc_centro_custo        = wa_ccusto.
          wa_final-unid_organizacional      = wa_pa0001-orgeh.
          wa_final-desc_unid_organizacional = wa_unidorg.
          wa_final-diretoria                = ls_diretoria-orgeh.
          wa_final-desc_diretoria           = ls_diretoria-orgetxt.
          wa_final-cargo                    = wa_pa0001-stell.
          wa_final-desc_cargo               = wa_cargo-stext.
          wa_final-matricula                = wa_pa0001-pernr.
          wa_final-nome                     = wa_pa0002-cname.
          wa_final-cpf                      = wa_cpf_nr-cpf_nr.
          wa_final-situacao                 = wa_txtSit.
          wa_final-data_admissao            = wa_entry_dt.
          wa_final-data_desligamento        = wa_fire_dt.
          wa_final-sexo                     = wa_gender_text-gender_text.
          wa_final-tipo_plano               = lv_subty. "ls_pa0167-subty.
          wa_final-tipo_beneficio           = lv_bplan. "ls_pa0167-bplan.
          wa_final-cobertura_dep            = ls_pa0167-depcv.
          wa_final-custo_empregado          = wa_t5ubi-eecst.
          wa_final-custo_empregador         = wa_t5ubi-ercst.
          wa_final-custo_prestador          = wa_t5ubi-accst.
          wa_final-copart_0015              = lv_soma_betrg.
          wa_final-copart_folha             = lva_soma_folha.
          wa_final-dt_pagto                 = lva_paydt .

          APPEND wa_final TO it_final.
        ENDIF.
      ENDIF.
    ENDIF.

    "Limpando varáveis
    CLEAR wa_1bbranch.
    CLEAR wa_pa0001.
    CLEAR lv_name.
    CLEAR wa_ccusto.
    CLEAR wa_unidorg.
    CLEAR wa_cargo.
    CLEAR wa_pa0002.
    CLEAR wa_cpf_nr.
    CLEAR wa_txtSit.
    CLEAR wa_entry_dt.
    CLEAR wa_fire_dt.
    CLEAR wa_gender_text.
    CLEAR ls_pa0167.
    CLEAR wa_t5ubi.
    CLEAR lva_soma_betrg.
    CLEAR wa_final.
* BUG - 150325 - CBRAND - Inicio
    CLEAR: lv_subty.
    CLEAR: lv_bplan.
    CLEAR: lva_paydt.
    CLEAR: lva_betrg.
    CLEAR: lva_soma_folha.
    CLEAR: lv_soma_betrg.
* BUG - 150325 - CBRAND - Fim

  ENDSELECT.

** BUG - 150325 - CBRAND - Inicio
*  DATA: field_body TYPE slis_t_fieldcat_alv.
*  DATA: gt_fieldcatalog TYPE slis_t_fieldcat_alv.
*  DATA: gs_fieldcatalog TYPE slis_fieldcat_alv.
*
*  DATA: da_layout  TYPE slis_layout_alv,
*        da_repid   LIKE sy-repid,
*        da_variant TYPE disvariant.
*
*  CLEAR gt_fieldcatalog.
*  PERFORM fm_cria_fieldcat TABLES gt_fieldcatalog.

* Configuração e Exibição do ALV Grid
*  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
*    EXPORTING
*      i_structure_name = 'TY_OUTPUT'
*      it_fieldcat      = gt_fieldcatalog
*    TABLES
*      t_outtab         = it_final.

*** BUG - 150325 - CBRAND - Fim

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat.
*** BUG - 150325 - CBRAND - Inicio
*  FORM fm_cria_fieldcat TABLES gt_fieldcatalog.
*  DATA: field_body TYPE slis_t_fieldcat_alv.
*  DATA: gt_fieldcatalog TYPE slis_t_fieldcat_alv.
*  DATA: gs_fieldcatalog TYPE slis_fieldcat_alv.

*  gs_fieldcatalog-fieldname     = 'EMPRESA'.
*  gs_fieldcatalog-ref_tabname   = 'IT_FINAL'.
*  gs_fieldcatalog-ref_fieldname = 'EMPRESA'.
*  gs_fieldcatalog-seltext_l     = 'Empresa'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname      = 'DESC_EMPRESA'.
*  gs_fieldcatalog-ref_tabname    = 'DESC_EMPRESA'.
*  gs_fieldcatalog-ref_fieldname  = 'DESC_EMPRESA'.
*  gs_fieldcatalog-seltext_l      = 'Desc. Empresa'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname      = 'CNPJ'.
*  gs_fieldcatalog-ref_tabname    = 'CNPJ'.
*  gs_fieldcatalog-ref_fieldname  = 'CNPJ'.
*  gs_fieldcatalog-seltext_l      = 'CNPJ'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'FILIAL'.
*  gs_fieldcatalog-ref_tabname     = 'FILIAL'.
*  gs_fieldcatalog-ref_fieldname   = 'FILIAL'.
*  gs_fieldcatalog-seltext_l       = 'Filial'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DESC_FILIAL'.
*  gs_fieldcatalog-ref_tabname     = 'DESC_FILIAL'.
*  gs_fieldcatalog-ref_fieldname   = 'DESC_FILIAL'.
*  gs_fieldcatalog-seltext_l       = 'Descr. Filial'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'CENTRO_CUSTO'.
*  gs_fieldcatalog-ref_tabname     = 'CENTRO_CUSTO'.
*  gs_fieldcatalog-ref_fieldname   = 'CENTRO_CUSTO'.
*  gs_fieldcatalog-seltext_l       = 'Centro de Custo'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DESC_CENTRO_CUSTO'.
*  gs_fieldcatalog-ref_tabname     = 'DESC_CENTRO_CUSTO'.
*  gs_fieldcatalog-ref_fieldname   = 'DESC_CENTRO_CUSTO'.
*  gs_fieldcatalog-seltext_l       = 'Desc. Centro de Custo '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'UNID_ORGANIZACIONAL'.
*  gs_fieldcatalog-ref_tabname     = 'UNID_ORGANIZACIONAL'.
*  gs_fieldcatalog-ref_fieldname   = 'UNID_ORGANIZACIONAL'.
*  gs_fieldcatalog-seltext_l       = 'Unidade Organizacional '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DESC_UNID_ORGANIZACIONAL'.
*  gs_fieldcatalog-ref_tabname     = 'DESC_UNID_ORGANIZACIONAL'.
*  gs_fieldcatalog-ref_fieldname   = 'DESC_UNID_ORGANIZACIONAL'.
*  gs_fieldcatalog-seltext_l       = 'Desc. Unidade Organizacional '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DIRETORIA'.
*  gs_fieldcatalog-ref_tabname     = 'DIRETORIA'.
*  gs_fieldcatalog-ref_fieldname   = 'DIRETORIA'.
*  gs_fieldcatalog-seltext_l       = 'Diretoria'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DESC_DIRETORIA'.
*  gs_fieldcatalog-ref_tabname     = 'DESC_DIRETORIA'.
*  gs_fieldcatalog-ref_fieldname   = 'DESC_DIRETORIA'.
*  gs_fieldcatalog-seltext_l       = 'Desc. Diretoria'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'CARGO'.
*  gs_fieldcatalog-ref_tabname     = 'CARGO'.
*  gs_fieldcatalog-ref_fieldname   = 'CARGO'.
*  gs_fieldcatalog-seltext_l       = 'Cargo'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DESC_CARGO'.
*  gs_fieldcatalog-ref_tabname     = 'DESC_CARGO'.
*  gs_fieldcatalog-ref_fieldname   = 'DESC_CARGO'.
*  gs_fieldcatalog-seltext_l       = 'Desc. Cargo'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'MATRICULA'.
*  gs_fieldcatalog-ref_tabname     = 'MATRICULA'.
*  gs_fieldcatalog-ref_fieldname   = 'MATRICULA'.
*  gs_fieldcatalog-seltext_l       = 'Matrícula'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'NOME'.
*  gs_fieldcatalog-ref_tabname     = 'NOME'.
*  gs_fieldcatalog-ref_fieldname   = 'NOME'.
*  gs_fieldcatalog-seltext_l       = 'Nome'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname        = 'CPF'.
*  gs_fieldcatalog-ref_tabname      = 'CPF'.
*  gs_fieldcatalog-ref_fieldname    = 'CPF'.
*  gs_fieldcatalog-seltext_l        = 'CPF'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'SITUACAO'.
*  gs_fieldcatalog-ref_tabname     = 'SITUACAO'.
*  gs_fieldcatalog-ref_fieldname   = 'SITUACAO'.
*  gs_fieldcatalog-seltext_l       = 'Situação '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'DATA_ADMISSAO'.
*  gs_fieldcatalog-ref_tabname     = 'DATA_ADMISSAO'.
*  gs_fieldcatalog-ref_fieldname   = 'DATA_ADMISSAO'.
*  gs_fieldcatalog-seltext_l       = 'Data Admissão '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname        = 'DATA_DESLIGAMENTO'.
*  gs_fieldcatalog-ref_tabname      = 'DATA_DESLIGAMENTO'.
*  gs_fieldcatalog-ref_fieldname    = 'DATA_DESLIGAMENTO'.
*  gs_fieldcatalog-seltext_l        = 'Data Desligamento '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname        = 'SEXO'.
*  gs_fieldcatalog-ref_tabname      = 'SEXO'.
*  gs_fieldcatalog-ref_fieldname    = 'SEXO'.
*  gs_fieldcatalog-seltext_l        = 'Sexo'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'TIPO_PLANO'.
*  gs_fieldcatalog-ref_tabname     = 'TIPO_PLANO'.
*  gs_fieldcatalog-ref_fieldname   = 'TIPO_PLANO'.
*  gs_fieldcatalog-seltext_l       = 'Tipo Plano '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'TIPO_BENEFICIO'.
*  gs_fieldcatalog-ref_tabname     = 'TIPO_BENEFICIO'.
*  gs_fieldcatalog-ref_fieldname   = 'TIPO_BENEFICIO'.
*  gs_fieldcatalog-seltext_l       = 'Tipo Benefício'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'COBERTURA_DEP'.
*  gs_fieldcatalog-ref_tabname     = 'COBERTURA_DEP'.
*  gs_fieldcatalog-ref_fieldname   = 'COBERTURA_DEP'.
*  gs_fieldcatalog-seltext_l       = 'Cobertura Dep.'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'CUSTO_EMPREGADO'.
*  gs_fieldcatalog-ref_tabname     = 'CUSTO_EMPREGADO'.
*  gs_fieldcatalog-ref_fieldname   = 'CUSTO_EMPREGADO'.
*  gs_fieldcatalog-seltext_l       = 'Custo Empregado'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname        = 'CUSTO_EMPREGADOR'.
*  gs_fieldcatalog-ref_tabname      = 'CUSTO_EMPREGADOR'.
*  gs_fieldcatalog-ref_fieldname    = 'CUSTO_EMPREGADOR'.
*  gs_fieldcatalog-seltext_l        = 'Custo Empregador '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'CUSTO_PRESTADOR'.
*  gs_fieldcatalog-ref_tabname     = 'CUSTO_PRESTADOR'.
*  gs_fieldcatalog-ref_fieldname   = 'CUSTO_PRESTADOR'.
*  gs_fieldcatalog-seltext_l       = 'Custo Prestador '.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'COPARTICIPACAO'.
*  gs_fieldcatalog-ref_tabname     = 'COPARTICIPACAO'.
*  gs_fieldcatalog-ref_fieldname   = 'COPARTICIPACAO'.
*  gs_fieldcatalog-seltext_l       = 'coparticipação (0015)'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
*  gs_fieldcatalog-fieldname       = 'COPARTICIPACAO'.
*  gs_fieldcatalog-ref_tabname     = 'COPARTICIPACAO'.
*  gs_fieldcatalog-ref_fieldname   = 'COPARTICIPACAO'.
*  gs_fieldcatalog-seltext_l       = 'coparticipação (folha)'.
*  APPEND gs_fieldcatalog TO gt_fieldcatalog.
*
  TYPES:  lit_fieldcat_aux   TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  gt_fieldcatalog = VALUE lit_fieldcat_aux(
  ( fieldname = 'EMPRESA'                     coltext =  'Empresa'                        col_opt = 'X' no_zero = '' )
  ( fieldname = 'DESC_EMPRESA'                coltext =  'Desc. Empresa'                  col_opt = 'X' no_zero = '' )
  ( fieldname = 'CNPJ'                        coltext =  'CNPJ'                           col_opt = 'X' no_zero = '' )
  ( fieldname = 'FILIAL'                      coltext =  'Filial'                         col_opt = 'X' no_zero = '' )
  ( fieldname = 'DESC_FILIAL'                 coltext =  'Descr. Filial'                  col_opt = 'X' no_zero = '' )
  ( fieldname = 'CENTRO_CUSTO'                coltext =  'Centro de Custo'                col_opt = 'X' no_zero = '' )
  ( fieldname = 'DESC_CENTRO_CUSTO'           coltext =  'Desc. Centro de Custo'          col_opt = 'X' no_zero = '' )
  ( fieldname = 'UNID_ORGANIZACIONAL'         coltext =  'Unidade Organizacional'         col_opt = 'X' no_zero = '' )
  ( fieldname = 'DESC_UNID_ORGANIZACIONAL'    coltext =  'Desc. Unidade Organizacional'   col_opt = 'X' no_zero = '' )
  ( fieldname = 'DIRETORIA'                   coltext =  'Diretoria'                      col_opt = 'X' no_zero = '' )
  ( fieldname = 'DESC_DIRETORIA'              coltext =  'Desc. Diretoria'                col_opt = 'X' no_zero = '' )
  ( fieldname = 'CARGO'                       coltext =  'Cargo'                          col_opt = 'X' no_zero = '' )
  ( fieldname = 'DESC_CARGO'                  coltext =  'Desc. Cargo'                    col_opt = 'X' no_zero = '' )
  ( fieldname = 'MATRICULA'                   coltext =  'Matrícula'                      col_opt = 'X' no_zero = '' )
  ( fieldname = 'NOME'                        coltext =  'Nome'                           col_opt = 'X' no_zero = '' )
  ( fieldname = 'CPF'                         coltext =  'CPF'                            col_opt = 'X' no_zero = '' )
  ( fieldname = 'SITUACAO'                    coltext =  'Situação'                       col_opt = 'X' no_zero = '' )
  ( fieldname = 'DATA_ADMISSAO'               coltext =  'Data Admissão'                  col_opt = 'X' no_zero = '' )
  ( fieldname = 'DATA_DESLIGAMENTO'           coltext =  'Data Desligamento'              col_opt = 'X' no_zero = '' )
  ( fieldname = 'SEXO'                        coltext =  'Sexo'                           col_opt = 'X' no_zero = '' )
  ( fieldname = 'TIPO_PLANO'                  coltext =  'Tipo Plano'                     col_opt = 'X' no_zero = '' )
  ( fieldname = 'TIPO_BENEFICIO'              coltext =  'Tipo Benefício'                 col_opt = 'X' no_zero = '' )
  ( fieldname = 'COBERTURA_DEP'               coltext =  'Cobertura Dep.'                 col_opt = 'X' no_zero = '' )
  ( fieldname = 'CUSTO_EMPREGADO'             coltext =  'Custo Empregado'                col_opt = 'X' no_zero = '' )
  ( fieldname = 'CUSTO_EMPREGADOR'            coltext =  'Custo Empregador'               col_opt = 'X' no_zero = '' )
  ( fieldname = 'CUSTO_PRESTADOR'             coltext =  'Custo Prestador'                col_opt = 'X' no_zero = '' )
  ( fieldname = 'COPART_0015'                 coltext =  'coparticipação (0015)'          col_opt = 'X' no_zero = '' )
  ( fieldname = 'COPART_FOLHA'                coltext =  'coparticipação (folha)'         col_opt = 'X' no_zero = '' )
  ( fieldname = 'DT_PAGTO'                    coltext =  'data contb. (folha)'            col_opt = 'X' no_zero = '' )
  ).
*** BUG - 150325 - CBRAND - Fim
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  monat_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM monat_f4 .
  DATA: BEGIN OF mf_dynpfields OCCURS 1.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF mf_dynpfields.
  DATA: mf_returncode LIKE sy-subrc,
        mf_monat      LIKE isellist-month,
        mf_hlp_repid  LIKE sy-repid.
  FIELD-SYMBOLS: <mf_feld>.

* Wert von Dynpro lesen
  GET CURSOR FIELD mf_dynpfields-fieldname.
  APPEND mf_dynpfields.
  mf_hlp_repid = sy-repid.
  DO 2 TIMES.
    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = mf_hlp_repid
        dynumb               = sy-dynnr
      TABLES
        dynpfields           = mf_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 01
        invalid_dynprofield  = 02
        invalid_dynproname   = 03
        invalid_dynpronummer = 04
        invalid_request      = 05
        no_fielddescription  = 06
        undefind_error       = 07.
    IF sy-subrc = 3.
*     Aktuelles Dynpro ist Wertemengenbild
      mf_hlp_repid = 'SAPLALDB'.
    ELSE.
      READ TABLE mf_dynpfields INDEX 1.
*     Unterstriche durch Blanks ersetzen
      TRANSLATE mf_dynpfields-fieldvalue USING '_ '.
      EXIT.
    ENDIF.
  ENDDO.
  IF sy-subrc = 0.
*   Konvertierung ins interne Format
    CALL FUNCTION 'CONVERSION_EXIT_PERI_INPUT'
      EXPORTING
        input         = mf_dynpfields-fieldvalue
      IMPORTING
        output        = mf_monat
      EXCEPTIONS
        error_message = 1.
    IF mf_monat IS INITIAL.
*     Monat ist initial => Vorschlagswert aus akt. Datum ableiten
      mf_monat = sy-datlo(6).
    ENDIF.
    CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
      EXPORTING
        actual_month               = mf_monat
      IMPORTING
        selected_month             = mf_monat
        return_code                = mf_returncode
      EXCEPTIONS
        factory_calendar_not_found = 01
        holiday_calendar_not_found = 02
        month_not_found            = 03.
    IF sy-subrc = 0 AND mf_returncode = 0.
*     ASSIGN (MF_DYNPFIELDS-FIELDNAME) TO <MF_FELD>. " ==>> note 148804
*     <MF_FELD> = MF_MONAT.
      CALL FUNCTION 'CONVERSION_EXIT_PERI_OUTPUT'
        EXPORTING
          input  = mf_monat
        IMPORTING
          output = mf_dynpfields-fieldvalue.
      COLLECT mf_dynpfields.
      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = mf_hlp_repid
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = mf_dynpfields
        EXCEPTIONS
          invalid_abapworkarea = 01
          invalid_dynprofield  = 02
          invalid_dynproname   = 03
          invalid_dynpronummer = 04
          invalid_request      = 05
          no_fielddescription  = 06
          undefind_error       = 07.           "<<== note 148804
    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos.

  DATA: field_body TYPE slis_t_fieldcat_alv.
  "DATA: gt_fieldcatalog TYPE slis_t_fieldcat_alv.
  DATA: gs_fieldcatalog TYPE slis_fieldcat_alv.

  DATA: da_layout  TYPE slis_layout_alv,
        da_repid   LIKE sy-repid,
        da_variant TYPE disvariant.


  DATA: lva_data(22)    TYPE c,
        lva_periodo(22) TYPE c,
        w_layout        TYPE lvc_s_layo.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.
  CONCATENATE p_mesano-low+4(2) '/'  p_mesano-low+0(4) INTO lva_periodo.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Relatório Assistência Médica'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data execução:' valor = lva_data )
                                                    ( parametro = 'Período Processado:' valor =  lva_periodo ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.
*
*    w_layout-cwidth_opt = abap_true.
*    w_layout-zebra      = 'X'.
*    w_layout-sel_mode   = 'A'.
*    w_layout-col_opt    = abap_true.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
*      EXPORTING
*        is_layout                     = w_layout
*        i_save                        = 'A'
*        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = it_final
        it_fieldcatalog               = gt_fieldcatalog
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ST0200'.
  SET TITLEBAR 'TIT0200'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

*  PERFORM caixa_txt_obs.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'STATUS'.
*  DATA fcode TYPE TABLE OF sy-ucomm.
*
**  IF rb_proc IS NOT INITIAL.
*  APPEND 'PROCESSAR' TO fcode.
**  ENDIF.
*
*  SET PF-STATUS 'PF0100' EXCLUDING fcode.
*  SET TITLEBAR 'TB0100' WITH 'Integração Abastecimentos'.

  PERFORM fm_criar_objetos.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0  .
    WHEN 'CANCEL'  .
      LEAVE TO SCREEN 0.
  ENDCASE.
*  CASE sy-ucomm.
*    WHEN 'BACK'.
*      LEAVE TO SCREEN 0.
*
*    WHEN 'PROCESSAR'.
**      IF git_zpm0058 IS NOT INITIAL.
*      PERFORM fm_processar.
**      ENDIF.
*
*  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
