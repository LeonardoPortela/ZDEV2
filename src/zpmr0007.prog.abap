*&---------------------------------------------------------------------*
*& Report  ZPMR0007
*& Custos de Equipamento com Manutenção por classe de custo
*&---------------------------------------------------------------------*
*& Autor:    Marcos Faneli
*& Analista: Cleudo Ferreira
*& Data:     06.10.2014
*&---------------------------------------------------------------------*

REPORT  zpmr0007.

INCLUDE <cl_alv_control>.
TABLES: equi, itob, cosp, aufk, t247, coep, fleet.

FIELD-SYMBOLS: <fs_cosp>  TYPE cosp,
               <fs_field> TYPE any.

* Estruturas
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_cosp.
         INCLUDE TYPE cosp.
TYPES: END OF ty_cosp.

TYPES: BEGIN OF ty_equi,
         equnr         TYPE equi-equnr,
         typbz         TYPE equi-typbz,
         eqart         TYPE equi-eqart,
         objnr         TYPE equi-objnr,
         swerk         TYPE v_equi-swerk,
         baujj         TYPE v_equi-baujj,
         iwerk         TYPE v_equi-iwerk,
         datbi         TYPE v_equi-datbi,
         datab         TYPE v_equi-datab,
         timbi         TYPE v_equi-timbi,
         timei         TYPE v_equi-timbi,
         herst         TYPE v_equi-herst,
         zzidade_eqpto TYPE p,
       END OF ty_equi,

       BEGIN OF ty_aufk,
         equnr TYPE viaufkst-equnr,
         aufnr TYPE viaufkst-aufnr,
         objnr TYPE viaufkst-objnr,
         erdat TYPE viaufkst-erdat,
         ktext TYPE viaufkst-ktext,
         auart TYPE viaufkst-auart,
       END OF ty_aufk,

       BEGIN OF ty_viaufkst,
         equnr TYPE viaufkst-equnr,
         aufnr TYPE viaufkst-aufnr,
         objnr TYPE viaufkst-objnr,
         erdat TYPE viaufkst-erdat,
         ktext TYPE viaufkst-ktext,
         auart TYPE viaufkst-auart,
       END OF ty_viaufkst,

       BEGIN OF ty_afih,
         equnr TYPE equi-equnr,
         aufnr TYPE viaufkst-aufnr,
         objnr TYPE afvc-objnr,
       END OF ty_afih,

       BEGIN OF ty_mseg,
         aufnr TYPE mseg-aufnr,
         matnr TYPE mseg-matnr,
*        SAKNR TYPE ZMMT0039-SAKNR,
         dmbtr TYPE mseg-dmbtr,
       END OF ty_mseg,

       BEGIN OF ty_0039,
         matnr TYPE mara-matnr,
         matkl TYPE mara-matkl,
         saknr TYPE zmmt0039-saknr,
       END OF ty_0039,

       BEGIN OF ty_imrg,
         mpobj TYPE imptt-mpobj,
         mdocm TYPE imrg-mdocm,
         point TYPE imrg-point,
         recdu TYPE imrg-recdu,
         cdiff TYPE imrg-cdiff,
         idate TYPE imrg-idate,
         itime TYPE imrg-itime,
       END OF ty_imrg,

       BEGIN OF ty_cosp_lin,
         objnr TYPE cosp-objnr,
         kstar TYPE cosp-kstar,
         tipo  TYPE c LENGTH 10,
         gjahr TYPE cosp-gjahr,
         cont  TYPE i,
         valor TYPE imrc_totac,
       END OF ty_cosp_lin,

       BEGIN OF ty_cskb,
         kstar TYPE cskb-kstar,
         mctxt TYPE m_kartc-mctxt,
       END OF ty_cskb,

       BEGIN OF ty_t370k,
         eqart TYPE t370k_t-eqart,
         eartx TYPE t370k_t-eartx,
       END OF ty_t370k,

       BEGIN OF ty_saida,
         aufnr         TYPE viaufkst-aufnr,
         equnr         TYPE equi-equnr,    " Nº Equipamento
         eartx         TYPE t370k_t-eartx,    " Tipo do Equipamento
         tipbz         TYPE equi-typbz,    " Modelo
         swerk         TYPE itob-swerk,    " Centro
         periodo       TYPE c LENGTH 7,    " Centro
         vl_cont       TYPE p DECIMALS 2,  " Horas trabalhadas
         vl_abast      TYPE p DECIMALS 2,  " Consumo
         custo_hr      TYPE p DECIMALS 2,  " Custo da Hora
         cons_med      TYPE p DECIMALS 2,  " Consumo Médio
         vl_comb       TYPE p DECIMALS 2,  " Valor combustível
         vl_exter      TYPE p DECIMALS 2,  " Valor ofi. externo
         vl_inter      TYPE p DECIMALS 2,  " Valor ofi. interna
         vl_fl_lb      TYPE p DECIMALS 2,  " Valor filtro/lubrificante
         vl_peca       TYPE p DECIMALS 2,  " Valor peças/acessórios
         vl_pneu       TYPE p DECIMALS 2,  " Valor pneus
         vl_mate       TYPE p DECIMALS 2,  " Valor materias
         vl_terc       TYPE p DECIMALS 2,  " Valor terceiros
         vl_total      TYPE p DECIMALS 2,  " total
         cell_color    TYPE lvc_t_scol,    " Cor da Célula
         datab         TYPE v_equi-datab,
         datbi         TYPE v_equi-datbi,
         timbi         TYPE v_equi-timbi,
         zzidade_eqpto TYPE fleet-zzidade_eqpto,
         safra         TYPE zpmt0072-safra,
         herst         TYPE equi-herst, "US #175265 - MMSILVA - 29.04.2025
       END OF ty_saida,

       BEGIN OF ty_detail,
         orderid    TYPE aufk-aufnr,
         tipo       TYPE char10,
         order_type TYPE aufk-auart,
         enter_date TYPE aufk-erdat,
         short_text TYPE c LENGTH 40,
         vl_order   TYPE p DECIMALS 2,
       END OF ty_detail.

TYPES: BEGIN OF ty_covp.
         INCLUDE STRUCTURE covp.
TYPES:   valor TYPE imrc_totac.
TYPES: END OF ty_covp.

* Tabelas internas
*----------------------------------------------------------------------*
DATA: it_equi          TYPE TABLE OF ty_equi,
      it_equi_aux      TYPE TABLE OF ty_equi,
      it_imrg_comb     TYPE TABLE OF ty_imrg,
      it_imrg_comb_aux TYPE TABLE OF ty_imrg,
      it_aufk          TYPE TABLE OF ty_aufk,
      it_aufk_aux      TYPE TABLE OF ty_viaufkst,
      it_viaufkst      TYPE TABLE OF ty_viaufkst,
      it_afih          TYPE TABLE OF ty_afih,
      it_imrg_cont     TYPE TABLE OF ty_imrg,
      it_imrg_cont_aux TYPE TABLE OF ty_imrg,
      it_mseg          TYPE TABLE OF ty_mseg,
      it_0039          TYPE TABLE OF ty_0039,
      it_cosp          TYPE TABLE OF ty_cosp,
      it_cosp_lin      TYPE TABLE OF ty_cosp_lin,
      it_cskb          TYPE TABLE OF ty_cskb,
      it_cskb_sel      TYPE TABLE OF ty_cskb,
      it_t370k         TYPE TABLE OF ty_t370k,
      it_zpmt0072      TYPE TABLE OF zpmt0072,
      ws_zpmt0072      TYPE  zpmt0072,
      it_saida         TYPE TABLE OF ty_saida,
      it_detail        TYPE TABLE OF ty_detail,
      it_covp          TYPE TABLE OF ty_covp,

      it_covp_aux      TYPE TABLE OF ty_covp.
*      IT_RETURN     TYPE TABLE OF BAPIRET2,
*      IT_RANGES     TYPE TABLE OF BAPI_ALM_ORDER_LISTHEAD_RANGES,
*      IT_RESULT     TYPE TABLE OF BAPI_ALM_ORDER_LISTHEAD_RESULT WITH HEADER LINE.

DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata,
      p_resp, check, p_erro(1),
      wa_bdcdata LIKE LINE OF ti_bdcdata.

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: BEGIN OF  r_objnr1 OCCURS 0,
        sign(1)   TYPE c,
        option(2) TYPE c,
        low(10)   TYPE c,
        high(10)  TYPE c.
DATA: END OF r_objnr1.


* Work Áreas
*----------------------------------------------------------------------*
DATA: wa_equi      TYPE ty_equi,
      wa_equi_aux  TYPE ty_equi,
      wa_imrg_comb TYPE ty_imrg,
      wa_imrg_cont TYPE ty_imrg,
      wa_aufk      TYPE ty_aufk,
      wa_aufk_aux  TYPE ty_viaufkst,
      wa_mseg      TYPE ty_mseg,
      wa_0039      TYPE ty_0039,
      wa_cosp      TYPE ty_cosp,
      wa_cosp_lin  TYPE ty_cosp_lin,
      wa_cskb      TYPE ty_cskb,
      wa_t370k     TYPE ty_t370k,
      wa_saida     TYPE ty_saida,
      wa_detail    TYPE ty_detail,
      wa_viaufkst  TYPE ty_viaufkst,
      wa_afih      TYPE ty_afih,
      wa_covp      TYPE ty_covp.

DATA: lv_data  TYPE c LENGTH 10,
      lv_campo TYPE c LENGTH 50,
      lv_erro  TYPE char01,
      lv_num   TYPE n LENGTH 2.

* ALV
*----------------------------------------------------------------------*
DATA: obj_grid_geral   TYPE REF TO cl_gui_alv_grid,
      obj_grid_deta    TYPE REF TO cl_gui_alv_grid,
      obj_cont_geral   TYPE REF TO cl_gui_custom_container,
      obj_cont_deta    TYPE REF TO cl_gui_custom_container,

      it_fieldcatalog  TYPE lvc_t_fcat,
      wa_fieldcatalog  TYPE lvc_s_fcat,

      it_function      TYPE ui_functions,
      wa_function      LIKE LINE OF it_function,

      wa_layout        TYPE lvc_s_layo,
      wa_stable        TYPE lvc_s_stbl,

      it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row,

      it_sort          TYPE lvc_t_sort,
      wa_sort          TYPE lvc_s_sort,

      wa_color         TYPE          lvc_s_scol,  " Cor para célula
      it_color         TYPE TABLE OF lvc_s_scol.  " Cor para célula.

DATA: BEGIN OF t_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF t_msg.

DATA: lv_kokrs TYPE tka01-kokrs VALUE 'MAGI',
      r_kstar  TYPE RANGE OF s_kstar.

* Variáveis
*----------------------------------------------------------------------*
DATA: it_dta   TYPE TABLE OF bdcdata,
      wa_dta   TYPE bdcdata,
      vg_safra TYPE zpmt0072-safra,
      wg_opt   TYPE char10,

      tg_data  TYPE RANGE OF aufk-erdat,
      wg_data  LIKE LINE OF tg_data,

      tg_ano   TYPE RANGE OF cosp-gjahr,
      wg_ano   LIKE LINE OF tg_ano.

DATA: tp_rel     TYPE RANGE OF char10,
*---> 28/06/2023 - Migração S4 - JS
*      vg_mes_ini TYPE char02,
*      vg_mes_fim TYPE char02,
      vg_mes_ini TYPE zchar02,
      vg_mes_fim TYPE zchar02,
*<--- 28/06/2023 - Migração S4 - JS
      tw_rel     LIKE tp_rel.


*SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-002.
*SELECTION-SCREEN BEGIN OF LINE.
*
*SELECTION-SCREEN POSITION 1.
*PARAMETER: P_SINT RADIOBUTTON GROUP REL .
*SELECTION-SCREEN COMMENT 2(10) TEXT-003 FOR FIELD P_SINT.
*
*SELECTION-SCREEN POSITION 13.
*PARAMETER: P_ANAL RADIOBUTTON GROUP REL .
*SELECTION-SCREEN COMMENT 14(10) TEXT-004 FOR FIELD P_ANAL.
*
*SELECTION-SCREEN END OF LINE.
***SELECTION-SCREEN: END OF BLOCK B1.
**SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
**
**SELECTION-SCREEN BEGIN OF LINE.
**SELECTION-SCREEN COMMENT 4(22) text-003.
***SELECTION-SCREEN POSITION 2.
**PARAMETERS: r_r1 RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND abc.
**
**SELECTION-SCREEN COMMENT 32(10) text-004.
***SELECTION-SCREEN POSITION 20.
**PARAMETERS: r_r2 RADIOBUTTON GROUP 1.
**
**SELECTION-SCREEN COMMENT 50(20) text-013.
***SELECTION-SCREEN POSITION 79.
**PARAMETERS: r_r3 RADIOBUTTON GROUP 1.
**
**SELECTION-SCREEN COMMENT 79(30) text-014.
***SELECTION-SCREEN POSITION 79.
**PARAMETERS: r_r4 RADIOBUTTON GROUP 1.
**SELECTION-SCREEN END OF LINE.
**
**SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-009.
  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN POSITION 03.
    PARAMETER: rb_s RADIOBUTTON GROUP 1 DEFAULT 'X' USER-COMMAND abc.
    SELECTION-SCREEN COMMENT 05(10) TEXT-007 FOR FIELD rb_s.

    SELECTION-SCREEN POSITION 14.
    SELECTION-SCREEN COMMENT 20(7) TEXT-008 FOR FIELD rb_p.
    PARAMETER: rb_p RADIOBUTTON GROUP 1 .
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b5.

SELECTION-SCREEN: BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: p_swerk  FOR itob-swerk ,
                  p_equi   FOR equi-equnr,
                  p_eqart  FOR equi-eqart,
                  p_herst  FOR equi-herst,
                  p_typbz  FOR equi-typbz,
                  p_eqtyp  FOR equi-eqtyp ,
                  p_kstar  FOR cosp-kstar,
                  p_safra  FOR wa_saida-periodo MODIF ID i NO INTERVALS,
                  p_data   FOR wa_saida-periodo MODIF ID t NO-EXTENSION,
                  p_idade  FOR fleet-zzidade_eqpto NO-EXTENSION NO INTERVALS,
                  p_auart  FOR aufk-auart.

*                P_ANO   FOR COSP-GJAHR NO-EXTENSION NO INTERVALS OBLIGATORY,
*                P_PERIO FOR T247-MNR OBLIGATORY.

  PARAMETERS: p_inatel AS CHECKBOX,
              p_scusto AS CHECKBOX,
              p_segreg AS CHECKBOX.

SELECTION-SCREEN:END OF BLOCK a1.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE comm.
  SELECTION-SCREEN COMMENT /1(75) comm0.
  SELECTION-SCREEN COMMENT /1(75) comm1.
  SELECTION-SCREEN COMMENT /1(75) comm2.

  SELECTION-SCREEN COMMENT /1(75) comm3.
  SELECTION-SCREEN COMMENT /1(75) comm4.
  SELECTION-SCREEN COMMENT /1(75) comm5.

  SELECTION-SCREEN COMMENT /1(75) comm6.
  SELECTION-SCREEN COMMENT /1(75) comm7.
  SELECTION-SCREEN COMMENT /1(75) comm8.

  SELECTION-SCREEN COMMENT /1(75) comm9.
  SELECTION-SCREEN COMMENT /1(75) comm10.
  SELECTION-SCREEN COMMENT /1(75) comm11.

  SELECTION-SCREEN COMMENT /1(75) comm12.
  SELECTION-SCREEN COMMENT /1(75) comm13.
  SELECTION-SCREEN COMMENT /1(75) comm14.

  SELECTION-SCREEN COMMENT /1(75) comm15.
  SELECTION-SCREEN COMMENT /1(75) comm16.
  SELECTION-SCREEN COMMENT /1(75) comm17.
SELECTION-SCREEN END OF BLOCK b04.


*---------------------------------------------------------------------*
* SELECTION-SCREEN --------------------------------------------------*
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  IF rb_s IS NOT INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 = 'T'.
        screen-active = 0.
      ENDIF.

      IF screen-group1 = 'I'.
        screen-active = 1.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'T'.
        screen-active = 1.
      ENDIF.

      IF screen-group1 = 'I'.
        screen-active = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

* Layout dos campos para carga de infotipos
  PERFORM f_layout.

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventos DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

ENDCLASS.                    "LCL_EVENT DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_evento IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_eventos IMPLEMENTATION.
  METHOD on_hotspot_click.
    DATA: lv_data  TYPE c LENGTH 10,
          lv_campo TYPE c LENGTH 50,
          lv_num   TYPE n LENGTH 2.

    DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata,
          p_resp, check, p_erro(1),
          wa_bdcdata LIKE LINE OF ti_bdcdata.



    DATA: lv_kokrs TYPE tka01-kokrs VALUE 'MAGI',
          r_kstar  TYPE RANGE OF s_kstar.


    wg_opt = e_column_id-fieldname.

    CASE e_column_id-fieldname.
      WHEN: 'VL_COMB'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF sy-subrc IS INITIAL.
          REFRESH it_detail.
          LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_saida-equnr.
            IF p_segreg IS INITIAL.
              LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
                CASE wa_cosp_lin-kstar.
                  WHEN: '412012' OR '412013' OR '412014' OR '412015' OR '412017' OR '412018' OR '412104' OR '412542'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_cosp_lin-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ELSE.
              LOOP AT it_covp INTO wa_covp WHERE objnr = wa_aufk-objnr.
                CASE wa_covp-kstar.
                  WHEN: '412012' OR '412013' OR '412014' OR '412015' OR '412017' OR '412018' OR '412104' OR '412542'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_covp-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          CALL SCREEN 0200.
        ENDIF.
      WHEN: 'VL_FL_LB'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF sy-subrc IS INITIAL.
          REFRESH it_detail.
          LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_saida-equnr.
            IF p_segreg IS INITIAL.
              LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
                CASE wa_cosp_lin-kstar.
                  WHEN: '412016'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_cosp_lin-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ELSE.
              LOOP AT it_covp INTO wa_covp WHERE objnr = wa_aufk-objnr.
                CASE wa_covp-kstar.
                  WHEN: '412016'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_covp-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          CALL SCREEN 0200.
        ENDIF.
      WHEN: 'VL_PECA'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF sy-subrc IS INITIAL.
          REFRESH it_detail.
          LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_saida-equnr.
            IF p_segreg IS INITIAL.
              LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
                CASE wa_cosp_lin-kstar.
*                  WHEN: '412103'.
                  WHEN:
                '412035' " Materiais de obra civil
            OR '412039' " Materiais para pintura
            OR '412040' "
            OR '412045' "
            OR '412561' "
            OR '412043' " Peças e Acessórios - Veículos
            OR '412046' " Peças e Acessórios - Industriais
            OR '412000' " Outros - manutenção
            OR '412101' " Peças e acessórios - embarcações
            OR '412102' " Peças e acessórios - aeronaves
            OR '412103' " Peças e acessórios - máq. e equipamentos
            OR '412106' " Materiais para embarcações
            OR '412107' " Peças e acessórios - manut. mecânica
            OR '412108' " Peças e acessórios - manut. elétrica
            OR '412099'. " Outros - materiais.

                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_cosp_lin-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ELSE.
              LOOP AT it_covp INTO wa_covp WHERE objnr = wa_aufk-objnr.
                CASE wa_covp-kstar.
*                  WHEN: '412103'.
                  WHEN:
            '412035' " Materiais de obra civil
        OR '412039' " Materiais para pintura
        OR '412040' "
        OR '412045' "
        OR '412561' "
        OR '412043' " Peças e Acessórios - Veículos
        OR '412046' " Peças e Acessórios - Industriais
        OR '412000' " Outros - manutenção
        OR '412101' " Peças e acessórios - embarcações
        OR '412102' " Peças e acessórios - aeronaves
        OR '412103' "  Peças e acessórios - máq. e equipamentos
        OR '412106' " Materiais para embarcações
        OR '412107' " Peças e acessórios - manut. mecânica
        OR '412108' " Peças e acessórios - manut. elétrica
        OR '412099'. " Outros - materiais.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_covp-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          CALL SCREEN 0200.
        ENDIF.
      WHEN: 'VL_PNEU'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF sy-subrc IS INITIAL.
          REFRESH it_detail.
          LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_saida-equnr.
            IF p_segreg IS INITIAL.
              LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
                CASE wa_cosp_lin-kstar.
                  WHEN: '412044'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_cosp_lin-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ELSE.
              LOOP AT it_covp INTO wa_covp WHERE objnr = wa_aufk-objnr.
                CASE wa_covp-kstar.
                  WHEN: '412044'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_covp-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          CALL SCREEN 0200.
        ENDIF.
      WHEN: 'VL_MATE'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF sy-subrc IS INITIAL.
          REFRESH it_detail.
          LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_saida-equnr.
            IF p_segreg IS INITIAL.
              LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
                CASE wa_cosp_lin-kstar.
*                  WHEN: '412035' OR '412039' OR '412040' OR '412045'.
*                  WHEN:
*                 '412035' " Materiais de obra civil
*             OR '412039' " Materiais para pintura
*             OR '412040' "
*             OR '412045' "
*             OR '412561' "
*             OR '412043' " Peças e Acessórios - Veículos
*             OR '412046' " Peças e Acessórios - Industriais
*             OR '412000' " Outros - manutenção
*             OR '412101' " Peças e acessórios - embarcações
*             OR '412102' " Peças e acessórios - aeronaves
*             OR '412103' "  Peças e acessórios - máq. e equipamentos
*             OR '412106' " Materiais para embarcações
*             OR '412107' " Peças e acessórios - manut. mecânica
*             OR '412108' " Peças e acessórios - manut. elétrica
*             OR '412099'. " Outros - materiais.

*                    PERFORM F_DETAIL_VALOR USING WA_AUFK-AUFNR
*                                                 WG_OPT
*                                                 WA_COSP_LIN-VALOR
*                                                 WA_AUFK-AUART
*                                                 WA_AUFK-KTEXT
*                                                 WA_AUFK-ERDAT.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ELSE.
              LOOP AT it_covp INTO wa_covp WHERE objnr = wa_aufk-objnr.
                CASE wa_covp-kstar.
*                  WHEN: '412035' OR '412039' OR '412040' OR '412045'.
*                  WHEN:
*                 '412035' " Materiais de obra civil
*             OR '412039' " Materiais para pintura
*             OR '412040' "
*             OR '412045' "
*             OR '412561' "
*             OR '412043' " Peças e Acessórios - Veículos
*             OR '412046' " Peças e Acessórios - Industriais
*             OR '412000' " Outros - manutenção
*             OR '412101' " Peças e acessórios - embarcações
*             OR '412102' " Peças e acessórios - aeronaves
*             OR '412103' "  Peças e acessórios - máq. e equipamentos
*             OR '412106' " Materiais para embarcações
*             OR '412107' " Peças e acessórios - manut. mecânica
*             OR '412108' " Peças e acessórios - manut. elétrica
*             OR '412099'. " Outros - materiais.
*                    PERFORM F_DETAIL_VALOR USING WA_AUFK-AUFNR
*                                                 WG_OPT
*                                                 WA_COVP-VALOR
*                                                 WA_AUFK-AUART
*                                                 WA_AUFK-KTEXT
*                                                 WA_AUFK-ERDAT.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          CALL SCREEN 0200.
        ENDIF.
      WHEN: 'VL_TERC'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
        IF sy-subrc IS INITIAL.
          REFRESH it_detail.
          LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_saida-equnr.
            IF p_segreg IS INITIAL.
              LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
                CASE wa_cosp_lin-kstar.
                  WHEN: '422511' OR '412512' OR '412513' OR '412514' OR '412515' OR '412516' OR '412517' OR '412525' OR '412526' OR '412563'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_cosp_lin-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ELSE.
              LOOP AT it_covp INTO wa_covp WHERE objnr = wa_aufk-objnr.
                CASE wa_covp-kstar.
                  WHEN: '422511' OR '412512' OR '412513' OR '412514' OR '412515' OR '412516' OR '412517' OR '412525' OR '412526' OR '412563'.
                    PERFORM f_detail_valor USING wa_aufk-aufnr
                                                 wg_opt
                                                 wa_covp-valor
                                                 wa_aufk-auart
                                                 wa_aufk-ktext
                                                 wa_aufk-erdat.
                  WHEN OTHERS.
                    CONTINUE.
                ENDCASE.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
          CALL SCREEN 0200.
        ENDIF.
      WHEN 'ORDERID'.
        READ TABLE it_detail INTO wa_detail INDEX e_row_id-index.

        REFRESH it_dta.
        READ TABLE tg_data INTO wg_data INDEX 1.
        IF sy-subrc EQ 0.
          PERFORM call_fuc_kob1 USING wa_detail.
        ENDIF.

*        PERFORM F_SHDB USING:
*              ' '         ' '     'T'  'KOB1'               ' ',
*              'RKAEP000'  '0110'  'X'  ' '                  ' ',
*              ' '         ' '     ' '  'BDC_OKCODE'         '=%011',
*              ' '         ' '     ' '  'AUFNR-LOW'          WA_DETAIL-ORDERID,
*              ' '         ' '     ' '  'KSTAR-LOW'          'MAGI'.
*
*        CONCATENATE WG_DATA-LOW+6(2) '.' WG_DATA-LOW+4(2) '.' WG_DATA-LOW+0(4) INTO LV_DATA.
*        PERFORM F_SHDB USING:
**              'RKAEP000'  '0110'  'X'  ' '                  ' ',
**              ' '         ' '     ' '  'BDC_CURSOR'         'R_BUDAT-LOW',
*              ' '         ' '     ' '  'R_BUDAT-LOW'        LV_DATA.

*        CONCATENATE WG_DATA-HIGH+6(2) '.' WG_DATA-HIGH+4(2) '.' WG_DATA-HIGH+0(4) INTO LV_DATA.
*        PERFORM F_SHDB USING:
*              ' '         ' '     ' '  'R_BUDAT-HIGH'       LV_DATA,
*              'SAPLALDB'  '3000'  'X'  ' '                  ' ',
*              ' '         ' '     ' '  'BDC_OKCODE'         '/EDELA',
*              'SAPLALDB'   '3000'  'X'  ' '                  '=LINS',
*              ' '         ' '     ' '  'BDC_SUBSCR'         'SAPLALDB                                3010SCREEN_HEADER',
*              ' '         ' '     ' '  'BDC_CURSOR'         'RSCSEL_255-SLOW_I(01)'.

*        IF P_KSTAR IS INITIAL.
*          CASE WA_DETAIL-TIPO.
*            WHEN: 'VL_COMB'.
*              PERFORM F_SHDB USING:
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  '412012',
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(02)'  '412013',
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(03)'  '412014',
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(04)'  '412015',
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(05)'  '412017',
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(06)'  '412018',
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(07)'  '412104'.
*            WHEN: 'VL_FL_LB'.
*              PERFORM F_SHDB USING:
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  '412016'.
*            WHEN: 'VL_PNEU'.
*              PERFORM F_SHDB USING:
*                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  '412044'.
*            WHEN: 'VL_PECA'.
*              PERFORM CALL_FUC_KOB1 USING WA_DETAIL.
*
*            WHEN: 'VL_MATE'.
**              PERFORM F_SHDB USING:
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  '412035',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(02)'  '412039',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(03)'  '412040',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(04)'  '412045'.
*            WHEN: 'VL_TERC'.
*              PERFORM F_SHDB USING:
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(01)'  '422511',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(02)'  '412512',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(03)'  '412513',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(04)'  '412514',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(05)'  '412515',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(06)'  '412516',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(07)'  '412517',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(07)'  '412525',
**                    ' '         ' '     ' '  'RSCSEL_255-SLOW_I(07)'  '412526'.
*          ENDCASE.
*
*        ELSE.
*          CLEAR: LV_CAMPO, LV_NUM.
*          LOOP AT IT_CSKB_SEL INTO WA_CSKB.
*            ADD 1 TO LV_NUM.
*            CONCATENATE 'RSCSEL_255-SLOW_I(' LV_NUM ')' INTO LV_CAMPO.
*            PERFORM F_SHDB USING:
*                    ' '         ' '     ' '  LV_CAMPO  WA_CSKB-KSTAR.
*          ENDLOOP.
*        ENDIF.

*        PERFORM F_SHDB USING:
*        '        '  '    '  ' '  'BDC_OKCODE'         'P_KOKRS',
**        ' '         ' '     ' '  'BDC_OKCODE'         '=ACPT',
*        'RKAEP000'  '0110'  'X'  ' '                  ' ',
*        ' '         ' '     ' '  'BDC_OKCODE'         '=ONLI'.",
*        IF WA_DETAIL-TIPO NE 'VL_PECA'.
*          CALL TRANSACTION 'KOB1' USING IT_DTA MODE 'E'.
*        ENDIF.
    ENDCASE.



  ENDMETHOD.                    "ON_HOTSPOT_CLICK
ENDCLASS.                    "lcl_evento IMPLEMENTATION

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_typbz-low.
  PERFORM busca_modelo USING 'MODELO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_typbz-high.
  PERFORM busca_modelo USING 'MODELO'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_herst-low.
  PERFORM busca_modelo USING 'FABRICANTE'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_herst-high.
  PERFORM busca_modelo USING 'FABRICANTE'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kstar-low.
  PERFORM busca_classe_custo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kstar-high.
  PERFORM busca_classe_custo.

AT SELECTION-SCREEN.
  PERFORM: f_valida_periodo,
           f_valida_classe.

START-OF-SELECTION.

  PERFORM check_camp_obrig.

  IF lv_erro  IS INITIAL.
    IF p_safra IS INITIAL.
      PERFORM f_selecionar_dados.

      IF lv_erro  IS INITIAL.
        PERFORM f_organizar_dados.
        CALL SCREEN 0100.
      ENDIF.
    ELSE.

      LOOP AT p_safra.
*        clear: vg_safra.
*        vg_safra = p_safra-low.
*        rg_safra = VALUE #( ( sign = 'I' option = 'EQ' low = p_safra-low ) ).

        PERFORM f_selecionar_dados.

        IF lv_erro  IS INITIAL.
          PERFORM f_organizar_dados.
        ENDIF.
      ENDLOOP.

      IF it_saida IS NOT INITIAL.
        CALL SCREEN 0100.
      ENDIF.
    ENDIF.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM call_fuc_kob1 USING wa_detail TYPE ty_detail.
  FREE ti_bdcdata[].
  FREE it_msg[].

  DATA: l_data TYPE c LENGTH 10.

  SET PARAMETER ID 'CAC' FIELD 'MAGI'.

  PERFORM f_bdc_data USING:

  '        '      '0000'     'T'     'KOB1                 '        '                                                         ',
  'RKAEP000'      '0110'     'X'     '                     '        '                                                         ',
  '        '      '0000'     ' '     'BDC_CURSOR           '        'KSTAR-LOW                                                ',
  '        '      '0000'     ' '     'BDC_OKCODE           '        '=%011                                                    ',
  '        '      '0000'     ' '     'P_KOKRS              '        'MAGI                                                     ',
  '        '      '0000'     ' '     'AUFNR-LOW            '        wa_detail-orderid,
  '        '      '0000'     ' '     'KSTAR-LOW            '        '412103                                                   '.

  CONCATENATE wg_data-low+6(2) '.' wg_data-low+4(2) '.' wg_data-low+0(4) INTO lv_data.
  PERFORM f_bdc_data USING:
'        '      '0000'     ' '     'R_BUDAT-LOW          '        lv_data                                                     .

  CONCATENATE wg_data-high+6(2) '.' wg_data-high+4(2) '.' wg_data-high+0(4) INTO lv_data.
  l_data = lv_data.

  PERFORM f_bdc_data USING:
       '        '      '0000'     ' '     'R_BUDAT-HIGH         '        lv_data                                                    ,
     '        '      '0000'     ' '     'P_DISVAR             '        '                                                         ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    '.

  CASE wa_detail-tipo.
    WHEN: 'VL_COMB'.
      SET PARAMETER ID 'CAC' FIELD 'MAGI'.
      PERFORM f_bdc_data USING:
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',

     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=ACPT                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412012                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412013                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412014                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412015                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412017                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412018                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412104                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412012                                                   '.

    WHEN: 'VL_FL_LB'.
      SET PARAMETER ID 'CAC' FIELD 'MAGI'.
      PERFORM f_bdc_data USING:
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=ACPT                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412016                                                   '.

    WHEN: 'VL_PNEU'.
      SET PARAMETER ID 'CAC' FIELD 'MAGI'.
      PERFORM f_bdc_data USING:
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=ACPT                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412044                                                   '.

    WHEN: 'VL_TERC'.
      SET PARAMETER ID 'CAC' FIELD 'MAGI'.
      PERFORM f_bdc_data USING:

     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '422511                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412512                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412513                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412514                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412515                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412516                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412517                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412525                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                  ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=ACPT                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412526                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '422511                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412512                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412513                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412514                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412515                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412516                                                  ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412517                                                  '.

    WHEN 'VL_PECA'.
      SET PARAMETER ID 'CAC' FIELD 'MAGI'.
      PERFORM f_bdc_data USING:

     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412035                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412039                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412040                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412045                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412561                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412043                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412046                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412035                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412039                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412040                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412045                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412561                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412043                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412101                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412035                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412039                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412040                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412045                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412561                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412102                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412101                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412035                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412039                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412040                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412045                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412106                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412102                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412101                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412035                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412039                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412040                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412107                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412106                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412102                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412101                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412035                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412039                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=LINS                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412108                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412107                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412106                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412102                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412101                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412103                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412035                                                   ',
     'SAPLALDB'      '3000'     'X'     '                     '        '                                                         ',
     '        '      '0000'     ' '     'BDC_OKCODE           '        '=ACPT                                                    ',
     '        '      '0000'     ' '     'BDC_SUBSCR           '        'SAPLALDB                                3010SCREEN_HEADER',
     '        '      '0000'     ' '     'BDC_CURSOR           '        'RSCSEL_255-SLOW_I(01)                                    ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(01)'        '412099                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(02)'        '412108                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(03)'        '412107                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(04)'        '412106                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(05)'        '412102                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(06)'        '412101                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(07)'        '412000                                                   ',
     '        '      '0000'     ' '     'RSCSEL_255-SLOW_I(08)'        '412103                                                   '.
  ENDCASE.

  PERFORM f_bdc_data USING:
  'RKAEP000'      '0110'     'X'     '                     '        '                                                         ',
  '        '      '0000'     ' '     'BDC_CURSOR           '        'KSTAR-LOW                                                ',
  '        '      '0000'     ' '     'BDC_OKCODE           '        '=ONLI                                                    ',
  '        '      '0000'     ' '     'P_KOKRS              '        'MAGI                                                     ',
  '        '      '0000'     ' '     'AUFNR-LOW            '        wa_detail-orderid                                          ,
  '        '      '0000'     ' '     'KSTAR-LOW            '        '412099                                                   ',
* '        '      '0000'     ' '     'R_BUDAT-LOW          '        '01.10.2014                                               ',
* '        '      '0000'     ' '     'R_BUDAT-HIGH         '        '31.10.2014                                               ',
* '        '      '0000'     ' '     'P_DISVAR             '        '1SAP                                                     ',
  'RKAEP000'      '0110'     'X'     '                     '        '                                                         ',
  '        '      '0000'     ' '     'BDC_OKCODE           '        '/EENDE                                                   ',
  '        '      '0000'     ' '     'BDC_CURSOR           '        'AUFNR-LOW                                                '.

  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'KOB1' CHANGING p_erro.

  IF p_erro IS NOT INITIAL.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
FORM f_selecionar_dados .
  DATA: lv_valor         TYPE imrc_totac,
        lv_cont          TYPE i,
        lv_aux           TYPE c LENGTH 3,
        lv_campo         TYPE c LENGTH 6,
        lv_zzidade_eqpto TYPE fleet-zzidade_eqpto,
        lv_txt           TYPE bsvx-sttxt,

**      Variáveis para consulta de periodo contábil
        lv_exe_ini       TYPE cosp-gjahr,
        lv_per_ini       TYPE i,
        lv_exe_fim       TYPE cosp-gjahr,
        lv_per_fim       TYPE i,

        wl_return        TYPE bapiret2,
        tl_status_equip  TYPE TABLE OF bapi_itob_status WITH HEADER LINE,
        tl_status_user   TYPE TABLE OF bapi_itob_status WITH HEADER LINE,
        vl_equnr         TYPE v_equi-equnr,
        vl_iwerk         TYPE v_equi-iwerk,
        vl_timbi         TYPE v_equi-timbi,
        vl_lines         TYPE i.

* ---> S4 Migration - 17/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_langu           TYPE bapi0015_10,
        lv_controllingarea TYPE bapi1030_gen-co_area,
        lv_costelement     TYPE bapi1030_gen-cost_elem,
        lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 17/07/2023 - CA

  FIELD-SYMBOLS: <fs_equi> TYPE ty_equi,
                 <wa_covp> TYPE ty_covp.

  FREE: it_cskb_sel, it_cosp_lin.

  CLEAR: vg_mes_ini, vg_mes_fim, lv_erro.

  r_kstar = VALUE #(
  (  option = 'EQ' sign = 'I' low = '0000412012' )
  (  option = 'EQ' sign = 'I' low = '0000412013' )
  (  option = 'EQ' sign = 'I' low = '0000412014' )
  (  option = 'EQ' sign = 'I' low = '0000412015' )
  (  option = 'EQ' sign = 'I' low = '0000412017' )
  (  option = 'EQ' sign = 'I' low = '0000412542' )
  (  option = 'EQ' sign = 'I' low = '0000412018' )
  (  option = 'EQ' sign = 'I' low = '0000412104' )
  (  option = 'EQ' sign = 'I' low = '0000412016' )
  (  option = 'EQ' sign = 'I' low = '0000412044' )
  (  option = 'EQ' sign = 'I' low = '0000412103' )
  (  option = 'EQ' sign = 'I' low = '0000412035' )
  (  option = 'EQ' sign = 'I' low = '0000412039' )
  (  option = 'EQ' sign = 'I' low = '0000412040' )
  (  option = 'EQ' sign = 'I' low = '0000412045' )
  (  option = 'EQ' sign = 'I' low = '0000422511' )
  (  option = 'EQ' sign = 'I' low = '0000412512' )
  (  option = 'EQ' sign = 'I' low = '0000412513' )
  (  option = 'EQ' sign = 'I' low = '0000412514' )
  (  option = 'EQ' sign = 'I' low = '0000412515' )
  (  option = 'EQ' sign = 'I' low = '0000412516' )
  (  option = 'EQ' sign = 'I' low = '0000412517' )
  (  option = 'EQ' sign = 'I' low = '0000412525' )
  (  option = 'EQ' sign = 'I' low = '0000412526' )
  (  option = 'EQ' sign = 'I' low = '0000412043' )
  (  option = 'EQ' sign = 'I' low = '0000412046' )
  (  option = 'EQ' sign = 'I' low = '0000412100' )
  (  option = 'EQ' sign = 'I' low = '0000412101' )
  (  option = 'EQ' sign = 'I' low = '0000412102' )
  (  option = 'EQ' sign = 'I' low = '0000412103' )
  (  option = 'EQ' sign = 'I' low = '0000412106' )
  (  option = 'EQ' sign = 'I' low = '0000412107' )
  (  option = 'EQ' sign = 'I' low = '0000412108' )
  (  option = 'EQ' sign = 'I' low = '0000412199' )
  (  option = 'EQ' sign = 'I' low = '0000412561' )
  (  option = 'EQ' sign = 'I' low = '0000412563' ) ).

  IF p_segreg IS INITIAL.
    SELECT equnr typbz eqart objnr swerk baujj iwerk datbi datab timbi herst
      FROM v_equi
      INTO CORRESPONDING FIELDS OF TABLE it_equi
      WHERE equnr IN p_equi
       AND  typbz IN p_typbz
       AND  eqart IN p_eqart
       AND  herst IN p_herst
       AND  swerk IN p_swerk
       AND  eqtyp IN p_eqtyp
       AND  datbi EQ '99991231'.

  ELSE.
    SELECT equnr typbz eqart objnr swerk baujj iwerk datbi datab timbi herst
      FROM v_equi
      INTO CORRESPONDING FIELDS OF TABLE it_equi
      WHERE equnr IN p_equi
      AND   typbz IN p_typbz
      AND   eqart IN p_eqart
      AND   herst IN p_herst
*     AND   IWERK IN P_SWERK
      AND   eqtyp IN p_eqtyp.

    it_equi_aux = it_equi.
    CLEAR: it_equi.
    SORT it_equi_aux BY equnr datab datbi timbi.

    LOOP AT it_equi_aux INTO wa_equi_aux.
      IF sy-tabix EQ 1.
        vl_equnr = wa_equi_aux-equnr.
        vl_iwerk = wa_equi_aux-iwerk.
        APPEND wa_equi_aux TO it_equi.
      ELSE.
        IF vl_equnr EQ wa_equi_aux-equnr AND vl_iwerk EQ wa_equi_aux-iwerk.
          DESCRIBE TABLE it_equi LINES vl_lines.
          MODIFY it_equi FROM wa_equi_aux INDEX vl_lines TRANSPORTING datbi timbi.
        ELSE.
          vl_equnr = wa_equi_aux-equnr.
          vl_iwerk = wa_equi_aux-iwerk.
          APPEND wa_equi_aux TO it_equi.
        ENDIF.
      ENDIF.
    ENDLOOP.
    CLEAR: vl_equnr.

    LOOP AT it_equi INTO wa_equi_aux.
      IF vl_equnr NE wa_equi_aux-equnr.
        wa_equi_aux-timei = '000000'.
        vl_equnr = wa_equi_aux-equnr.
        vl_timbi = wa_equi_aux-timbi.
      ELSE.
        wa_equi_aux-timei = vl_timbi.
        vl_equnr = wa_equi_aux-equnr.
        vl_timbi = wa_equi_aux-timbi.
      ENDIF.

      MODIFY it_equi FROM wa_equi_aux INDEX sy-tabix.
    ENDLOOP.

    DELETE it_equi WHERE iwerk NOT IN p_swerk.

  ENDIF.

  CHECK it_equi IS NOT INITIAL.

  SORT it_equi BY equnr.

  IF p_inatel IS INITIAL.
*  * Checando status do equipamento
    LOOP AT it_equi ASSIGNING <fs_equi>.
      CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
        EXPORTING
          equipment     = <fs_equi>-equnr
        IMPORTING
          return        = wl_return
        TABLES
          system_status = tl_status_equip
          user_status   = tl_status_user.

      READ TABLE tl_status_equip WITH KEY status = 'I0076'.
      IF sy-subrc IS INITIAL.
        DELETE it_equi WHERE equnr = <fs_equi>-equnr.
      ELSE.
        READ TABLE tl_status_equip WITH KEY status = 'I0320'.
        IF sy-subrc IS INITIAL.
          DELETE it_equi WHERE equnr = <fs_equi>-equnr.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

**  Tipo de Equipamento
  SELECT DISTINCT eqart eartx
    INTO TABLE it_t370k
    FROM t370k_t
    FOR ALL ENTRIES IN it_equi
    WHERE eqart = it_equi-eqart
     AND  spras = sy-langu.

*  Determinando a range de data
  REFRESH: tg_data, tg_ano.
  CLEAR:   wg_data, wg_ano.

  wg_ano-sign   = 'I'.
  wg_ano-option = 'BT'.

  wg_data-sign   = 'I'.
  wg_data-option = 'BT'.

  "Formatar data.
  IF p_data IS NOT INITIAL.
    CONCATENATE p_data-low(4)  p_data-low+5(2)  '01' INTO wg_data-low.
    wg_ano-low = p_data-low(4).
    lv_per_ini = p_data-low+5(2).
    lv_exe_ini = p_data-low(4).

    IF p_data-high IS INITIAL.
      CONCATENATE p_data-low(4)  p_data-low+5(2)  '01' INTO wg_data-high.
      wg_ano-high = p_data-low(4).
      lv_per_fim  = p_data-low+5(2).
      lv_exe_fim  = p_data-low(4).
    ELSE.
      CONCATENATE p_data-high(4) p_data-high+5(2) '01' INTO wg_data-high.
      wg_ano-high = p_data-high(4).
      lv_per_fim  = p_data-high+5(2).
      lv_exe_fim  = p_data-high(4).
    ENDIF.
  ELSE.
    CLEAR: ws_zpmt0072.
    IF p_safra IS NOT INITIAL.
      SELECT SINGLE * FROM zpmt0072 INTO ws_zpmt0072 WHERE werks IN p_swerk AND safra EQ p_safra-low.
      IF sy-subrc EQ 0.

        CONCATENATE ws_zpmt0072-ano_ini  ws_zpmt0072-mes_ini  '01' INTO wg_data-low.
        wg_ano-low = ws_zpmt0072-ano_ini.
        lv_per_ini = ws_zpmt0072-mes_ini.
        lv_exe_ini = ws_zpmt0072-ano_ini.

        CONCATENATE ws_zpmt0072-ano_fim  ws_zpmt0072-mes_fim  '01' INTO wg_data-high.
        wg_ano-high = ws_zpmt0072-ano_fim.
        lv_per_fim  = ws_zpmt0072-mes_fim.
        lv_exe_fim  = ws_zpmt0072-ano_fim.
      ELSE.
        MESSAGE |Safra { p_safra-low } não cadastrada na transação ZPM0091| TYPE 'I'.
        lv_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.


  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = wg_data-high
    IMPORTING
      last_day_of_month = wg_data-high.

  APPEND wg_data TO tg_data.
  APPEND wg_ano TO tg_ano.

  CHECK tg_data IS NOT INITIAL.
  READ TABLE tg_data INTO DATA(ws_data) INDEX 1.

  LOOP AT it_equi ASSIGNING FIELD-SYMBOL(<ls_equi>).
    "Idade do equipamento.
    CLEAR: lv_zzidade_eqpto.
    IF <ls_equi>-baujj IS NOT INITIAL AND ws_data-low IS NOT INITIAL.
      lv_zzidade_eqpto = ws_data-low(4) - <ls_equi>-baujj.
      CONDENSE lv_zzidade_eqpto NO-GAPS.
      IF lv_zzidade_eqpto IS NOT INITIAL.
        <ls_equi>-zzidade_eqpto =  lv_zzidade_eqpto.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: ws_data.

  IF p_idade IS NOT INITIAL.
    SORT it_equi BY zzidade_eqpto.
    DELETE it_equi WHERE zzidade_eqpto > p_idade-low.
  ENDIF.

  CHECK it_equi IS NOT INITIAL.

  SORT it_equi BY equnr.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_viaufkst
    FROM viaufkst
    FOR ALL ENTRIES IN it_equi
    WHERE equnr EQ  it_equi-equnr
    AND   auart IN p_auart.
*     AND  ERDAT IN TG_DATA.


  LOOP AT it_viaufkst INTO wa_viaufkst.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        objnr            = wa_viaufkst-objnr
        spras            = sy-langu
        flg_user_stat    = 'X'
      IMPORTING
        line             = lv_txt
      EXCEPTIONS
        object_not_found = 01.

    IF NOT ( lv_txt+0(3) = 'LIB' OR lv_txt+0(3) = 'ENT' ).
      DELETE TABLE it_viaufkst FROM wa_viaufkst.
    ENDIF.
  ENDLOOP.

  CHECK it_viaufkst IS NOT INITIAL.

** Seleciona pontos de odometro/horimentro
  FREE it_imrg_cont.
  SELECT DISTINCT mpobj ir~point mdocm cdiff recdu idate itime
    FROM imrg AS ir
    INNER JOIN imptt AS ip ON ir~point = ip~point
    INTO CORRESPONDING FIELDS OF TABLE it_imrg_cont
    FOR ALL ENTRIES IN it_equi
    WHERE ip~mpobj = it_equi-objnr
     AND  ir~idate IN tg_data
     AND  ir~cancl EQ ''
     AND  ip~mptyp EQ 'V'
     AND  ip~indtr EQ ''
     AND  ip~inact EQ ''.

** Seleciona pontos de abastecimento
  FREE it_imrg_comb.
  SELECT DISTINCT mpobj ir~point mdocm cdiff recdu idate itime
    FROM imrg AS ir
    INNER JOIN imptt AS ip ON ir~point = ip~point
    INTO CORRESPONDING FIELDS OF TABLE it_imrg_comb
    FOR ALL ENTRIES IN it_equi
    WHERE ip~mpobj EQ it_equi-objnr
     AND  ir~idate IN tg_data
     AND  ir~cancl EQ ''
     AND  ip~mptyp EQ 'M'
     AND  ip~indtr EQ ''
     AND  ip~inact EQ ''.

* ---> S4 Migration - 17/07/2023 - CA
*** Seleciona apenas as contas necessárias
*  FREE it_cskb.
*  SELECT DISTINCT c~kstar m~mctxt
*  FROM cskb AS c
*  INNER JOIN m_kartc AS m ON c~kstar = m~kstar
*  INTO TABLE it_cskb
*  WHERE m~spras EQ sy-langu
*   AND  c~kstar IN r_kstar.

* não há filtro por controlling area - buscar todos
  SELECT kokrs
    FROM tka01
    INTO TABLE @DATA(lt_tka01)
    WHERE kokrs <> @space.

  IF sy-subrc = 0.

    LOOP AT lt_tka01 INTO DATA(ls_tka01).

      lv_controllingarea = ls_tka01-kokrs.
      lv_keydate         = sy-datum.
      ls_langu-langu     = sy-langu(1).
      ls_langu-langu_iso = sy-langu.

      LOOP AT r_kstar INTO DATA(ls_kstar).

        lv_costelement = ls_kstar-low.

        CLEAR: lt_returns[], ls_coeldes.

        CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
          EXPORTING
            controllingarea   = lv_controllingarea
            costelement       = lv_costelement
            keydate           = lv_keydate
            language          = ls_langu
          IMPORTING
            costelementdetail = ls_coeldes
          TABLES
            return            = lt_returns.

        READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc <> 0.
          wa_cskb-kstar = lv_costelement.
          wa_cskb-mctxt = ls_coeldes-name.

          APPEND wa_cskb TO it_cskb.
          CLEAR wa_cskb.
        ENDIF.

        CLEAR: ls_kstar, lv_costelement.
      ENDLOOP.

      CLEAR: ls_tka01.
    ENDLOOP.
  ENDIF.
* <--- S4 Migration - 17/07/2023 - CA

  SORT it_cskb ASCENDING BY kstar.


  IF p_segreg IS INITIAL.

*     SELECT *
*        FROM COSP
*        INTO TABLE IT_COSP
*        FOR ALL ENTRIES IN IT_AUFK
*        WHERE OBJNR = IT_AUFK-OBJNR
*         AND  GJAHR IN TG_ANO
*         AND  KSTAR IN P_KSTAR.

    FREE it_aufk_aux.
    SELECT equnr aufnr objnr erdat ktext auart
    FROM viaufkst
    INTO TABLE it_aufk_aux
    FOR ALL ENTRIES IN it_viaufkst
    WHERE aufnr EQ it_viaufkst-aufnr
*---> IR117715 CS1039460
      AND equnr IN p_equi.
*<--- IR117715 CS1039460

    FREE it_aufk.
    SELECT o~equnr a~aufnr c~objnr a~erdat a~ktext a~auart
    FROM equi AS o
    INNER JOIN viaufkst AS a ON a~equnr EQ o~equnr
    INNER JOIN afko AS b ON b~aufnr EQ a~aufnr
    INNER JOIN afvc AS c ON c~aufpl EQ b~aufpl
    INNER JOIN cosp AS d ON d~objnr EQ c~objnr
    APPENDING TABLE it_aufk
          FOR ALL ENTRIES IN it_aufk_aux
    WHERE b~aufnr EQ it_aufk_aux-aufnr
     AND  d~gjahr IN tg_ano
     AND  ( d~kstar IN p_kstar OR d~kstar IN r_kstar ).

    DATA: r_objnr TYPE RANGE OF j_objnr.

    r_objnr = VALUE #( FOR ls IN it_aufk_aux
                          FOR ls1 IN it_aufk WHERE ( objnr EQ ls-objnr )
                          (
                            sign = 'I'
                            option = 'EQ'
                            low = ls1-objnr
                          )
                     ).

    SORT r_objnr.
    SORT it_aufk_aux.
    IF r_objnr IS NOT INITIAL.
      DELETE it_aufk_aux[] WHERE objnr IN r_objnr.
    ENDIF.
    APPEND LINES OF it_aufk_aux[] TO it_aufk.


*    LOOP AT IT_AUFK_AUX ASSIGNING FIELD-SYMBOL(<AUFK>).
*      IF LINE_EXISTS( IT_AUFK[ OBJNR = <AUFK>-OBJNR ] ).
*        CLEAR <AUFK>.
*      ELSE.
*        APPEND <AUFK> TO
*        IT_AUFK.
*      ENDIF.
*    ENDLOOP.
*
*    SORT IT_AUFK BY AUFNR.

    IF it_aufk IS NOT INITIAL.

      FREE it_cosp.
      SELECT *
       FROM cosp
       INTO TABLE it_cosp
       FOR ALL ENTRIES IN it_aufk
       WHERE objnr EQ it_aufk-objnr
       AND   gjahr IN tg_ano
       AND   kstar IN p_kstar
       AND   vrgng NOT IN ( 'RMBA', 'RMBE' ).
    ENDIF.

    FIELD-SYMBOLS: <ws_cosp> TYPE ty_cosp.

    LOOP AT it_cosp ASSIGNING <ws_cosp>.
**  Remover registro com contas
      CLEAR wa_cskb.
      READ TABLE it_cskb INTO wa_cskb WITH KEY kstar = <ws_cosp>-kstar.
      IF sy-subrc IS INITIAL.
        APPEND wa_cskb TO it_cskb_sel.
      ELSE.
        DELETE it_cosp WHERE kstar = <ws_cosp>-kstar.
        CONTINUE.
      ENDIF.

*** Remover zeros a esquerda
      SHIFT <ws_cosp>-kstar LEFT DELETING LEADING '0'.
    ENDLOOP.

    UNASSIGN <ws_cosp>.

    SORT: it_cosp     BY objnr kstar gjahr,
          it_cskb_sel BY kstar.

    DELETE ADJACENT DUPLICATES FROM it_cskb_sel COMPARING kstar.

    LOOP AT it_cosp ASSIGNING <fs_cosp>.
      lv_cont = 0.

      WHILE lv_cont <= 12.
        ADD 1 TO lv_cont.
        CLEAR lv_valor.

        lv_aux = lv_cont.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = lv_aux
          IMPORTING
            output = lv_aux.

        CONCATENATE 'WTG' lv_aux INTO lv_campo.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <fs_cosp> TO <fs_field>.
        lv_valor = <fs_field>.

        IF <fs_field> IS NOT INITIAL.
          IF  ( <fs_cosp>-gjahr = lv_exe_ini )
          AND ( lv_cont < lv_per_ini ).
            CONTINUE.
          ENDIF.

          IF  ( <fs_cosp>-gjahr = lv_exe_fim )
          AND ( lv_cont > lv_per_fim ).
            CONTINUE.
          ENDIF.

          wa_cosp_lin-objnr     = <fs_cosp>-objnr.
          wa_cosp_lin-kstar     = <fs_cosp>-kstar.
          wa_cosp_lin-gjahr     = <fs_cosp>-gjahr.
          wa_cosp_lin-cont      = lv_cont.
          wa_cosp_lin-valor     = lv_valor.

          APPEND wa_cosp_lin TO it_cosp_lin.
*        ENDIF.
        ENDIF.
      ENDWHILE.
    ENDLOOP.

  ELSE.

*    SELECT *
*        FROM VIAUFKST
*        INTO CORRESPONDING FIELDS OF TABLE IT_AUFK_AUX
*        FOR ALL ENTRIES IN IT_VIAUFKST
*        WHERE AUFNR EQ IT_VIAUFKST-AUFNR.

    FREE it_viaufkst.
    SELECT equnr aufnr objnr erdat ktext auart
    FROM viaufkst
    INTO TABLE it_aufk_aux
    FOR ALL ENTRIES IN it_viaufkst
    WHERE aufnr EQ it_viaufkst-aufnr
*---> IR117715 CS1039460
      AND equnr IN  p_equi.
*<---IR117715 CS1039460
    FREE it_aufk.
    SELECT o~equnr a~aufnr c~objnr a~erdat a~ktext a~auart
     FROM equi AS o
     INNER JOIN viaufkst AS a ON a~equnr EQ o~equnr
     INNER JOIN afko AS b ON b~aufnr EQ a~aufnr
     INNER JOIN afvc AS c ON c~aufpl EQ b~aufpl
     INNER JOIN covp AS d ON d~objnr EQ c~objnr
     APPENDING TABLE it_aufk
           FOR ALL ENTRIES IN it_aufk_aux
     WHERE b~aufnr EQ it_aufk_aux-aufnr
      AND  d~gjahr IN tg_ano
*---> IR117715 CS1039460
      AND o~equnr IN  p_equi
*<---IR117715 CS1039460
      AND  ( d~kstar IN p_kstar OR d~kstar IN r_kstar ).
*      AND  D~KSTAR IN P_KSTAR.

*---> IR117715 CS1039460
*    r_objnr = VALUE #( FOR ls IN it_aufk_aux
*                        FOR ls1 IN it_aufk WHERE ( objnr EQ ls-objnr )
*                        (
*                          sign = 'I'
*                          option = 'EQ'
*                          low = ls1-objnr
*                        )
*                   ).


    CLEAR r_objnr1. REFRESH r_objnr1.

    LOOP AT it_aufk_aux INTO wa_aufk_aux.

      READ TABLE it_aufk INTO wa_aufk
      WITH KEY objnr = wa_aufk-objnr.

      IF sy-subrc EQ 0.
        r_objnr1-sign = 'I'.
        r_objnr1-option = 'EQ'.
        r_objnr1-low = wa_aufk-objnr.
        APPEND r_objnr1 TO r_objnr1.
      ENDIF.
    ENDLOOP.

    r_objnr[] = r_objnr1[].
*<---IR117715 CS1039460

    SORT r_objnr.
    SORT it_aufk_aux.
    IF r_objnr IS NOT INITIAL.
      DELETE it_aufk_aux[] WHERE objnr IN r_objnr.
    ENDIF.
    APPEND LINES OF it_aufk_aux[] TO it_aufk.

*    LOOP AT IT_AUFK_AUX ASSIGNING FIELD-SYMBOL(<AUFK_>).
*      IF LINE_EXISTS( IT_AUFK[ OBJNR = <AUFK_>-OBJNR ] ).
*        CLEAR <AUFK_>.
*      ELSE.
*        APPEND <AUFK_> TO IT_AUFK.
*      ENDIF.
*    ENDLOOP.
*
*    SORT IT_AUFK BY AUFNR.

    IF it_aufk IS NOT INITIAL.
      SELECT *
      FROM covp
      INTO TABLE it_covp
      FOR ALL ENTRIES IN it_aufk
      WHERE objnr EQ it_aufk-objnr
      AND   gjahr IN tg_ano
      AND   kstar IN p_kstar.
    ENDIF.

*    SELECT *
*      FROM COVP
*      INTO TABLE IT_COVP
*      FOR ALL ENTRIES IN IT_AUFK
*      WHERE OBJNR = IT_AUFK-OBJNR
*      AND  GJAHR IN TG_ANO
*      AND  KSTAR IN P_KSTAR.

    IF it_covp IS NOT INITIAL.
      IF lv_per_ini NE 1.
        DELETE it_covp
          WHERE gjahr EQ wg_ano-low
          AND perio LT lv_per_ini.
      ENDIF.
      IF lv_per_fim NE 12.
        DELETE it_covp
          WHERE gjahr EQ wg_ano-high
          AND perio GT lv_per_fim.
      ENDIF.
    ENDIF.

*** Remover zeros a esquerda
    LOOP AT it_covp ASSIGNING <wa_covp>.
      SHIFT <wa_covp>-kstar LEFT DELETING LEADING '0'.
      <wa_covp>-valor = <wa_covp>-wtgbtr.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_organizar_dados .
  FIELD-SYMBOLS: <ws_field> TYPE any,
                 <ls_field> TYPE any.

  DATA: lv_valor  TYPE imrc_totac.

  CLEAR: lv_valor, wa_saida.

  SORT: it_equi BY swerk equnr,
        it_cosp BY objnr gjahr.

  DELETE ADJACENT DUPLICATES FROM it_equi COMPARING ALL FIELDS.

  LOOP AT it_equi INTO wa_equi.
    wa_saida-equnr = wa_equi-equnr.
    wa_saida-tipbz = wa_equi-typbz.
*   US #175265 - MMSILVA - 29.04.2025 - Inicio
    wa_saida-herst = wa_equi-herst.
*   US #175265 - MMSILVA - 29.04.2025 - Fim
    wa_saida-zzidade_eqpto = wa_equi-zzidade_eqpto.

    IF p_segreg IS INITIAL.
      wa_saida-swerk = wa_equi-swerk.
      it_imrg_comb_aux = it_imrg_comb.
      it_imrg_cont_aux = it_imrg_cont.
    ELSE.
      wa_saida-swerk = wa_equi-iwerk.
      wa_saida-datab = wa_equi-datab.
      wa_saida-datbi = wa_equi-datbi.
      wa_saida-timbi = wa_equi-timbi.
      it_imrg_comb_aux = it_imrg_comb.
      it_imrg_cont_aux = it_imrg_cont.
      it_covp_aux = it_covp.

      DELETE it_imrg_comb_aux
        WHERE ( idate GT wa_equi-datbi OR idate LT wa_equi-datab )
        OR ( idate EQ wa_equi-datab AND itime LE wa_equi-timei )
        OR ( idate EQ wa_equi-datbi AND itime GT wa_equi-timbi ).

      DELETE it_imrg_cont_aux
        WHERE ( idate GT wa_equi-datbi OR idate LT wa_equi-datab )
        OR ( idate EQ wa_equi-datab AND itime LE wa_equi-timei )
        OR ( idate EQ wa_equi-datbi AND itime GT wa_equi-timbi ).

      DELETE it_covp_aux
        WHERE ( budat GT wa_equi-datbi OR budat LT wa_equi-datab )
        OR ( budat EQ wa_equi-datab AND cputm LE wa_equi-timei )
        OR ( budat EQ wa_equi-datbi AND cputm GT wa_equi-timbi ).

    ENDIF.

**  Horas trabalhadas
    LOOP AT it_imrg_cont_aux INTO wa_imrg_cont WHERE mpobj = wa_equi-objnr.
      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit       = wa_imrg_cont-recdu
          decimals        = 0
          exponent        = 0
          fltp_value_si   = wa_imrg_cont-cdiff                                " Campo Consumo
          indicator_value = 'X'
          masc_symbol     = ' '
        IMPORTING
          char_value      = lv_valor.

      ADD lv_valor TO wa_saida-vl_cont.
    ENDLOOP.


**  Abastecimentos do periodo
    LOOP AT it_imrg_comb_aux INTO wa_imrg_comb WHERE mpobj = wa_equi-objnr.

      CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
        EXPORTING
          char_unit       = wa_imrg_comb-recdu
          decimals        = 0
          exponent        = 0
          fltp_value_si   = wa_imrg_comb-cdiff                                " Campo Consumo
          indicator_value = 'X'
          masc_symbol     = ' '
        IMPORTING
          char_value      = lv_valor.

      ADD lv_valor TO wa_saida-vl_abast.
    ENDLOOP.

    TRY.
        IF wa_imrg_cont-recdu = 'KM'.
          wa_saida-cons_med = ( wa_saida-vl_cont / wa_saida-vl_abast ).                 " Campo Média Consumo
        ELSEIF wa_imrg_cont-recdu = 'H'.
          wa_saida-cons_med = ( wa_saida-vl_abast / wa_saida-vl_cont ).                 " Campo Média Consumo
        ENDIF.
      CATCH cx_root.

    ENDTRY.

    LOOP AT it_aufk INTO wa_aufk WHERE equnr = wa_equi-equnr.
      IF p_segreg IS INITIAL.
        LOOP AT it_cosp_lin INTO wa_cosp_lin WHERE objnr = wa_aufk-objnr.
*          READ TABLE IT_CSKB_SEL INTO WA_CSKB WITH KEY KSTAR = WA_COSP_LIN-KSTAR.
*          IF WA_COSP_LIN-KSTAR = |{ WA_CSKB-KSTAR ALPHA = OUT }|.
          CASE wa_cosp_lin-kstar.
*         Total consumo de combustível
            WHEN: '412012' OR '412013' OR '412014' OR '412015' OR '412017' OR '412018' OR '412104' OR '412542'.
              ADD wa_cosp_lin-valor TO wa_saida-vl_comb.
*         Total filtro e lubrificantes
            WHEN: '412016'.
              ADD wa_cosp_lin-valor TO wa_saida-vl_fl_lb.

*         Total pneu e camara
            WHEN: '412044'.
              ADD wa_cosp_lin-valor TO wa_saida-vl_pneu.

*         Total Peças e materiais de manutenção
            WHEN:
                 '412035' " Materiais de obra civil
              OR '412039' " Materiais para pintura
              OR '412040' "
              OR '412045' "
              OR '412561' "
              OR '412043' " Peças e Acessórios - Veículos
              OR '412046' " Peças e Acessórios - Industriais
              OR '412000' " Outros - manutenção
              OR '412101' " Peças e acessórios - embarcações
              OR '412102' " Peças e acessórios - aeronaves
              OR '412103' "  Peças e acessórios - máq. e equipamentos
              OR '412106' " Materiais para embarcações
              OR '412107' " Peças e acessórios - manut. mecânica
              OR '412108' " Peças e acessórios - manut. elétrica
              OR '412099'. " Outros - materiais.

              ADD wa_cosp_lin-valor TO wa_saida-vl_peca.

****Comentado porque as contas de materiais deiversos foram incorporadas e peças e materiais de manuteção.
*         Total materiais diversos
*            WHEN: '412035' OR '412039' OR '412040' OR '412045'.
*              ADD WA_COSP_LIN-VALOR TO WA_SAIDA-VL_MATE.

*         Total terceiros
            WHEN: '422511' OR '412512' OR '412513' OR '412514' OR '412515' OR '412516' OR '412517' OR '412525'OR '412526' OR '412563'.
              ADD wa_cosp_lin-valor TO wa_saida-vl_terc.
          ENDCASE.
        ENDLOOP.
      ELSE.
        LOOP AT it_covp_aux INTO wa_covp WHERE objnr = wa_aufk-objnr.
*          READ TABLE IT_CSKB_SEL INTO WA_CSKB WITH KEY KSTAR = WA_COSP_LIN-KSTAR.
*          IF WA_COSP_LIN-KSTAR = |{ WA_CSKB-KSTAR ALPHA = OUT }|.
          CASE wa_covp-kstar.
*         Total consumo de combustível
            WHEN: '412012' OR '412013' OR '412014' OR '412015' OR '412017' OR '412018' OR '412104' OR '412542'.
              ADD wa_covp-wtgbtr TO wa_saida-vl_comb.
*         Total Peças e materiais de manutenção
            WHEN:
                  '412035' " Materiais de obra civil
              OR '412039' " Materiais para pintura
              OR '412040' "
              OR '412045' "
              OR '412561' "
              OR '412043' " Peças e Acessórios - Veículos
              OR '412046' " Peças e Acessórios - Industriais
              OR '412000' " Outros - manutenção
              OR '412101' " Peças e acessórios - embarcações
              OR '412102' " Peças e acessórios - aeronaves
              OR '412103' "  Peças e acessórios - máq. e equipamentos
              OR '412106' " Materiais para embarcações
              OR '412107' " Peças e acessórios - manut. mecânica
              OR '412108' " Peças e acessórios - manut. elétrica
              OR '412099'. " Outros - materiais.
              ADD wa_covp-wtgbtr TO wa_saida-vl_fl_lb.

*         Total pneu e camara
            WHEN: '412044'.
              ADD wa_covp-wtgbtr TO wa_saida-vl_pneu.

*         Total peças e acessórios
            WHEN: '412103'.
              ADD wa_covp-wtgbtr TO wa_saida-vl_peca.

****Comentado porque as contas de materiais deiversos foram incorporadas e peças e materiais de manuteção.
*         Total materiais diversos
*            WHEN: '412035' OR '412039' OR '412040' OR '412045'.
*              ADD WA_COVP-WTGBTR TO WA_SAIDA-VL_MATE.
*         Total terceiros
            WHEN: '422511' OR '412512' OR '412513' OR '412514' OR '412515' OR '412516' OR '412517' OR '412525' OR '412526' OR '412563'.
              ADD wa_covp-wtgbtr TO wa_saida-vl_terc.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    wa_saida-vl_total = wa_saida-vl_comb  +
                        wa_saida-vl_fl_lb +
                        wa_saida-vl_peca  +
                        wa_saida-vl_pneu  +
                        wa_saida-vl_mate  +
                        wa_saida-vl_terc.

    TRY.
        wa_saida-custo_hr = wa_saida-vl_total / wa_saida-vl_cont.
      CATCH cx_root.
        wa_saida-custo_hr = 0.
    ENDTRY.

** Tipo de Equipamento
    READ TABLE it_t370k INTO wa_t370k WITH KEY eqart = wa_equi-eqart.
    wa_saida-eartx = wa_t370k-eartx.

** Trazer apenas equipamentos com custo conforme flag na tela de parâmetros
    IF  p_scusto IS INITIAL
    AND wa_saida-vl_total IS INITIAL.
      CONTINUE.
    ENDIF.

**  Mudar cor das células
    FREE it_color.

    CLEAR wa_color.
    MOVE 'VL_TOTAL' TO wa_color-fname.
    MOVE '4'        TO wa_color-color-col.
    MOVE '1'        TO wa_color-color-int.
    MOVE '1'        TO wa_color-color-inv.
    APPEND wa_color TO it_color.

    CLEAR wa_color.
    MOVE 'VL_CONT' TO wa_color-fname.
    MOVE '5'        TO wa_color-color-col.
    MOVE '0'        TO wa_color-color-int.
    MOVE '0'        TO wa_color-color-inv.
    APPEND wa_color TO it_color.

    CLEAR wa_color.
    MOVE 'CUSTO_HR' TO wa_color-fname.
    MOVE '5'        TO wa_color-color-col.
    MOVE '0'        TO wa_color-color-int.
    MOVE '0'        TO wa_color-color-inv.
    APPEND wa_color TO it_color.

    CLEAR wa_color.
    MOVE 'VL_ABAST' TO wa_color-fname.
    MOVE '5'        TO wa_color-color-col.
    MOVE '0'        TO wa_color-color-int.
    MOVE '0'        TO wa_color-color-inv.
    APPEND wa_color TO it_color.


    wa_saida-safra = p_safra-low.
    wa_saida-cell_color[] = it_color[].

*    WA_SAIDA-AUFNR = WA_AUFK-AUFNR.
    APPEND wa_saida TO it_saida.
    CLEAR: wa_saida, wa_imrg_cont, wa_imrg_cont, wa_t370k, wa_aufk, wa_equi, wa_cosp_lin, wa_covp, wa_imrg_comb, wa_imrg_cont.

  ENDLOOP.


ENDFORM.                    " F_ORGANIZAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_EXIBIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_exibir_dados USING p_tipo TYPE char10.
  CLEAR: wa_layout, wa_stable.

  FREE it_function.

  CASE p_tipo.
    WHEN 'GERAL'.
      IF  it_saida IS INITIAL
      AND p_equi   IS NOT INITIAL.
        MESSAGE 'Verificar equipamento ou periodo informado' TYPE 'I'.
      ELSE.
        IF obj_cont_geral IS NOT INITIAL.
          CALL METHOD obj_grid_geral->free.
          CALL METHOD obj_cont_geral->free.

          FREE: obj_cont_geral,
                obj_grid_geral.
        ENDIF.

        CREATE OBJECT obj_cont_geral
          EXPORTING
            container_name = 'OBJ_AREA'.

        CREATE OBJECT obj_grid_geral
          EXPORTING
            i_parent = obj_cont_geral.

        wa_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_paste.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_undo.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_copy.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_cut.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_cut.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_check.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_refresh.
        APPEND wa_function TO it_function.

        PERFORM f_montar_alv USING p_tipo.
        PERFORM f_ordenar_alv USING p_tipo.

        wa_layout-zebra       = 'X'.
        wa_stable-row         = 'X'.
        wa_layout-cwidth_opt  = 'X'.
        wa_layout-info_fname  = space .
        wa_layout-sel_mode    = 'A'.
        wa_layout-ctab_fname  = 'CELL_COLOR'.

        SORT it_saida BY safra equnr swerk.

        CALL METHOD obj_grid_geral->set_table_for_first_display
          EXPORTING
            it_toolbar_excluding = it_function
            is_layout            = wa_layout
            i_save               = 'X'
          CHANGING
            it_fieldcatalog      = it_fieldcatalog[]
            it_sort              = it_sort[]
            it_outtab            = it_saida[].

        SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_grid_geral.

        CALL METHOD obj_grid_geral->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified.

        CALL METHOD obj_grid_geral->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_enter.

        CALL METHOD obj_grid_geral->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.
    WHEN 'DETAIL'.
      IF obj_cont_deta IS INITIAL.
        wa_layout-zebra      = 'X'.
        wa_stable-row        = 'X'.
*        WA_LAYOUT-STYLEFNAME = 'STYLE'.
*        WA_LAYOUT-CWIDTH_OPT = 'X'.
        wa_layout-info_fname = space .
        wa_layout-sel_mode   = 'A'.

        CREATE OBJECT obj_cont_deta
          EXPORTING
            container_name = 'OBJ_AREA2'.

        CREATE OBJECT obj_grid_deta
          EXPORTING
            i_parent = obj_cont_deta.

        wa_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_paste.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_undo.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_copy.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_cut.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_loc_cut.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_check.
        APPEND wa_function TO it_function.
        wa_function = cl_gui_alv_grid=>mc_fc_refresh.
        APPEND wa_function TO it_function.

        PERFORM f_montar_alv USING p_tipo.
        PERFORM f_ordenar_alv USING p_tipo.

        CALL METHOD obj_grid_deta->set_table_for_first_display
          EXPORTING
            it_toolbar_excluding = it_function
            is_layout            = wa_layout
            i_save               = 'X'
          CHANGING
            it_fieldcatalog      = it_fieldcatalog[]
            it_sort              = it_sort[]
            it_outtab            = it_detail[].

        SET HANDLER: lcl_eventos=>on_hotspot_click FOR obj_grid_deta.

        CALL METHOD obj_grid_deta->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_modified.

        CALL METHOD obj_grid_deta->register_edit_event
          EXPORTING
            i_event_id = cl_gui_alv_grid=>mc_evt_enter.
      ELSE.
        CALL METHOD obj_grid_deta->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.
  ENDCASE.
ENDFORM.                    " F_EXIBIR_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_montar_alv USING p_tipo TYPE char10.
  REFRESH it_fieldcatalog.
  CASE p_tipo.
    WHEN 'GERAL'.

      IF p_segreg IS NOT INITIAL.
        PERFORM f_grava_field USING:
          1  ' '     'DATAB'   'IT_SAIDA'   'DATAB'      'Dt Início'               ' '  ' '  ' '  ' '  ' '  ' ',
          2  ' '     'DATBI'   'IT_SAIDA'   'DATBI'      'Dt Fim'                  ' '  ' '  ' '  ' '  ' '  ' ',
          3  ' '     'TIMBI'   'IT_SAIDA'   'TIMBI'      'Hr Fim'                  ' '  ' '  ' '  ' '  ' '  ' '.
      ENDIF.

      PERFORM f_grava_field USING:
        4  '    '     'SAFRA'   'IT_SAIDA'   'SAFRA'      'Safra'                ' '  ' '  ' '  ' '  ' '  ' ',
        5  'ITOB'     'SWERK'   'IT_SAIDA'   'SWERK'      'Centro'               ' '  ' '  ' '  ' '  ' '  ' ',
        6  'T370K_T'  'EARTX'   'IT_SAIDA'   'EARTX'      'Tipo de Equipamento'  ' '  ' '  ' '  ' '  ' '  ' ',
        7  'EQUI'     'EQUNR'   'IT_SAIDA'   'EQUNR'      'Equipamento'          ' '  ' '  ' '  ' '  ' '  ' ',
        8  'FLEET'    'ZZIDADE_EQPTO'   'IT_SAIDA'   'ZZIDADE_EQPTO'      'Idade eqpto'          ' '  ' '  ' '  ' '  ' '  ' ',
        9  'EQUI'     'TIPBZ'   'IT_SAIDA'   'TIPBZ'      'Modelo'               ' '  ' '  ' '  ' '  ' '  ' ',
        9  'EQUI'     'HERST'   'IT_SAIDA'   'HERST'      'Fabricante'           ' '  ' '  ' '  ' '  ' '  ' ', "US #175265 - MMSILVA - 29.04.2025
       10  ' '        ' '       'IT_SAIDA'   'CONS_MED'   'Consumo Médio'        ' '  ' '  ' '  ' '  ' '  ' ',
       11  ' '        ' '       'IT_SAIDA'   'VL_COMB'    'Valor Combustivel'    ' '  ' '  ' '  ' '  'X'  ' ',
       12  ' '        ' '       'IT_SAIDA'   'VL_FL_LB'   'Lubrificante       '  ' '  ' '  ' '  ' '  'X'  ' ',
       13 ' '        ' '       'IT_SAIDA'   'VL_PECA'    'Peças/Acessorios'     ' '  ' '  ' '  ' '  'X'  ' ',
       14 ' '        ' '       'IT_SAIDA'   'VL_PNEU'    'Pneu/Camara'          ' '  ' '  ' '  ' '  'X'  ' ',
       15 ''         ' '       'IT_SAIDA'   'VL_TERC'    'Serviços'             ' '  ' '  ' '  ' '  'X'  ' ',
       16 ' '        ' '       'IT_SAIDA'   'VL_TOTAL'   'Valor Total'          ' '  ' '  ' '  ' '  ' '  ' ',
       17 ' '        ' '       'IT_SAIDA'   'VL_CONT'    'Hr/KM'                ' '  ' '  ' '  ' '  ' '  ' ',
       18 ' '        ' '       'IT_SAIDA'   'VL_ABAST'   'Qtde. Abastecida'     ' '  ' '  ' '  ' '  ' '  ' ',
       19 ' '        ' '       'IT_SAIDA'   'CUSTO_HR'   'Custo Hr/KM'          ' '  ' '  ' '  ' '  ' '  ' '.
    WHEN 'DETAIL'.
      PERFORM  f_grava_field USING:
        1  ' '      ' '       'IT_DETAIL'  'ORDERID'    'Ordem'                '15'  ' '  ' '  ' '  'X'  ' ',
        2  ' '      ' '       'IT_DETAIL'  'ORDER_TYPE' 'Tipo'                 '06'  ' '  ' '  ' '  ' '  ' ',
        3  ' '      ' '       'IT_DETAIL'  'SHORT_TEXT' 'Descrição'            '40'  ' '  ' '  ' '  ' '  ' ',
        4  ' '      ' '       'IT_DETAIL'  'VL_ORDER'   'Valor Ordem'          '12'  ' '  ' '  ' '  ' '  ' '.
  ENDCASE.
ENDFORM.                    "F_MONTAR_ALV
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_field USING VALUE(p_col_pos)       TYPE i
                         VALUE(p_ref_tabname)   LIKE dd02d-tabname
                         VALUE(p_ref_field)     LIKE dd03d-fieldname
                         VALUE(p_tabname)       LIKE dd02d-tabname
                         VALUE(p_field)         LIKE dd03d-fieldname
                         VALUE(p_text)          LIKE dd03p-scrtext_l
                         VALUE(p_outputlen)
                         VALUE(p_edit)
                         VALUE(p_sum)
                         VALUE(p_emphasize)
                         VALUE(p_hotspot)
                         VALUE(p_key).

  DATA: lv_cont TYPE string.

  CLEAR: wa_fieldcatalog, lv_cont.

  wa_fieldcatalog-col_pos       = p_col_pos.
  wa_fieldcatalog-ref_table     = p_ref_tabname.
  wa_fieldcatalog-ref_field     = p_ref_field.
  wa_fieldcatalog-tabname       = p_tabname.
  wa_fieldcatalog-fieldname     = p_field.
  wa_fieldcatalog-reptext       = p_text.
  wa_fieldcatalog-outputlen     = p_outputlen.
*  WA_FIELDCATALOG-NO_OUT        = ' '.
*  WA_FIELDCATALOG-EDIT          = P_EDIT.
*  WA_FIELDCATALOG-DO_SUM        = P_SUM.
*  WA_FIELDCATALOG-EMPHASIZE     = 'X'.
  wa_fieldcatalog-hotspot       = p_hotspot.
  wa_fieldcatalog-scrtext_s     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_m     = wa_fieldcatalog-reptext.
  wa_fieldcatalog-scrtext_l     = wa_fieldcatalog-reptext.

  APPEND wa_fieldcatalog TO it_fieldcatalog.

ENDFORM.                    " F_GRAVA_FIELD
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100'.
  SET TITLEBAR '0100'.

  PERFORM: f_exibir_dados USING 'GERAL'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'ATUALIZAR'.
      PERFORM: f_selecionar_dados,
               f_organizar_dados,
               f_exibir_dados  USING 'GERAL'.

    WHEN 'BACK' OR 'EXIT' OR 'LEAVE' OR 'CANCEL'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_ORDENAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ordenar_alv USING p_tipo.
  DATA lv_pos TYPE i.

  CLEAR lv_pos.

  REFRESH it_sort.

  CASE p_tipo .
    WHEN 'GERAL'.

      IF p_segreg IS NOT INITIAL.
        CLEAR wa_sort.
        ADD 1 TO lv_pos.
        wa_sort-spos      = lv_pos.
        wa_sort-fieldname = 'DATAB'.
        wa_sort-up        = 'X'.
        wa_sort-subtot    = ' '.
        wa_sort-group     = 'X'.
        APPEND wa_sort TO it_sort.

        CLEAR wa_sort.
        ADD 1 TO lv_pos.
        wa_sort-spos      = lv_pos.
        wa_sort-fieldname = 'DATBI'.
        wa_sort-up        = 'X'.
        wa_sort-subtot    = ' '.
        wa_sort-group     = 'X'.
        APPEND wa_sort TO it_sort.

        CLEAR wa_sort.
        ADD 1 TO lv_pos.
        wa_sort-spos      = lv_pos.
        wa_sort-fieldname = 'TIMBI'.
        wa_sort-up        = 'X'.
        wa_sort-subtot    = ' '.
        wa_sort-group     = 'X'.
        APPEND wa_sort TO it_sort.
      ENDIF.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'SAFRA'.
      wa_sort-up        = 'X'.
      wa_sort-subtot    = ' '.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'SWERK'.
      wa_sort-up        = 'X'.
      wa_sort-subtot    = ' '.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'EARTX'.
      wa_sort-up        = 'X'.
      wa_sort-subtot    = ' '.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'TIPBZ'.
      wa_sort-up        = 'X'.
      wa_sort-subtot    = ' '.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'EQUNR'.
      wa_sort-up        = 'X'.
      wa_sort-subtot    = ' '.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'VL_CONT'.
      wa_sort-up        = ' '.
      wa_sort-subtot    = 'X'.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.


      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'VL_ABAST'.
      wa_sort-up        = ' '.
      wa_sort-subtot    = 'X'.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'CUSTO_HR'.
      wa_sort-up        = ' '.
      wa_sort-subtot    = 'X'.
      wa_sort-group     = 'X'.
      APPEND wa_sort TO it_sort.

    WHEN 'DETA'.
      CLEAR wa_sort.
      ADD 1 TO lv_pos.
      wa_sort-spos      = lv_pos.
      wa_sort-fieldname = 'ORDERID'.
      wa_sort-up        = 'X'.
      wa_sort-subtot    = ' '.
      APPEND wa_sort TO it_sort.
  ENDCASE.
ENDFORM.                    " F_ORDENAR_ALV
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.

  CASE wg_opt.
    WHEN 'VL_COMB'.
      SET TITLEBAR '0200'.
    WHEN 'VL_FL_LB'.
      SET TITLEBAR '0300'.
    WHEN 'VL_PECA'.
      SET TITLEBAR '0400'.
    WHEN 'VL_PNEU'.
      SET TITLEBAR '0500'.
    WHEN 'VL_MATE'.
      SET TITLEBAR '0600'.
    WHEN 'VL_TERC'.
      SET TITLEBAR '0700'.
  ENDCASE.

  PERFORM f_exibir_dados USING 'DETAIL'.

ENDMODULE.                 " STATUS_0200  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  F_shdb
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PROGRAM  text
*      -->P_DYNPRO   text
*      -->P_START    text
*      -->P_FNAM     text
*      -->P_FVAL     text
*----------------------------------------------------------------------*
FORM f_shdb  USING p_program p_dynpro p_start p_fnam p_fval.
  CLEAR wa_dta.
  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_start.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.                    "F_shdb


*&---------------------------------------------------------------------*
*&      Form  f_detail_valor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_detail_valor USING p_orderid    TYPE aufk-aufnr
                          p_tipo       TYPE char10
                          p_vl_order   TYPE imrc_totac
                          p_order_type TYPE aufk-auart
                          p_short_text TYPE aufk-ktext
                          p_enter_date TYPE aufk-erdat.
  READ TABLE it_detail INTO wa_detail WITH KEY orderid = p_orderid.
  IF sy-subrc IS INITIAL.
    ADD p_vl_order TO wa_detail-vl_order.
    MODIFY it_detail FROM wa_detail TRANSPORTING vl_order WHERE orderid = p_orderid.
  ELSE.
    wa_detail-orderid    = p_orderid.
    wa_detail-tipo       = p_tipo.
    wa_detail-vl_order   = p_vl_order.
    wa_detail-order_type = p_order_type.
    wa_detail-short_text = p_short_text.
    wa_detail-enter_date = p_enter_date.
    APPEND wa_detail TO it_detail.
  ENDIF.
ENDFORM.                    "f_detail_valor

*&---------------------------------------------------------------------*
*&      Form  BUSCA_MODELO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TIPO     text
*----------------------------------------------------------------------*
FORM busca_modelo USING p_tipo.
  DATA: BEGIN OF tl_temp OCCURS 0,
          typbz TYPE zpmr0001-typbz,
        END OF tl_temp.

  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  IF p_tipo = 'FABRICANTE'.
    SELECT DISTINCT herst
      INTO TABLE tl_temp
      FROM zpmr0001.
  ELSEIF p_tipo = 'MODELO'.
    SELECT DISTINCT typbz
      INTO TABLE tl_temp
      FROM zpmr0001.
  ENDIF.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'TYPBZ'
        dynpprog        = sy-repid
        dynpnr          = sy-dynnr
        dynprofield     = 'ZPMR0001-TYPBZ'
        value_org       = 'S'
      TABLES
        value_tab       = tl_temp
        return_tab      = tl_return_tab
        dynpfld_mapping = tl_dselc.

  ENDIF.
ENDFORM.                    " BUSCA_MODELO
*&---------------------------------------------------------------------*
*&      Form  VALIDA_PERIODO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_valida_periodo .

  IF p_data IS NOT INITIAL.
    SEARCH p_data-low FOR '/'.
    IF sy-fdpos NE 4.
      MESSAGE 'Formato inválido no período. Usar como XXXX/XX.' TYPE 'E'.
    ENDIF.

    IF p_data-high IS NOT INITIAL.
      SEARCH p_data-high FOR '/'.
      IF sy-fdpos NE 4.
        MESSAGE 'Formato inválido no período. Usar como XXXX/XX.' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_safra IS NOT INITIAL.
    SEARCH p_safra-low FOR '/'.
    IF sy-fdpos NE 2.
      MESSAGE 'Formato inválido na safra. Usar como XX/XX.' TYPE 'E'.
    ENDIF.
  ENDIF.

  IF p_safra IS NOT INITIAL.
    FREE: p_data.
  ELSE.
    FREE: p_safra.
  ENDIF.
ENDFORM.                    " VALIDA_PERIODO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_CLASSE_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_classe_custo .
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lr_kstar           TYPE RANGE OF s_kstar,
        lt_returns         TYPE TABLE OF bapiret2,
        lt_costlist        TYPE TABLE OF bapi1030_celist,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_langu           TYPE bapi0015_10,
        lv_controllingarea TYPE bapi1030_gen-co_area,
        lv_costelement     TYPE bapi1030_gen-cost_elem,
        lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA

  IF it_cskb IS INITIAL.
* ---> S4 Migration - 18/07/2023 - CA
*    SELECT DISTINCT c~kstar m~mctxt
*    FROM cskb AS c
*    INNER JOIN m_kartc AS m ON c~kstar = m~kstar
*    INTO TABLE it_cskb
*    WHERE m~spras EQ sy-langu
*     AND  c~kstar IN ('0000412012',
*                      '0000412013',
*                      '0000412014',
*                      '0000412015',
*                      '0000412017',
*                      '0000412542',
*                      '0000412018',
*                      '0000412104',
*                      '0000412016',
*                      '0000412044',
*                      '0000412103',
*                      '0000412035',
*                      '0000412039',
*                      '0000412040',
*                      '0000412045',
*                      '0000422511',
*                      '0000412512',
*                      '0000412513',
*                      '0000412514',
*                      '0000412515',
*                      '0000412516',
*                      '0000412517',
*                      '0000412043', "
*                      '0000412046', "
*                      '0000412100', "
*                      '0000412101', "
*                      '0000412102', "
*                      '0000412103', "
*                      '0000412106', "
*                      '0000412107', "
*                      '0000412108', "
*                      '0000412199', "
*                      '0000412561' )
*      AND c~kstar IN p_kstar.

    lr_kstar = VALUE #(
    (  option = 'EQ' sign = 'I' low = '0000412012' )
    (  option = 'EQ' sign = 'I' low = '0000412013' )
    (  option = 'EQ' sign = 'I' low = '0000412014' )
    (  option = 'EQ' sign = 'I' low = '0000412015' )
    (  option = 'EQ' sign = 'I' low = '0000412017' )
    (  option = 'EQ' sign = 'I' low = '0000412542' )
    (  option = 'EQ' sign = 'I' low = '0000412018' )
    (  option = 'EQ' sign = 'I' low = '0000412104' )
    (  option = 'EQ' sign = 'I' low = '0000412016' )
    (  option = 'EQ' sign = 'I' low = '0000412044' )
    (  option = 'EQ' sign = 'I' low = '0000412103' )
    (  option = 'EQ' sign = 'I' low = '0000412035' )
    (  option = 'EQ' sign = 'I' low = '0000412039' )
    (  option = 'EQ' sign = 'I' low = '0000412040' )
    (  option = 'EQ' sign = 'I' low = '0000412045' )
    (  option = 'EQ' sign = 'I' low = '0000422511' )
    (  option = 'EQ' sign = 'I' low = '0000412512' )
    (  option = 'EQ' sign = 'I' low = '0000412513' )
    (  option = 'EQ' sign = 'I' low = '0000412514' )
    (  option = 'EQ' sign = 'I' low = '0000412515' )
    (  option = 'EQ' sign = 'I' low = '0000412516' )
    (  option = 'EQ' sign = 'I' low = '0000412517' )
    (  option = 'EQ' sign = 'I' low = '0000412043' )
    (  option = 'EQ' sign = 'I' low = '0000412046' )
    (  option = 'EQ' sign = 'I' low = '0000412100' )
    (  option = 'EQ' sign = 'I' low = '0000412101' )
    (  option = 'EQ' sign = 'I' low = '0000412102' )
    (  option = 'EQ' sign = 'I' low = '0000412103' )
    (  option = 'EQ' sign = 'I' low = '0000412106' )
    (  option = 'EQ' sign = 'I' low = '0000412107' )
    (  option = 'EQ' sign = 'I' low = '0000412108' )
    (  option = 'EQ' sign = 'I' low = '0000412199' )
    (  option = 'EQ' sign = 'I' low = '0000412561' ) ).


    SELECT kokrs
      FROM tka01
      INTO TABLE @DATA(lt_tka01)
      WHERE kokrs <> @space.

    IF sy-subrc = 0.

      LOOP AT lt_tka01 INTO DATA(ls_tka01).

        lv_controllingarea = ls_tka01-kokrs.
        lv_keydate         = sy-datum.
        ls_langu-langu     = sy-langu(1).
        ls_langu-langu_iso = sy-langu.

        LOOP AT lr_kstar INTO DATA(ls_kstar).

          lv_costelement = ls_kstar-low.

          CLEAR: lt_returns[], ls_coeldes.

          CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
            EXPORTING
              controllingarea   = lv_controllingarea
              costelement       = lv_costelement
              keydate           = lv_keydate
              language          = ls_langu
            IMPORTING
              costelementdetail = ls_coeldes
            TABLES
              return            = lt_returns.

          READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc <> 0.
            wa_cskb-kstar = lv_costelement.
            wa_cskb-mctxt = ls_coeldes-name.

            APPEND wa_cskb TO it_cskb.
            CLEAR wa_cskb.
          ENDIF.

          CLEAR: ls_kstar, lv_costelement.
        ENDLOOP.

        IF p_kstar IS NOT INITIAL.

          FREE: lt_costlist, lt_returns.

          CALL FUNCTION 'K_COSTELEM_BAPI_GETLIST'
            EXPORTING
              controllingarea = lv_controllingarea
              date            = lv_keydate
            TABLES
              costelementlist = lt_costlist
              return          = lt_returns.


          DELETE lt_costlist WHERE cost_elem NOT IN p_kstar.

          LOOP AT lt_costlist INTO DATA(ls_costlist).

            wa_cskb-kstar = ls_costlist-cost_elem.
            wa_cskb-mctxt = ls_costlist-name.

            APPEND wa_cskb TO it_cskb.
            CLEAR wa_cskb.

            CLEAR: ls_costlist.
          ENDLOOP.
        ENDIF.

        CLEAR: ls_tka01.
      ENDLOOP.

      SORT it_cskb BY kstar.
      DELETE ADJACENT DUPLICATES FROM it_cskb COMPARING kstar.
    ENDIF.
* <--- S4 Migration - 18/07/2023 - CA
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'KSTAR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'CSKB-KSTAR'
      value_org       = 'S'
    TABLES
      value_tab       = it_cskb
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDFORM.                    " BUSCA_CLASSE_CUSTO
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_CLASSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_valida_classe .

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lr_kstar           TYPE RANGE OF s_kstar,
        lt_returns         TYPE TABLE OF bapiret2,
        lt_costlist        TYPE TABLE OF bapi1030_celist,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_langu           TYPE bapi0015_10,
        lv_controllingarea TYPE bapi1030_gen-co_area,
        lv_costelement     TYPE bapi1030_gen-cost_elem,
        lv_keydate         TYPE bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA

  IF it_cskb IS INITIAL.
* ---> S4 Migration - 18/07/2023 - CA
*    SELECT DISTINCT c~kstar m~mctxt
*    FROM cskb AS c
*    INNER JOIN m_kartc AS m ON c~kstar = m~kstar
*    INTO TABLE it_cskb
*    WHERE m~spras EQ sy-langu
*     AND  c~kstar IN ('0000412012',
*                      '0000412013',
*                      '0000412014',
*                      '0000412015',
*                      '0000412017',
*                      '0000412542',
*                      '0000412018',
*                      '0000412104',
*                      '0000412016',
*                      '0000412044',
*                      '0000412103',
*                      '0000412035',
*                      '0000412039',
*                      '0000412040',
*                      '0000412045',
*                      '0000422511',
*                      '0000412512',
*                      '0000412513',
*                      '0000412514',
*                      '0000412515',
*                      '0000412516',
*                      '0000412517',
*                      '0000412043', "
*                      '0000412046', "
*                      '0000412100', "
*                      '0000412101', "
*                      '0000412102', "
*                      '0000412103', "
*                      '0000412106', "
*                      '0000412107', "
*                      '0000412108', "
*                      '0000412199', "
*                      '0000412561')
*      AND c~kstar IN p_kstar.

    lr_kstar = VALUE #(
    (  option = 'EQ' sign = 'I' low = '0000412012' )
    (  option = 'EQ' sign = 'I' low = '0000412013' )
    (  option = 'EQ' sign = 'I' low = '0000412014' )
    (  option = 'EQ' sign = 'I' low = '0000412015' )
    (  option = 'EQ' sign = 'I' low = '0000412017' )
    (  option = 'EQ' sign = 'I' low = '0000412542' )
    (  option = 'EQ' sign = 'I' low = '0000412018' )
    (  option = 'EQ' sign = 'I' low = '0000412104' )
    (  option = 'EQ' sign = 'I' low = '0000412016' )
    (  option = 'EQ' sign = 'I' low = '0000412044' )
    (  option = 'EQ' sign = 'I' low = '0000412103' )
    (  option = 'EQ' sign = 'I' low = '0000412035' )
    (  option = 'EQ' sign = 'I' low = '0000412039' )
    (  option = 'EQ' sign = 'I' low = '0000412040' )
    (  option = 'EQ' sign = 'I' low = '0000412045' )
    (  option = 'EQ' sign = 'I' low = '0000422511' )
    (  option = 'EQ' sign = 'I' low = '0000412512' )
    (  option = 'EQ' sign = 'I' low = '0000412513' )
    (  option = 'EQ' sign = 'I' low = '0000412514' )
    (  option = 'EQ' sign = 'I' low = '0000412515' )
    (  option = 'EQ' sign = 'I' low = '0000412516' )
    (  option = 'EQ' sign = 'I' low = '0000412517' )
    (  option = 'EQ' sign = 'I' low = '0000412043' )
    (  option = 'EQ' sign = 'I' low = '0000412046' )
    (  option = 'EQ' sign = 'I' low = '0000412100' )
    (  option = 'EQ' sign = 'I' low = '0000412101' )
    (  option = 'EQ' sign = 'I' low = '0000412102' )
    (  option = 'EQ' sign = 'I' low = '0000412103' )
    (  option = 'EQ' sign = 'I' low = '0000412106' )
    (  option = 'EQ' sign = 'I' low = '0000412107' )
    (  option = 'EQ' sign = 'I' low = '0000412108' )
    (  option = 'EQ' sign = 'I' low = '0000412199' )
    (  option = 'EQ' sign = 'I' low = '0000412561' ) ).


    SELECT kokrs
      FROM tka01
      INTO TABLE @DATA(lt_tka01)
      WHERE kokrs <> @space.

    IF sy-subrc = 0.

      LOOP AT lt_tka01 INTO DATA(ls_tka01).

        lv_controllingarea = ls_tka01-kokrs.
        lv_keydate         = sy-datum.
        ls_langu-langu     = sy-langu(1).
        ls_langu-langu_iso = sy-langu.

        LOOP AT lr_kstar INTO DATA(ls_kstar).

          lv_costelement = ls_kstar-low.

          CLEAR: lt_returns[], ls_coeldes.

          CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
            EXPORTING
              controllingarea   = lv_controllingarea
              costelement       = lv_costelement
              keydate           = lv_keydate
              language          = ls_langu
            IMPORTING
              costelementdetail = ls_coeldes
            TABLES
              return            = lt_returns.

          READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
          IF sy-subrc <> 0.
            wa_cskb-kstar = lv_costelement.
            wa_cskb-mctxt = ls_coeldes-name.

            APPEND wa_cskb TO it_cskb.
            CLEAR wa_cskb.
          ENDIF.

          CLEAR: ls_kstar, lv_costelement.
        ENDLOOP.

        IF p_kstar IS NOT INITIAL.

          FREE: lt_costlist, lt_returns.

          CALL FUNCTION 'K_COSTELEM_BAPI_GETLIST'
            EXPORTING
              controllingarea = lv_controllingarea
              date            = lv_keydate
            TABLES
              costelementlist = lt_costlist
              return          = lt_returns.


          DELETE lt_costlist WHERE cost_elem NOT IN p_kstar.

          LOOP AT lt_costlist INTO DATA(ls_costlist).

            wa_cskb-kstar = ls_costlist-cost_elem.
            wa_cskb-mctxt = ls_costlist-name.

            APPEND wa_cskb TO it_cskb.
            CLEAR wa_cskb.

            CLEAR: ls_costlist.
          ENDLOOP.
        ENDIF.

        CLEAR: ls_tka01.
      ENDLOOP.

      SORT it_cskb BY kstar.
      DELETE ADJACENT DUPLICATES FROM it_cskb COMPARING kstar.
    ENDIF.
* <--- S4 Migration - 18/07/2023 - CA
  ENDIF.

* ---> S4 Migration - 18/07/2023 - CA
  IF it_cskb[] IS INITIAL.
*  IF sy-subrc IS NOT INITIAL.
* <--- S4 Migration - 18/07/2023 - CA
    MESSAGE 'Verificar classe de custos informada' TYPE 'E'.
  ENDIF.

ENDFORM.                    " F_VALIDA_CLASSE
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2988   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.

  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  DATA: wl_cont    TYPE sy-tabix,
        wl_mode(1).

*  FREE IT_MSG .
  CLEAR wl_mode.
  CLEAR wl_cont.
  CLEAR it_msg.
  wl_mode = 'E'.
*  BREAK-POINT.
  CALL TRANSACTION p_trans USING ti_bdcdata
                           MODE wl_mode
                           MESSAGES INTO it_msg.
  COMMIT WORK.
  WAIT UP TO 10 SECONDS.

  CLEAR: wl_cont.

  IF line_exists( it_msg[ msgtyp = 'A' ] ).
    p_erro = abap_true.
  ELSE.
    IF line_exists( it_msg[ msgtyp = 'E' ] ).
      p_erro = abap_true.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2074   text
*      -->P_2075   text
*      -->P_2076   text
*      -->P_2077   text
*      -->P_2078   text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.

  APPEND VALUE #(
                program   = p_program
                dynpro    = p_dynpro
                dynbegin  = p_start
                fnam      = p_fnam
                fval      = p_fval
  ) TO ti_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  CONFIG_FIELDS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE config_fields_0100 OUTPUT.

*  LOOP AT SCREEN.
*    IF rb_p IS NOT INITIAL.
*      IF screen-group1 = 'B'.
*        screen-active = 0.
*      ENDIF.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  CHECK_CAMP_OBRIG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_camp_obrig .
  CLEAR: lv_erro.
  REFRESH: it_saida.

  IF p_swerk IS INITIAL.
    MESSAGE 'Preencha o centro' TYPE 'I'.
    lv_erro = abap_true.
  ENDIF.

  CHECK lv_erro IS INITIAL.

  IF p_eqtyp IS INITIAL.
    MESSAGE 'Preencha a categoria' TYPE 'I'.
    lv_erro = abap_true.
  ENDIF.

  CHECK lv_erro IS INITIAL.

  IF rb_s IS NOT INITIAL.
    IF p_safra IS INITIAL.
      MESSAGE 'Preencha a safra' TYPE 'I'.
      lv_erro = abap_true.
    ENDIF.
  ELSE.
    IF p_data IS INITIAL.
      MESSAGE 'Preencha o periodo' TYPE 'I'.
      lv_erro = abap_true.
    ENDIF.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_layout .
  DATA: ws_zpmt0072 TYPE zpmt0072.

  CLEAR: ws_zpmt0072,  comm,  comm0, comm1, comm2.
  IF p_safra IS NOT INITIAL.
    LOOP AT p_safra.
      CLEAR: ws_zpmt0072.
      SELECT SINGLE * FROM zpmt0072 INTO ws_zpmt0072 WHERE werks IN p_swerk AND safra EQ p_safra-low.
      IF sy-subrc EQ 0.
        CASE sy-tabix.
          WHEN 1.
            comm  =  |Periodo de safra|.
            comm0 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 2.
            comm1 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 3.
            comm2 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 4.
            comm3 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 5.
            comm4 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 6.
            comm5 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 7.
            comm6 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 8.
            comm7 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN 9.
            comm8 =  |Safra { p_safra-low }:       inicio { ws_zpmt0072-mes_ini } / { ws_zpmt0072-ano_ini }  até  { ws_zpmt0072-mes_fim  } / {  ws_zpmt0072-ano_fim }|.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.


ENDFORM.
