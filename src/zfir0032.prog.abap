*&---------------------------------------------------------------------*
*& Report   ZFIR0032
*&
*&---------------------------------------------------------------------*
*&TITULO: COCKIPT – Geração Solicitação Adiantamento
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 31.07.2013
*&TRANSAÇÃO: ZFI0026
*&---------------------------------------------------------------------*


REPORT  zfir0032.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
TYPE-POOLS: vrm.
DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zfit0045.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*

DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

TYPES:
  BEGIN OF ty_zfit0045,
    bukrs         TYPE zfit0045-bukrs,
    nro_sol       TYPE zfit0045-nro_sol,
    ebeln         TYPE zfit0045-ebeln,
    lifnr         TYPE zfit0045-lifnr,
    dt_pgto       TYPE zfit0045-dt_pgto,
    moeda_pgto    TYPE zfit0045-moeda_pgto,
    motivo        TYPE zfit0045-motivo,
    dep_resp      TYPE zfit0045-dep_resp,
    resp_neg      TYPE zfit0045-resp_neg,
    usnam         TYPE zfit0045-usnam,
    status        TYPE zfit0045-status,
    zlsch         TYPE zfit0045-zlsch,
    bvtyp         TYPE zfit0045-bvtyp,
    hbkid         TYPE zfit0045-hbkid,
    hbkid_e       TYPE zfit0045-hbkid_e,
    identificador TYPE zfit0045-identificador,
    sgtxt         TYPE zfit0045-sgtxt,
  END OF ty_zfit0045,

  BEGIN OF ty_zfit0046,
    nro_sol          TYPE zfit0046-nro_sol,
    ebeln            TYPE zfit0046-ebeln,
    ebelp            TYPE zfit0046-ebelp,
    anln1            TYPE zfit0046-anln1,
    anln2            TYPE zfit0046-anln2,
    vlr_adiantamento TYPE zfit0046-vlr_adiantamento,
  END OF ty_zfit0046,

  BEGIN OF ty_ekko,
    ebeln TYPE ekko-ebeln,
    frggr TYPE ekko-frggr,
    frgke TYPE ekko-frgke,
  END OF ty_ekko,

  BEGIN OF ty_ekpo,
    ebeln TYPE ekpo-ebeln,
    ebelp TYPE ekpo-ebelp,
    werks TYPE ekpo-werks,
  END OF ty_ekpo,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END OF ty_lfa1,

  BEGIN OF ty_t001,
    bukrs TYPE t001-bukrs,
    butxt TYPE t001-butxt,
  END OF ty_t001,

  BEGIN OF ty_saida,
    checkbox(1),
    butxt            TYPE t001-butxt,
    nro_sol          TYPE zfit0045-nro_sol,
    icon1(4),
    icon2(4),
    ebeln            TYPE zfit0046-ebeln,
    name1_f          TYPE lfa1-name1,
    dt_pgto          TYPE zfit0045-dt_pgto,
    vlr_adiantamento TYPE zfit0046-vlr_adiantamento,
    moeda_pgto       TYPE zfit0045-moeda_pgto,
    dep_resp         TYPE zfit0045-dep_resp,
    resp_neg         TYPE zfit0045-resp_neg,
    usnam            TYPE zfit0045-usnam,
    motivo           TYPE zfit0045-motivo,
    belnr            TYPE zfit0045-belnr,
    lifnr            TYPE zfit0045-lifnr,
    zlsch            TYPE bseg-zlsch,
    bvtyp            TYPE bseg-bvtyp,
    hbkid            TYPE bseg-hbkid,
    hbkid_e          TYPE bseg-hbkid,
    identificador    TYPE zfit0045-identificador,
    sgtxt            TYPE zfit0045-sgtxt,
  END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

DATA: ti_bdcdata  TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      t_messtab   TYPE TABLE OF bdcmsgcoll,

      it_zfit0045 TYPE TABLE OF ty_zfit0045,
      it_zfit0046 TYPE TABLE OF ty_zfit0046,
      it_ekko     TYPE TABLE OF ty_ekko,
      it_ekpo     TYPE TABLE OF ty_ekpo,
      it_lfa1     TYPE TABLE OF ty_lfa1,
      it_t001     TYPE TABLE OF ty_t001,

      it_saida    TYPE TABLE OF ty_saida,
      it_color    TYPE TABLE OF lvc_s_scol,
      tg_msg_ret  TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_cont     TYPE REF TO cl_gui_custom_container,
  wa_alv      TYPE REF TO cl_gui_alv_grid,
  wa_bdcdata  LIKE LINE OF ti_bdcdata,

  wa_zfit0045 TYPE ty_zfit0045,
  wl_zfit0045 TYPE zfit0045,
  wa_zfit0046 TYPE ty_zfit0046,
  wa_ekko     TYPE ty_ekko,
  wa_ekpo     TYPE ty_ekpo,
  wa_lfa1     TYPE ty_lfa1,
  wa_t001     TYPE ty_t001,
  wa_saida    TYPE ty_saida.


*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
DATA:
  it_fcat         TYPE TABLE OF ty_estrutura,
  s_variant       TYPE disvariant           , " Tabela Estrutura co
  t_top           TYPE slis_t_listheader,
  xs_events       TYPE slis_alv_event,
  events          TYPE slis_t_event,
  gd_layout       TYPE slis_layout_alv,
  t_print         TYPE slis_print_alv,
  v_report        LIKE sy-repid,
  t_sort          TYPE slis_t_sortinfo_alv WITH HEADER LINE,
  it_setleaf      LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
  estrutura       TYPE TABLE OF ty_estrutura,
  vg_i            TYPE i,
  wg_mensagem(30).

DATA: ok-code          TYPE sy-ucomm,
      tabix            TYPE sy-tabix,
      tabix2           TYPE sy-tabix,
      vnum(10)         TYPE c,
      vseq(10)         TYPE p,
      wl_erro(1),
      wg_documento(10),
      vstatus(1),
      indrow           TYPE lvc_t_row,
      w_ind            TYPE lvc_t_row WITH HEADER LINE,
      w_cont           TYPE i,
      w_contc(5),
      w_mensagem(50),
      w_flag(1)        VALUE ''.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
DATA: editcontainer   TYPE REF TO cl_gui_custom_container,
      cl_container    TYPE REF TO cl_gui_custom_container,
      editor          TYPE REF TO cl_gui_textedit,
      cl_container_95 TYPE REF TO cl_gui_docking_container,
      cl_container_05 TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id   TYPE REF TO cl_dd_document,
      cl_grid         TYPE REF TO cl_gui_alv_grid,
      wa_stable       TYPE lvc_s_stbl,
      wa_afield       TYPE lvc_s_fcat,
      it_fieldcat     TYPE lvc_t_fcat,
      w_fieldcat      TYPE lvc_s_fcat,
      i_sort          TYPE lvc_t_sort,
      wa_layout       TYPE lvc_s_layo,
      is_stable       TYPE lvc_s_stbl VALUE 'XX',
      wg_repname      LIKE sy-repid,
      wg_x_variant    LIKE disvariant,
      wg_exit(1)      TYPE c,
      wg_save(1)      TYPE c,
      wg_variant      LIKE disvariant,
      gt_f4           TYPE lvc_t_f4 WITH HEADER LINE.


*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_0               TYPE c VALUE '0',
           c_1               TYPE c VALUE '1',
           c_2               TYPE c VALUE '2',
           c_b               TYPE c VALUE 'B',
           c_s               TYPE c VALUE 'S',
           c_l               TYPE c VALUE 'L',
           c_x               TYPE c VALUE 'X',
           c_d               TYPE c VALUE 'D',
           c_k               TYPE c VALUE 'K',
           c_w               TYPE c VALUE 'W',
           c_f               TYPE c VALUE 'F',
           c_t               TYPE c VALUE 'T',
           c_i               TYPE c VALUE 'I',
           c_n               TYPE c VALUE 'N',
           c_h               TYPE c VALUE 'H',
           c_ag(2)           TYPE c VALUE 'AG',
           c_ne(2)           TYPE c VALUE 'NE',
           c_01(2)           TYPE c VALUE '01',
           c_30(2)           TYPE c VALUE '30',
           c_40(2)           TYPE c VALUE '40',
           c_50(4)           TYPE c VALUE '0050',
           c_76(2)           TYPE c VALUE '76',
           c_71(2)           TYPE c VALUE '71',
           c_72(2)           TYPE c VALUE '72',
           c_br(2)           TYPE c VALUE 'BR',
           c_lf(2)           TYPE c VALUE 'LF',
           c_lr(2)           TYPE c VALUE 'LR',
           c_z1(2)           TYPE c VALUE 'Z1',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_dg1(3)          TYPE c VALUE 'DG1',
           c_dg2(3)          TYPE c VALUE 'DG2',
           c_dummy_header(3) TYPE c VALUE '099',
           c_dummy_itens(3)  TYPE c VALUE '098',
           c_exit(4)         TYPE c VALUE 'EXIT',
           c_root(4)         TYPE c VALUE 'ROOT',
           c_minimizar(4)    TYPE c VALUE '@K2@',
           c_maximizar(4)    TYPE c VALUE '@K1@',
           c_back(4)         TYPE c VALUE 'BACK',
           c_save(4)         TYPE c VALUE 'SAVE',
           c_desat(5)        TYPE c VALUE 'DESAT',
           c_dmbtr(5)        TYPE c VALUE 'DMBTR',
           c_modif(5)        TYPE c VALUE 'MODIF',
           c_cancel(6)       TYPE c VALUE 'CANCEL',
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE'.

************************************************************************
* BLOQUEIO
************************************************************************
DATA: tl_enq     TYPE TABLE OF seqg3 WITH HEADER LINE,
      wl_num_enq TYPE sy-tabix,
      wl_arg     TYPE seqg3-garg,
      w_belnr    TYPE zfit0045-belnr,
      w_bsart    TYPE ekko-bsart.

************************************************************************
* D E F I N I T I O N
************************************************************************
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      catch_hotspot
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no.

    METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .


    METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD on_data_changed.

    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'ID_TRANSPORTE'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

*      SELECT SINGLE ID_TRANSPORTE DS_NOME_TRANSPOR
*        FROM ZNOM_TRANSPORTE
*        INTO WA_ZNOM_TRANSPORTE
*          WHERE ID_TRANSPORTE EQ LV_VALUE.
*
*      IF SY-SUBRC IS INITIAL.
*        MOVE WA_ZNOM_TRANSPORTE-DS_NOME_TRANSPOR TO LV_VALUE.
*        CALL METHOD ER_DATA_CHANGED->MODIFY_CELL
*          EXPORTING
*            I_ROW_ID    = LS_GOOD-ROW_ID
*            I_FIELDNAME = 'DS_NOME_TRANSPOR'
*            I_VALUE     = LV_VALUE.
*      ELSE.
*
*      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED
  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finisheD
  METHOD catch_hotspot.
    IF e_row_id IS NOT INITIAL.
      READ TABLE it_saida INTO wa_saida INDEX e_row_id.
      IF e_column_id = 'BELNR' AND wa_saida-belnr IS NOT INITIAL.
        SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
        SET PARAMETER ID 'BUK' FIELD wa_saida-butxt+0(4).
        SET PARAMETER ID 'GJR' FIELD wa_saida-dt_pgto+0(4).
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ELSEIF  e_column_id = 'EBELN'.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "CATCH_HOTSPOT



ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

DATA:       event_receiver   TYPE REF TO lcl_event_receiver.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:  p_bukrs  FOR zfit0045-bukrs NO INTERVALS NO-EXTENSION,
                   p_budat  FOR zfit0045-dt_pgto.
  PARAMETER:       p_stat   TYPE c  AS CHECKBOX DEFAULT 'X',
                   p_orig(10) AS LISTBOX VISIBLE LENGTH 10.


SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

  value-key =  'B'.
  value-text = 'Brasil'.
  APPEND value TO list.

  value-key =  'E'.
  value-text = 'Exterior'.
  APPEND value TO list.

  name = 'P_ORIG'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.

  IF p_orig IS INITIAL.
    p_orig = 'B'.
  ENDIF.



*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM:
            f_seleciona_dados, " Form seleciona dados
            f_saida, " Form de saida
            f_imprime_dados.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  SELECT  bukrs nro_sol ebeln lifnr dt_pgto moeda_pgto motivo dep_resp resp_neg usnam status zlsch bvtyp hbkid hbkid_e identificador sgtxt
    FROM zfit0045
    INTO TABLE it_zfit0045
    WHERE belnr EQ ''
    "AND STATUS  EQ 'A'
    AND bukrs IN p_bukrs
    AND dt_pgto IN p_budat
    AND orig_pgt = p_orig
    AND loekz = ''.

  CHECK sy-subrc = 0.

  SELECT  nro_sol ebeln  ebelp anln1 anln2 vlr_adiantamento
    FROM zfit0046
    INTO TABLE it_zfit0046
    FOR ALL ENTRIES IN it_zfit0045
    WHERE nro_sol EQ it_zfit0045-nro_sol.

  SELECT ebeln ebelp werks
    FROM ekpo
    INTO TABLE it_ekpo
    FOR ALL ENTRIES IN it_zfit0046
    WHERE ebeln EQ it_zfit0046-ebeln
    AND   ebelp EQ it_zfit0046-ebelp.

  SELECT lifnr name1
    FROM lfa1
    INTO TABLE it_lfa1
    FOR ALL ENTRIES IN it_zfit0045
    WHERE lifnr EQ it_zfit0045-lifnr.


  SELECT bukrs butxt
    FROM t001
    INTO TABLE it_t001
    FOR ALL ENTRIES IN it_zfit0045
    WHERE bukrs EQ it_zfit0045-bukrs.

  SELECT ebeln frggr frgke
    FROM ekko
    INTO TABLE it_ekko
    FOR ALL ENTRIES IN it_zfit0045
    WHERE ebeln EQ  it_zfit0045-ebeln.



ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .
  SORT : it_zfit0045 BY bukrs nro_sol ebeln,
         it_zfit0046 BY nro_sol ebeln ebelp,
         it_ekko     BY ebeln,
         it_ekpo     BY ebeln ebelp,
         it_lfa1     BY lifnr,
         it_t001     BY bukrs.

  LOOP AT it_zfit0045 INTO wa_zfit0045.
    wa_saida-checkbox           = ''.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zfit0045-bukrs BINARY SEARCH.
    CONCATENATE wa_t001-bukrs '-' wa_t001-butxt INTO  wa_saida-butxt.

    IF wa_zfit0045-status = 'A'.
      wa_saida-icon2   = icon_okay.
    ELSE.
      wa_saida-icon2   = icon_led_red.
    ENDIF.

    wa_saida-nro_sol            = wa_zfit0045-nro_sol.


    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zfit0045-lifnr BINARY SEARCH.
    CONCATENATE wa_lfa1-lifnr '-' wa_lfa1-name1 INTO wa_saida-name1_f.

    wa_saida-dt_pgto            = wa_zfit0045-dt_pgto.
    wa_saida-moeda_pgto         = wa_zfit0045-moeda_pgto.
    wa_saida-dep_resp           = wa_zfit0045-dep_resp.
    wa_saida-resp_neg           = wa_zfit0045-resp_neg.
    wa_saida-usnam              = wa_zfit0045-usnam.
    wa_saida-motivo             = wa_zfit0045-motivo.
    wa_saida-lifnr              = wa_zfit0045-lifnr.

    wa_saida-zlsch              = wa_zfit0045-zlsch.
    wa_saida-bvtyp              = wa_zfit0045-bvtyp.
    wa_saida-hbkid              = wa_zfit0045-hbkid.
    wa_saida-hbkid_e            = wa_zfit0045-hbkid_e.
    wa_saida-identificador      = wa_zfit0045-identificador.
    wa_saida-sgtxt              = wa_zfit0045-sgtxt.

    READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_zfit0045-ebeln BINARY SEARCH.
    IF ( wa_ekko-frgke = '2' ) OR ( wa_ekko-frggr IS INITIAL ). "Se estiver liberado ou não estiver em uma estratégia de aprovação OK.
      wa_saida-icon1   = icon_okay.
    ELSE.
      wa_saida-icon1   = icon_led_red.
    ENDIF.
    IF p_stat = 'X'.
      IF wa_saida-icon1   NE icon_okay OR
         wa_saida-icon2   NE icon_okay.
        CONTINUE.
      ENDIF.
    ENDIF.
    wa_saida-ebeln              = wa_zfit0045-ebeln.
    wa_saida-vlr_adiantamento = 0.
    LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = wa_zfit0045-nro_sol
                                         AND   ebeln   = wa_zfit0045-ebeln.
      ADD wa_zfit0046-vlr_adiantamento TO wa_saida-vlr_adiantamento.
    ENDLOOP.
    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.
  ENDLOOP.


ENDFORM.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .

  PERFORM f_alv_fieldcat.

  wa_layout-zebra      = 'X'.
  wa_layout-no_rowmove = 'X'.
  wa_layout-no_rowins  = 'X'.
  wa_layout-no_rowmark = space.
  wa_layout-grid_title = 'COCKPIT - Adiantamento'.
  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt   = 'X'.

  CALL SCREEN 0100.
ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .

  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CHECKBOX'.
  wa_afield-checkbox      = 'X'.
  wa_afield-scrtext_s = 'Chk'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = 'X'.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUTXT'.
  wa_afield-scrtext_s = 'Empresa'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NRO_SOL'.
  wa_afield-scrtext_s = 'Nro.Sol.'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON1'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'St.Ped.'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON2'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'St.Sol.'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'EBELN'.
  wa_afield-scrtext_s = 'Ped.Compra'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BELNR'.
  wa_afield-scrtext_s = 'Doc.Gerado'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1_F'.
  wa_afield-scrtext_s = 'Fornecedor'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_PGTO'.
  wa_afield-scrtext_s = 'Dt.Pgto'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'VLR_ADIANTAMENTO'.
  wa_afield-scrtext_s = 'Vlr.Adiantamento '.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOEDA_PGTO'.
  wa_afield-scrtext_s = 'Moeda Pgto'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DEP_RESP'.
  wa_afield-scrtext_s = 'Departamento'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'RESP_NEG'.
  wa_afield-scrtext_s = 'Resp.Negociação'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'USNAM'.
  wa_afield-scrtext_s = 'Solicitante'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOTIVO'.
  wa_afield-scrtext_s = 'Motivo'.
  wa_afield-scrtext_l = wa_afield-scrtext_s.
  wa_afield-scrtext_m = wa_afield-scrtext_s.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.



ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.
  SET TITLEBAR  'ZFTITLE'.


  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF NOT cl_grid IS INITIAL.

    PERFORM zf_alv_header.
    CALL METHOD cl_grid->refresh_table_display.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        no_margins = 'X'.

    PERFORM zf_alv_header .


    IF editcontainer IS INITIAL .
      CREATE OBJECT editcontainer
        EXPORTING
          container_name = 'HEADER'.
    ENDIF .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = editcontainer
      EXCEPTIONS
        html_display_error = 1.


    CREATE OBJECT cl_grid
      EXPORTING
        i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    wg_save = 'X'.
    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    wa_stable-row        = c_x.
    wg_x_variant-report  = sy-repid.
    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_variant      = wg_x_variant
        is_layout       = wa_layout
        i_save          = wg_save
      CHANGING
        it_fieldcatalog = it_fieldcat[]
        it_sort         = i_sort[]
        it_outtab       = it_saida[].

    gt_f4-fieldname = 'UMSKZ'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    APPEND gt_f4.

    CALL METHOD cl_grid->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->catch_hotspot           FOR cl_grid.
*    SET HANDLER EVENT_RECEIVER->ON_DATA_CHANGED_FINISHED   FOR CL_GRID.



  ENDIF.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv_header .
  DATA:   wl_data(10),
              wl_hora(8),
              wl_linha(60),
              wl_text TYPE sdydo_text_element.

*  WL_TEXT = ''.
*
*  CALL METHOD OBJ_DYNDOC_ID->ADD_TEXT
*    EXPORTING
*      TEXT         = WL_TEXT
*      SAP_STYLE    = CL_DD_AREA=>HEADING
*      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
*      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.



  IF p_bukrs   IS NOT INITIAL.
    CONCATENATE 'Empresa  :' p_bukrs-low
      INTO wl_linha SEPARATED BY space.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF p_budat IS NOT INITIAL.
    CONCATENATE p_budat-low+6(2) p_budat-low+4(2) p_budat-low+0(4) INTO  wl_data SEPARATED BY '.'.
    IF p_budat-high IS INITIAL.
      CONCATENATE 'Período  :' wl_data
      INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Período :' wl_data  INTO wl_linha SEPARATED BY space.
      CONCATENATE p_budat-high+6(2) p_budat-high+4(2) p_budat-high+0(4) INTO  wl_data SEPARATED BY '.'.
      CONCATENATE wl_linha 'à' wl_data  INTO wl_linha SEPARATED BY space.
    ENDIF.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF p_stat = 'X'.
    wl_text = 'Somente Aprovados'.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.


ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_linha(6),
        vmsg(50).
  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'GERAR'.
      DATA v_entra(1).
      LOOP AT it_saida INTO wa_saida WHERE checkbox = 'X'.
        CLEAR: wl_arg, wl_num_enq.
        REFRESH tl_enq.
        CONCATENATE sy-mandt wa_saida-nro_sol INTO wl_arg.
        CALL FUNCTION 'ENQUEUE_READ'
          EXPORTING
            gname  = 'ZFIT0045'
            garg   = wl_arg
            guname = space
          IMPORTING
            number = wl_num_enq
          TABLES
            enq    = tl_enq.
        IF wl_num_enq <> 0.
          v_entra = 'X'.
          READ TABLE tl_enq INDEX 1.
          MESSAGE e601(mc) WITH tl_enq-guname tl_enq-gname.
        ENDIF.
      ENDLOOP.
      IF v_entra = 'X'.
        EXIT.
      ENDIF.
      LOOP AT it_saida INTO wa_saida WHERE checkbox = 'X'.
        "Checar se já foi gerado doc em tela aberta
        CLEAR w_belnr.
        SELECT SINGLE zfit0045~belnr, ekko~bsart
          INTO ( @w_belnr, @w_bsart )
          FROM zfit0045
          INNER JOIN ekko ON ekko~ebeln = zfit0045~ebeln
          WHERE nro_sol = @wa_saida-nro_sol.
        IF w_belnr IS NOT INITIAL.
          MOVE sy-tabix TO wl_linha.
          CONCATENATE 'Adiantamento já gerado contábil LINHA: ' wl_linha INTO  vmsg.
          MESSAGE vmsg TYPE 'I'.
*        ELSEIF 'PCEF_PSEF_YCEF_YSEF_ZEFI_ZGEF_ZEF' CS w_bsart.
*          MOVE sy-tabix TO wl_linha.
*          CONCATENATE 'Pedido de entrega futura, não gera doc LINHA: ' wl_linha INTO  vmsg.
*          MESSAGE vmsg TYPE 'I'.
        ELSE.
          "Bloqueia solicitação de adiantamento
          CALL FUNCTION 'ENQUEUE_EZFIT0045'
            EXPORTING
              nro_sol        = wa_saida-nro_sol
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              OTHERS         = 3.
          IF sy-subrc <> 0.
            v_entra = 'X'.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
          IF wa_saida-icon1   = icon_okay AND
             wa_saida-icon2   = icon_okay.
            IF wa_saida-belnr IS INITIAL.
              tabix   = sy-tabix.
              CLEAR wl_erro.
              PERFORM f_shdb2 CHANGING wa_saida wl_erro. "ALRS 14/12/2023
              IF wl_erro NE 'X' .
                v_entra = 'X'.
                UPDATE zfit0045 SET belnr = wg_documento
                                    status = 'A'
                WHERE nro_sol = wa_saida-nro_sol.

              ENDIF.
            ENDIF.
          ELSE.
            MOVE sy-tabix TO wl_linha.
            CONCATENATE 'Adiantamento não aprovado  LINHA: ' wl_linha INTO  vmsg.
            MESSAGE vmsg TYPE 'I'.
          ENDIF.

          "Desbloqueia solicitação de adiantamento
          CALL FUNCTION 'DEQUEUE_EZFIT0045'
            EXPORTING
              nro_sol = wa_saida-nro_sol.
        ENDIF.
      ENDLOOP.
      CLEAR v_entra.
      LOOP AT it_saida INTO wa_saida WHERE checkbox = 'X'.
        IF wa_saida-belnr IS INITIAL.
          v_entra = 'X'.
        ENDIF.
      ENDLOOP.
      IF  v_entra EQ 'X'.
        MESSAGE s836(sd) WITH 'Houve lançamentos com erro'.
      ENDIF.
      CALL METHOD cl_grid->refresh_table_display.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PORTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_porto INPUT.
  DATA: tl_return_tab2 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc2      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_porto OCCURS 0,
          ds_porto TYPE znom_transporte-ds_porto,
        END OF tl_porto.

  SELECT DISTINCT ds_porto
     FROM  znom_transporte INTO TABLE tl_porto.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DS_PORTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZMMR033'
      value_org       = 'S'
    TABLES
      value_tab       = tl_porto
      return_tab      = tl_return_tab2
      dynpfld_mapping = tl_dselc2.
ENDMODULE.                 " SEARCH_PORTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_NAVIO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_navio INPUT.
  DATA: tl_return_tab3 TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc3      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_navio OCCURS 0,
          ds_nome_transpor TYPE zmmt0033-ds_nome_transpor,
        END OF tl_navio.

  SELECT DISTINCT ds_nome_transpor
     FROM  zmmt0033 INTO TABLE tl_navio.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DS_NOME_TRANSPOR'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZMMR033'
      value_org       = 'S'
    TABLES
      value_tab       = tl_navio
      return_tab      = tl_return_tab3
      dynpfld_mapping = tl_dselc3.
ENDMODULE.                 " SEARCH_NAVIO  INPUT

*&---------------------------------------------------------------------*
*&      Form  F_SHDB2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ALV      text
*      -->P_ERRO     text
*----------------------------------------------------------------------*
FORM f_shdb2  CHANGING  p_alv LIKE wa_saida p_erro.
  DATA: vdata(10),
        vdata_v(10),
        wl_vlr(16),
        vgsber        TYPE t134g-gsber,
        v_xblnr       TYPE bkpf-xblnr,
        v_nro_sol(10),
        vlines        TYPE sy-tabix.

  SORT it_ekpo BY ebeln ebelp.

  vlines = 0.
  LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = p_alv-nro_sol
                                       AND   ebeln   = p_alv-ebeln.
    ADD 1 TO vlines.
  ENDLOOP.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = p_alv-nro_sol
    IMPORTING
      output = v_nro_sol.

  CONCATENATE 'SOL.' v_nro_sol INTO v_xblnr.
  REFRESH ti_bdcdata.
  CONCATENATE  sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO vdata SEPARATED BY '.'.
  CONCATENATE  p_alv-dt_pgto+6(2) p_alv-dt_pgto+4(2) p_alv-dt_pgto(4) INTO vdata_v SEPARATED BY '.'.
  tabix2 = 0.

  PERFORM f_bdc_data USING:
    'SAPMF05A'  '0112'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '/00',
    ''          ''      ''   'BKPF-BLDAT'       vdata,
    ''          ''      ''   'BKPF-BLART'       'KA',
    ''          ''      ''   'BKPF-BUKRS'       p_alv-butxt+0(4),
    ''          ''      ''   'BKPF-BUDAT'       vdata,
    ''          ''      ''   'BKPF-MONAT'       p_alv-dt_pgto+4(2),
    ''          ''      ''   'BKPF-WAERS'       p_alv-moeda_pgto,
    ''          ''      ''   'BKPF-XBLNR'       v_xblnr,
    ''          ''      ''   'RF05A-NEWKO'      p_alv-lifnr,
    ''          ''      ''   'RF05A-ZUMSK'      'A'.
  LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE nro_sol = p_alv-nro_sol
                                       AND   ebeln   = p_alv-ebeln.
    ADD 1 TO tabix2.
    WRITE: wa_zfit0046-vlr_adiantamento                TO wl_vlr.
    TRANSLATE wl_vlr USING '. ,'.
    CONDENSE wl_vlr NO-GAPS.
    CLEAR: wa_ekpo, vgsber.
    READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zfit0046-ebeln
                                             ebelp = wa_zfit0046-ebelp BINARY SEARCH.

    IF sy-subrc NE 0.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_zfit0046-ebeln.
      IF sy-subrc = 0.
        vgsber = wa_ekpo-werks.
      ENDIF.
    ELSE.
      vgsber = wa_ekpo-werks.
    ENDIF.

    IF wa_ekpo-werks IS NOT INITIAL.
      SELECT SINGLE *
         FROM t134g
         INTO @DATA(_t134g)
         WHERE werks = @wa_ekpo-werks.
      IF sy-subrc = 0.
        vgsber = _t134g-gsber.
      ENDIF.
    ENDIF.



    PERFORM f_bdc_data USING:
            'SAPMF05A'  '0304'  'X'  ''                  ' ',
            ''          ''      ''   'BDC_OKCODE'        '=ZK'.


    PERFORM f_bdc_data USING:
    ''          ''      ''   'BSEG-WRBTR'        wl_vlr,
    ''          ''      ''   'BSEG-GSBER'        vgsber,
    ''          ''      ''   'BSEG-ZFBDT'        vdata_v,
    ''          ''      ''   'BSEG-ZLSCH'        p_alv-zlsch,
    ''          ''      ''   'BSEG-KIDNO'        p_alv-identificador,
    ''          ''      ''   'BSEG-ANLN1'        wa_zfit0046-anln1,
    ''          ''      ''   'BSEG-ANLN2'        wa_zfit0046-anln2,
    ''          ''      ''   'BSEG-EBELN'        wa_zfit0046-ebeln,
    ''          ''      ''   'BSEG-EBELP'        wa_zfit0046-ebelp,
    ''          ''      ''   'BSEG-SGTXT'        p_alv-sgtxt.
    IF tabix2 = vlines.
      PERFORM f_bdc_data USING:
      'SAPMF05A'  '0332'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '/00'.
    ELSE.
      PERFORM f_bdc_data USING:
      'SAPMF05A'  '0332'  'X'  ''                 ' ',
      ''          ''      ''   'BDC_OKCODE'        '=NP'.
    ENDIF.

    IF p_alv-hbkid_e IS NOT INITIAL.
      CLEAR p_alv-hbkid.
    ENDIF.
    PERFORM f_bdc_data USING:
    ''          ''      ''   'BSEG-BVTYP'        p_alv-bvtyp,
    ''          ''      ''   'BSEG-HBKID'        p_alv-hbkid.
  ENDLOOP.

  PERFORM f_bdc_data USING:
    'SAPMF05A'  '0332'  'X'  ''                 ' ',
    ''          ''      ''   'BDC_OKCODE'        '=BU'.


  CLEAR p_erro.
  PERFORM zf_call_transaction USING 'F-47' CHANGING p_erro.
  IF p_erro = 'X'.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
    wa_saida-belnr = wg_documento.
    MODIFY it_saida FROM wa_saida INDEX tabix TRANSPORTING belnr.
  ENDIF.

  SELECT SINGLE * FROM zfit0045 INTO wl_zfit0045 WHERE nro_sol EQ p_alv-nro_sol.
  IF wl_zfit0045-orig_pgt EQ 'E'  AND wl_zfit0045-form_pgt = 'T'.
    IF wl_zfit0045-hbkid_e IS NOT INITIAL.
      WAIT UP TO 5 SECONDS.
      PERFORM f_gera_adto17.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_SHDB

*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WG_EXTERNO  text
*      <--P_WL_ERRO  text
*----------------------------------------------------------------------*
FORM f_shdb  USING  p_nro_sol  CHANGING p_erro.
  DATA: wl_setleaf TYPE setleaf,
        i_head     TYPE tbtcjob.

  DATA:   wl_job_id   LIKE tbtcjob-jobcount.
  DATA:   wl_jobn(32).

  DATA: BEGIN OF i_steplist OCCURS 10.
          INCLUDE STRUCTURE tbtcstep.
  DATA: END OF i_steplist.
  DATA : c_no(1) TYPE c . "value 'N', " Criação do job

  DATA: wl_tbtcjob  TYPE  tbtcjob,
        wl_tbtcstrt TYPE  tbtcstrt.

  DATA: lv_repname LIKE  rsvar-report.           " for variant handling
  DATA: iv_varname LIKE  raldb-variant VALUE 'SAP_UPGRADE'.
  DATA: iv_varianttext  LIKE  varit-vtext VALUE 'Upgrade variant'.
  DATA: wl_subrc TYPE sy-subrc.
  DATA: tt_reportparam TYPE TABLE OF  rsparams WITH HEADER LINE.

  SELECT SINGLE *
   FROM setleaf
   INTO wl_setleaf
    WHERE setname EQ 'MAGGI_JOB_USER'.

  IF sy-subrc NE 0.
    MESSAGE TEXT-e01 TYPE 'E'.
    EXIT.
  ENDIF.
  CONCATENATE 'Z_GRAVA_ZIB_ZADII' p_nro_sol  INTO wl_jobn SEPARATED BY '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-stepcount = 1.

  tt_reportparam-selname = 'P_SOL'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = p_nro_sol.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  lv_repname = 'Z_GRAVA_ZIB_ZADII'.
*    Write the variant first (Insert or Update)
  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
    EXPORTING
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    IMPORTING
      ev_funcrc             = wl_subrc
    TABLES
      tt_reportparam        = tt_reportparam
    EXCEPTIONS
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      OTHERS                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'Z_GRAVA_ZIB_ZADII'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  APPEND i_steplist.


  c_no = 'N'.
  CALL FUNCTION 'BP_JOB_CREATE'
    EXPORTING
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    IMPORTING
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    TABLES
      job_cr_steplist     = i_steplist
    EXCEPTIONS
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      OTHERS              = 5.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = wl_jobn
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.


ENDFORM.                    " F_SHDB
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2296   text
*      <--P_P_ERRO  text
*----------------------------------------------------------------------*
FORM zf_call_transaction  USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312',
             c_msgne LIKE it_msg-msgnr VALUE '539'.

  REFRESH it_msg.

  wl_mode = 'E'.
  CALL TRANSACTION p_trans WITHOUT AUTHORITY-CHECK
        USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg
        .

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

  CLEAR wg_documento.

  READ TABLE it_msg WITH KEY msgid = c_msgid
                             msgnr = c_msgnr
                             msgtyp = 'S'.

  IF sy-subrc = 0.
    MOVE it_msg-msgv1 TO wg_documento.
  ENDIF.

  IF  wg_documento IS INITIAL.
    p_erro = 'X'.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_documento
      IMPORTING
        output = wg_documento.
  ENDIF.



ENDFORM.                    " ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2362   text
*      -->P_2363   text
*      -->P_2364   text
*      -->P_2365   text
*      -->P_2366   text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA

FORM f_gera_adto17.
  DATA: vseq(10)      TYPE p,
        vlote_ad(10)  TYPE c,
        vnum2(11)     TYPE c,
        v_nro_sol(10),
        vtabkey       TYPE tiban-tabkey,
        vbvtyp        TYPE lfbk-bvtyp,
        vbvtyp2       TYPE lfbk-bvtyp.

  DATA: wa_zfit0036_ins  TYPE zfit0036.

  wl_zfit0045-belnr = wg_documento.
  " Gera numero do lote
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_INV'
    IMPORTING
      number      = vseq.
  vlote_ad = vseq .
  "
  " Sequencia "AC"
  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZID_FI17'
    IMPORTING
      number      = vseq.
  vnum2 = vseq .
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vnum2
    IMPORTING
      output = vnum2.
  "
  SELECT SINGLE *
  FROM zfit0043
  INTO @DATA(wa_zfit0043)
  WHERE tp_operacao = '08'
    AND spras = @sy-langu. "PGTO DE PEDIDOS/ZGL

  CLEAR wa_zfit0036_ins.
  CONCATENATE 'AC' vnum2 wl_zfit0045-lifnr INTO wa_zfit0036_ins-obj_key.
  "
  wa_zfit0036_ins-bukrs          = wl_zfit0045-bukrs.
  wa_zfit0036_ins-lote           = vlote_ad.
  wa_zfit0036_ins-invoice        = wa_zfit0043-ds_operacao.
  wa_zfit0036_ins-dt_pgto        = wl_zfit0045-dt_pgto.
  wa_zfit0036_ins-moeda_pgto     = wl_zfit0045-moeda_pgto.
*  WA_ZFIT0036_INS-VLR_PGTO       = 0.
  SELECT SUM( wrbtr )                  "#EC CI_DB_OPERATION_OK[2431747]
   INTO wa_zfit0036_ins-vlr_pgto
   FROM bseg
   WHERE bukrs = wl_zfit0045-bukrs
   AND   belnr = wl_zfit0045-belnr.

  wa_zfit0036_ins-hbkid          = wl_zfit0045-hbkid_e.  " Na ZFI0017 - indica o banco novamente?
  wa_zfit0036_ins-status         = 'L'. "Liberadas automaticamente pedido/ZGL
  wa_zfit0036_ins-forma_pg       = 'P'.
  wa_zfit0036_ins-motivo         = '08'. " payments of imports

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wl_zfit0045-nro_sol
    IMPORTING
      output = v_nro_sol.

  CONCATENATE 'SOL.' v_nro_sol INTO wa_zfit0036_ins-referencia.

  wa_zfit0036_ins-observacao = wl_zfit0045-sgtxt.
  wa_zfit0036_ins-operacao   = '08'.
  wa_zfit0036_ins-lifnr      = wl_zfit0045-lifnr.
  wa_zfit0036_ins-usuario    = sy-uname.
  wa_zfit0036_ins-data_atual = sy-datum.
  wa_zfit0036_ins-hora_atual = sy-uzeit.

  SELECT SINGLE *
     FROM lfbk
      INTO @DATA(wl_lfbk)
      WHERE lifnr = @wl_zfit0045-lifnr.

  IF wl_zfit0045-bvtyp IS NOT INITIAL.
    SELECT SINGLE *
         FROM lfbk
         INTO wl_lfbk
         WHERE lifnr = wl_zfit0045-lifnr
         AND   bvtyp = wl_zfit0045-bvtyp.
    IF sy-subrc NE 0.
      SELECT SINGLE *
          FROM lfbk
           INTO wl_lfbk
           WHERE lifnr = wl_zfit0045-lifnr.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
    FROM bnka
    INTO @DATA(wl_bnka)
    WHERE banks	=	@wl_lfbk-banks
    AND   bankl	=	@wl_lfbk-bankl.

  SELECT  *
    FROM tiban
    INTO TABLE @DATA(tl_tiban)
    WHERE banks   = @wl_lfbk-banks
    AND   bankl   = @wl_lfbk-bankl
    AND   bankn   = @wl_lfbk-bankn
    AND   tabname IN ('LFBK', 'BUT0BK')
    ORDER BY erdat DESCENDING.
  IF sy-subrc = 0.
    READ TABLE tl_tiban INTO DATA(wl_tiban) INDEX 1.
    wa_zfit0036_ins-iban_1  = wl_tiban-iban.
  ENDIF.

  wa_zfit0036_ins-bvtyp   = wl_lfbk-bvtyp.
  wa_zfit0036_ins-swift_1 = wl_bnka-swift.
  wa_zfit0036_ins-banks_1 = wl_lfbk-banks.
  wa_zfit0036_ins-bankl_1 = wl_lfbk-bankl.
  wa_zfit0036_ins-banka_1 = wl_bnka-banka.
  wa_zfit0036_ins-bankn_1 = wl_lfbk-bankn.
  "Não compensa o MEMO, sera feito nsa ZFI0017
  wa_zfit0036_ins-belnr_adt_c = wg_documento.
  vbvtyp2 = wl_zfit0045-bvtyp.
  CONCATENATE vbvtyp2+0(3) '2' INTO vbvtyp.

  SELECT SINGLE *
   FROM lfbk
   INTO wl_lfbk
   WHERE lifnr = wl_zfit0045-lifnr
   AND bvtyp  = vbvtyp.

  IF sy-subrc = 0.
    MOVE wl_zfit0045-lifnr TO vtabkey.
    SELECT SINGLE *
       FROM tiban
       INTO wl_tiban
       WHERE banks   =  wl_lfbk-banks
       AND   bankl   =  wl_lfbk-bankl
       AND   bankn   =  wl_lfbk-bankn
       AND   tabkey  =  vtabkey
       AND   tabname IN ('LFBK', 'BUT0BK').
    IF sy-subrc NE 0.
      CLEAR wl_tiban.
    ENDIF.
    SELECT SINGLE *
       FROM bnka
       INTO wl_bnka
       WHERE banks  = wl_lfbk-banks
       AND   bankl  = wl_lfbk-bankl.
    IF sy-subrc NE 0.
      CLEAR wl_bnka.
    ENDIF.
    wa_zfit0036_ins-bvtyp_2 = wl_lfbk-bvtyp.
    wa_zfit0036_ins-swift_2 = wl_bnka-swift.
    wa_zfit0036_ins-banks_2 = wl_lfbk-banks.
    wa_zfit0036_ins-bankl_2 = wl_lfbk-bankl.
    wa_zfit0036_ins-banka_2 = wl_bnka-banka.
    wa_zfit0036_ins-bankn_2 = wl_lfbk-bankn.

  ENDIF.

  INSERT INTO  zfit0036 VALUES wa_zfit0036_ins.
  IF sy-subrc NE 0.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK.
  ENDIF.


  CLEAR wa_zfit0036_ins.


ENDFORM.
