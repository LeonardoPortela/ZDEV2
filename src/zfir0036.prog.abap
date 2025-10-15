*&---------------------------------------------------------------------*
*& Report   ZFIR0036
*&
*&---------------------------------------------------------------------*
*&TITULO: Acompanhamento – Solicitação de Adiantamento
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 04.10.2013
*&TRANSAÇÃO: ZFI0034
*&---------------------------------------------------------------------*


REPORT  zfir0036.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.
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
    identificador TYPE zfit0045-identificador,
    sgtxt         TYPE zfit0045-sgtxt,
    belnr         TYPE zfit0045-belnr,
    adto_ins      TYPE zfit0045-adto_ins,
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
    lifnr TYPE ekko-lifnr,
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


  BEGIN OF ty_estra ,
    bukrs      TYPE zadt_sol_aprov-bukrs,
    nro_sol    TYPE zadt_sol_aprov-nro_sol,
    valor_de   TYPE zadto_aprovador-valor_de,
    valor_ate  TYPE zadto_aprovador-valor_ate,
    aprovador  TYPE zadto_aprovador-aprovador,
    nivel      TYPE zadto_aprovador-nivel,
    estado(4),
    opcoes(4),
    data_atual TYPE zadto_aprovador-data_atual,
    hora_atual TYPE zadto_aprovador-hora_atual,
  END OF ty_estra,


  BEGIN OF ty_zadto_aprovador,
    bukrs     TYPE zadto_aprovador-bukrs,
    bukrs_ate TYPE zadto_aprovador-bukrs_ate,
    dep_resp  TYPE zadto_aprovador-dep_resp,
    waers     TYPE zadto_aprovador-waers,
    nivel     TYPE zadto_aprovador-nivel,
    aprovador TYPE zadto_aprovador-aprovador,
    valor_de  TYPE zadto_aprovador-valor_de,
    valor_ate TYPE zadto_aprovador-valor_ate,
  END OF ty_zadto_aprovador,

  BEGIN OF ty_zadt_sol_aprov,
    bukrs      TYPE zadt_sol_aprov-bukrs,
    nro_sol    TYPE zadt_sol_aprov-nro_sol,
    nivel      TYPE zadt_sol_aprov-nivel,
    aprovador  TYPE zadt_sol_aprov-aprovador,
    valor_de   TYPE zadt_sol_aprov-valor_de,
    valor_ate  TYPE zadt_sol_aprov-valor_ate,
    data_atual TYPE zadt_sol_aprov-data_atual,
    hora_atual TYPE zadt_sol_aprov-hora_atual,
    usuario    TYPE zadt_sol_aprov-usuario,
  END OF ty_zadt_sol_aprov,

  BEGIN OF ty_saida,
    checkbox(1),
    butxt            TYPE t001-butxt,
    nro_sol          TYPE zfit0045-nro_sol,
    icon3(4),
    icon1(4),
    icon2(4),
    ebeln            TYPE zfit0046-ebeln,
    name1_f          TYPE lfa1-name1,
    name1_p          TYPE lfa1-name1,
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
    identificador    TYPE zfit0045-identificador,
    sgtxt            TYPE zfit0045-sgtxt,
    cellcolor        TYPE lvc_t_scol,
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
  wa_cont      TYPE REF TO cl_gui_custom_container,
  wa_alv       TYPE REF TO cl_gui_alv_grid,
  wa_bdcdata   LIKE LINE OF ti_bdcdata,

  wa_zfit0045  TYPE ty_zfit0045,
  wa_zfit0046  TYPE ty_zfit0046,
  wa_ekko      TYPE ty_ekko,
  wa_ekpo      TYPE ty_ekpo,
  wa_lfa1      TYPE ty_lfa1,
  wa_t001      TYPE ty_t001,
  wa_saida     TYPE ty_saida,
  wa_cellcolor TYPE lvc_s_scol.


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

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA:
  grid2               TYPE REF TO cl_gui_alv_grid,
  obg_conteiner_estra TYPE REF TO cl_gui_custom_container,
  g_cc_estra          TYPE scrfname VALUE 'CC_ESTRA'.

** Criação de tabela dinamica
DATA: t_fieldcatalog     TYPE lvc_t_fcat,
      w_fieldcatalog     TYPE lvc_s_fcat,
      wa_estra           TYPE ty_estra,
      tg_estra           TYPE TABLE OF ty_estra,
      wa_zadto_aprovador TYPE ty_zadto_aprovador,
      wa_zadt_sol_aprov  TYPE ty_zadt_sol_aprov,
      it_zadto_aprovador TYPE TABLE OF ty_zadto_aprovador,
      it_zadt_sol_aprov  TYPE TABLE OF ty_zadt_sol_aprov,
      vvalor_ate         TYPE zadt_sol_aprov-valor_ate,
      vdep_resp(2),
      tela200_msg(60),
      t_lotes            TYPE TABLE OF zad_lotes_imp,
      w_lotes            TYPE          zad_lotes_imp,
      t_estra            TYPE TABLE OF zfi_estrategia_imp,
      w_estra            TYPE          zfi_estrategia_imp,
      t_docs             TYPE TABLE OF zad_docs_imp,
      w_docs             TYPE          zad_docs_imp,
      v_msg              TYPE char50,
      tg_selectedcell    TYPE lvc_t_cell,
      wg_selectedcell    TYPE lvc_s_cell.

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
*      ELSEIF E_COLUMN_ID = 'ICON2'.
*        CALL SCREEN 200 STARTING AT 20  1
*                 ENDING   AT 150 13.
        ELSEIF e_column_id = 'NRO_SOL'.

          CALL FUNCTION 'Z_FUC_EXIBIR_ZFI0025'
            EXPORTING
              i_nro_sol       = wa_saida-nro_sol
                    .
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
                   p_budat  FOR zfit0045-dt_pgto,
                   p_sol    FOR zfit0045-nro_sol,
                   p_depto  FOR zfit0045-dep_resp,
                   p_usu    FOR zfit0045-usnam NO INTERVALS NO-EXTENSION,
                   p_resp   FOR zfit0045-resp_neg NO INTERVALS NO-EXTENSION.

SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETER:       p_stat   TYPE c  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF BLOCK b2.


DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
      tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.
DATA: BEGIN OF tl_dep OCCURS 0,
        dep_resp TYPE zfit0045-dep_resp,
        text1    TYPE t012t-text1,
      END OF tl_dep.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_depto-low .

  REFRESH tl_dep.
  CLEAR tl_dep.
  SELECT dep_resp dep_resp_desc
    FROM zimp_cad_depto
    INTO TABLE tl_dep.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DEP_RESP'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_DEPTO-LOW'
      value_org       = 'S'
    TABLES
      value_tab       = tl_dep
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_depto-high.

  REFRESH tl_dep.
  CLEAR tl_dep.
  SELECT dep_resp dep_resp_desc
    FROM zimp_cad_depto
    INTO TABLE tl_dep.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'DEP_RESP'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = 'P_DEPTO-HIGH'
      value_org   = 'S'
    TABLES
      value_tab   = tl_dep
      return_tab  = tl_return_tab.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_resp-low.
  DATA: BEGIN OF tl_usr OCCURS 0,
          bname     TYPE v_usr_name-bname,
          name_text TYPE v_usr_name-name_text,
        END OF tl_usr.

  SELECT bname  name_text
     FROM  v_usr_name INTO TABLE tl_usr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BNAME'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_RESP'
      value_org       = 'S'
    TABLES
      value_tab       = tl_usr
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_usu-low.
  DATA: BEGIN OF tl_usr OCCURS 0,
          bname     TYPE v_usr_name-bname,
          name_text TYPE v_usr_name-name_text,
        END OF tl_usr.

  SELECT bname  name_text
     FROM  v_usr_name INTO TABLE tl_usr.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BNAME'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'P_RESP'
      value_org       = 'S'
    TABLES
      value_tab       = tl_usr
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

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

  SELECT  bukrs nro_sol ebeln lifnr dt_pgto moeda_pgto motivo dep_resp resp_neg usnam status zlsch bvtyp hbkid  identificador sgtxt belnr adto_ins
    FROM zfit0045
    INTO TABLE it_zfit0045
    WHERE bukrs IN p_bukrs
    AND   usnam IN p_usu
    AND   resp_neg IN p_resp
    AND   dep_resp IN p_depto
    AND   dt_pgto IN p_budat
    AND   nro_sol IN p_sol
    AND   loekz = ''.

  IF p_stat = 'X'.
    DELETE it_zfit0045 WHERE status NE 'A'.
  ELSE.
    DELETE it_zfit0045 WHERE status EQ 'A'.
  ENDIF.
  CHECK it_zfit0045[] IS NOT INITIAL.

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

  SELECT ebeln frggr frgke lifnr
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
    wa_saida-belnr              = wa_zfit0045-belnr.

    wa_saida-zlsch              = wa_zfit0045-zlsch.
    wa_saida-bvtyp              = wa_zfit0045-bvtyp.
    wa_saida-hbkid              = wa_zfit0045-hbkid.
    wa_saida-identificador      = wa_zfit0045-identificador.
    wa_saida-sgtxt              = wa_zfit0045-sgtxt.

    READ TABLE it_ekko INTO wa_ekko WITH KEY ebeln = wa_zfit0045-ebeln BINARY SEARCH.
    IF ( wa_ekko-frgke = '2' ) OR ( wa_ekko-frggr IS INITIAL ). "Se estiver liberado ou não estiver em uma estratégia de aprovação OK.
      wa_saida-icon1   = icon_okay.
    ELSE.
      wa_saida-icon1   = icon_led_red.
    ENDIF.
    IF ( wa_zfit0045-adto_ins = 'X' ).
      wa_saida-icon3   = icon_locked.
    ELSE.
      wa_saida-icon3   = icon_checked.
    ENDIF.
    "
    SELECT SINGLE *
      FROM lfa1
      INTO CORRESPONDING FIELDS OF wa_lfa1
      WHERE lifnr = wa_ekko-lifnr.
    CONCATENATE wa_lfa1-lifnr '-' wa_lfa1-name1 INTO wa_saida-name1_p.
    IF wa_saida-name1_p NE wa_saida-name1_f.
      wa_cellcolor-fname = 'NAME1_F'.
      wa_cellcolor-color-col = '6'.
      wa_cellcolor-color-int = '1'.
      APPEND wa_cellcolor TO wa_saida-cellcolor.
      wa_cellcolor-fname = 'NAME1_P'.
      APPEND wa_cellcolor TO wa_saida-cellcolor.
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
  wa_layout-grid_title = 'Adiantamentos'.
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
  wa_afield-hotspot       = 'X'.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'ICON3'.
  wa_afield-icon          = 'X'.
  wa_afield-scrtext_s = 'Lib.Insumos'.
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
  "WA_AFIELD-HOTSPOT       = 'X'.
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
  wa_afield-fieldname     = 'NAME1_P'.
  wa_afield-scrtext_s = 'Fornecedor Pedido'.
  wa_afield-scrtext_l = 'Fornecedor Pedido'.
  wa_afield-scrtext_m = 'Fornecedor Pedido'.
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
    wa_layout-ctab_fname = 'CELLCOLOR'.
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

  IF p_depto    IS NOT INITIAL.
    IF p_depto-high IS INITIAL.
      CONCATENATE 'Departamento  :' p_depto-low
        INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Departamento :' p_depto-low  INTO wl_linha SEPARATED BY space.
      CONCATENATE wl_linha 'à' p_depto-high  INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

  IF p_usu IS NOT INITIAL.

    CONCATENATE 'Usuário Solicitante  :' p_usu-low
     INTO wl_linha SEPARATED BY space.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.
  IF p_resp IS NOT INITIAL.

    CONCATENATE 'Negociador  :' p_resp-low
     INTO wl_linha SEPARATED BY space.

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
        vmsg(50),
        tl_rows     TYPE lvc_t_row,
        sl_rows     TYPE lvc_s_row.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.
    WHEN '&LOG'.

      DATA: BEGIN OF itab OCCURS 0,
              name(80) TYPE c,
            END OF itab.
      DATA: vdata(10),vhora(10), msg_sol TYPE char80.


      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.


      IF tl_rows[] IS INITIAL.
        MESSAGE  'Selecione uma Linha.' TYPE 'I'.
        EXIT.
      ENDIF.
      READ TABLE: tl_rows    INTO sl_rows   INDEX 1,
                  it_saida   INTO wa_saida  INDEX sl_rows-index.
*  "Estratégia salva
      REFRESH: itab, it_zadt_sol_aprov.
      SELECT bukrs nro_sol nivel aprovador valor_de valor_ate data_atual hora_atual usuario
        FROM zadt_sol_aprov
        INTO TABLE it_zadt_sol_aprov
        WHERE  nro_sol  =  wa_saida-nro_sol.

*    ITAB-NAME    = 'NIVEL|APROVADOR    |DATA       |HORA'.
      itab-name+00(05) = TEXT-p02.
      itab-name+05(01) = '|'.
      itab-name+06(12) = TEXT-p03.
      itab-name+19(01) = '|'.
      itab-name+20(10) = TEXT-p04.
      itab-name+31(01) = '|'.
      itab-name+32(10) = TEXT-p05.

      APPEND itab .
      CLEAR itab.
      LOOP AT it_zadt_sol_aprov INTO wa_zadt_sol_aprov.
        CONCATENATE wa_zadt_sol_aprov-hora_atual+0(2) wa_zadt_sol_aprov-hora_atual+2(2) wa_zadt_sol_aprov-hora_atual+4(2) INTO vhora SEPARATED BY ':'.
        CONCATENATE wa_zadt_sol_aprov-data_atual+6(2) wa_zadt_sol_aprov-data_atual+4(2) wa_zadt_sol_aprov-data_atual+0(4) INTO vdata SEPARATED BY '.'.

        itab-name+00(04) = wa_zadt_sol_aprov-nivel.
        itab-name+05(01) = '|'.
        itab-name+06(12) = wa_zadt_sol_aprov-aprovador.
        itab-name+19(01) = '|'.
        itab-name+20(10) = vdata.
        itab-name+31(01) = '|'.
        itab-name+32(10) = vhora.
        APPEND itab .
        CLEAR itab.
      ENDLOOP.
      CONCATENATE 'Solicitação ' wa_saida-nro_sol INTO msg_sol SEPARATED BY space.
      CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
        EXPORTING
          endpos_col   = 140
          endpos_row   = 20
          startpos_col = 60
          startpos_row = 15
          titletext    = msg_sol
        TABLES
          valuetab     = itab
        EXCEPTIONS
          break_off    = 1
          OTHERS       = 2.
    WHEN '&EST'.
      CALL METHOD cl_grid->get_selected_rows
        IMPORTING
          et_index_rows = tl_rows.


      IF tl_rows[] IS INITIAL.
        MESSAGE  'Selecione uma Linha.' TYPE 'I'.
        EXIT.
      ENDIF.
      READ TABLE: tl_rows    INTO sl_rows   INDEX 1,
                  it_saida   INTO wa_saida  INDEX sl_rows-index.

      REFRESH: t_lotes,t_estra,t_docs.
      CALL FUNCTION 'Z_AD_ESTRATEGIA_LISTA'
        EXPORTING
          v_usuario = sy-uname
          v_nro_sol = wa_saida-nro_sol
        IMPORTING
          msg       = v_msg
        TABLES
          t_lotes   = t_lotes
          t_estra   = t_estra
          t_docs    = t_docs.
      REFRESH:  tg_estra.
      LOOP AT t_estra INTO w_estra.
        MOVE-CORRESPONDING w_estra TO wa_estra.
        wa_estra-nro_sol = w_estra-lote.
        APPEND wa_estra TO tg_estra.
      ENDLOOP.
      .
      CALL SCREEN 200 STARTING AT 20  1
                      ENDING   AT 150 13.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  "DATA: FCODE TYPE TABLE OF SY-UCOMM.

  REFRESH: fcode.
  APPEND c_save TO fcode.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0200'.


ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  "GRID2
  IF obg_conteiner_estra IS INITIAL.
    CREATE OBJECT obg_conteiner_estra
      EXPORTING
        container_name = g_cc_estra.


    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_estra.


    PERFORM montar_layout_estra.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Estratégia de Liberação'.
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_estra.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

  ELSE.
    PERFORM montar_layout_estra.
    CALL METHOD grid2->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_estra .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZADTO_APROVADOR'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         'Valor de'      '15' ' ' ' ' ' ',
        1 'ZADTO_APROVADOR'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        'Valor ate'     '15' ' ' ' ' ' ',
        1 'ZADTO_APROVADOR'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        'Aprovador'     '20' ' ' ' ' ' '.
*        1 ' '                         ' '               'TG_ESTRA' 'ESTADO'           'Estado'        '10' ' ' ' ' ' ',
*        1 ' '                         ' '               'TG_ESTRA' 'OPCOES'           'Opções Liber.' '12' ' ' ' ' ' ',
*        1 ' '                         ' '               'TG_ESTRA' 'DATA_ATUAL'       'Data Aprov'    '15' ' ' ' ' ' ',
*        1 ' '                         ' '               'TG_ESTRA' 'HORA_ATUAL'       'Hora'          '12' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE carrega_lotes OUTPUT.

  DATA: vflg_ico(1),
         wl_data TYPE sy-datum.

  CALL FUNCTION 'ADD_TIME_TO_DATE'
    EXPORTING
      i_idate               = wa_saida-dt_pgto
      i_time                = -3
      i_iprkz               = ''
    IMPORTING
      o_idate               = wl_data
    EXCEPTIONS
      invalid_period        = 1
      invalid_round_up_rule = 2
      internal_error        = 3
      OTHERS                = 4.

  CLEAR tela200_msg.
  IF wl_data LT sy-datum.
    tela200_msg = 'Pedido fora do prazo para aprovação'.
  ENDIF.

*  SELECT  BUKRS BUKRS_ATE DEP_RESP WAERS NIVEL APROVADOR VALOR_DE VALOR_ATE
*  FROM ZADTO_APROVADOR
*  INTO TABLE IT_ZADTO_APROVADOR
*  WHERE BUKRS     LE WA_SAIDA-BUTXT+0(4)
*  AND   BUKRS_ATE GE WA_SAIDA-BUTXT+0(4).
*
*  SORT IT_ZADTO_APROVADOR BY BUKRS BUKRS_ATE DEP_RESP NIVEL.
*
*  "Estratégia salva
*  REFRESH IT_ZADT_SOL_APROV.
*  SELECT BUKRS NRO_SOL NIVEL APROVADOR VALOR_DE VALOR_ATE DATA_ATUAL HORA_ATUAL USUARIO
*    FROM ZADT_SOL_APROV
*    INTO TABLE IT_ZADT_SOL_APROV
*    WHERE  NRO_SOL  =  WA_SAIDA-NRO_SOL.
*
*  SORT IT_ZADT_SOL_APROV BY NIVEL APROVADOR.
*  VFLG_ICO = 'N'.
*  CLEAR VDEP_RESP.
*  VVALOR_ATE = 0.
*  LOOP AT IT_ZADTO_APROVADOR INTO WA_ZADTO_APROVADOR.
*    IF  WA_ZADTO_APROVADOR-BUKRS_ATE IS INITIAL.
*      IF  WA_ZADTO_APROVADOR-BUKRS NE WA_SAIDA-BUTXT+0(4).
*        CONTINUE.
*      ENDIF.
*    ELSEIF WA_ZADTO_APROVADOR-BUKRS     GT WA_SAIDA-BUTXT+0(4) OR
*           WA_ZADTO_APROVADOR-BUKRS_ATE LT WA_SAIDA-BUTXT+0(4).
*      CONTINUE.
*    ENDIF.
*    IF WA_SAIDA-DEP_RESP+0(2) = WA_ZADTO_APROVADOR-DEP_RESP.
*      IF WA_SAIDA-VLR_ADIANTAMENTO > VVALOR_ATE.
*        VVALOR_ATE = WA_ZADTO_APROVADOR-VALOR_ATE.
*        VDEP_RESP = WA_ZADTO_APROVADOR-DEP_RESP.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
*  IF VDEP_RESP IS INITIAL.
*    LOOP AT IT_ZADTO_APROVADOR INTO WA_ZADTO_APROVADOR.
*      IF  WA_ZADTO_APROVADOR-BUKRS_ATE IS INITIAL.
*        IF  WA_ZADTO_APROVADOR-BUKRS NE WA_SAIDA-BUTXT+0(4).
*          CONTINUE.
*        ENDIF.
*      ELSEIF WA_ZADTO_APROVADOR-BUKRS     GT WA_SAIDA-BUTXT+0(4) OR
*             WA_ZADTO_APROVADOR-BUKRS_ATE LT WA_SAIDA-BUTXT+0(4).
*        CONTINUE.
*      ENDIF.
*      IF WA_ZADTO_APROVADOR-DEP_RESP IS INITIAL.
*        IF WA_SAIDA-VLR_ADIANTAMENTO > VVALOR_ATE.
*          VVALOR_ATE = WA_ZADTO_APROVADOR-VALOR_ATE.
*          VDEP_RESP = WA_ZADTO_APROVADOR-DEP_RESP.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*  LOOP AT IT_ZADTO_APROVADOR INTO WA_ZADTO_APROVADOR .
*    IF  WA_ZADTO_APROVADOR-BUKRS_ATE IS INITIAL.
*      IF  WA_ZADTO_APROVADOR-BUKRS NE WA_SAIDA-BUTXT+0(4).
*        CONTINUE.
*      ENDIF.
*    ELSEIF WA_ZADTO_APROVADOR-BUKRS     GT WA_SAIDA-BUTXT+0(4) OR
*           WA_ZADTO_APROVADOR-BUKRS_ATE LT WA_SAIDA-BUTXT+0(4).
*      CONTINUE.
*    ENDIF.
*    IF WA_ZADTO_APROVADOR-VALOR_ATE <= VVALOR_ATE.
*      WA_ESTRA-BUKRS        = WA_SAIDA-BUTXT+0(4).
*      WA_ESTRA-NRO_SOL      = WA_SAIDA-NRO_SOL.
*      WA_ESTRA-VALOR_DE     = WA_ZADTO_APROVADOR-VALOR_DE.
*      WA_ESTRA-VALOR_ATE    = WA_ZADTO_APROVADOR-VALOR_ATE.
*      WA_ESTRA-APROVADOR    = WA_ZADTO_APROVADOR-APROVADOR.
*      WA_ESTRA-NIVEL        = WA_ZADTO_APROVADOR-NIVEL.
*
*
*      READ TABLE IT_ZADT_SOL_APROV INTO WA_ZADT_SOL_APROV WITH KEY NIVEL     = WA_ZADTO_APROVADOR-NIVEL
*                                                                   APROVADOR = WA_ZADTO_APROVADOR-APROVADOR BINARY SEARCH.
*      IF SY-SUBRC = 0.
*        WA_ESTRA-ESTADO       = ICON_CHECKED .
*        WA_ESTRA-OPCOES       = ICON_SYSTEM_UNDO .
*        WA_ESTRA-DATA_ATUAL   = WA_ZADT_SOL_APROV-DATA_ATUAL.
*        WA_ESTRA-HORA_ATUAL   = WA_ZADT_SOL_APROV-HORA_ATUAL.
*        VFLG_ICO = 'N'.
*      ELSEIF VFLG_ICO = 'S'.
*        WA_ESTRA-ESTADO       = ICON_LED_YELLOW .
*        WA_ESTRA-OPCOES       = '' .
*      ELSE.
*        IF SY-UNAME NE WA_ZADTO_APROVADOR-APROVADOR.
*          WA_ESTRA-ESTADO       =  ' '.
*          WA_ESTRA-OPCOES       = ICON_LED_YELLOW  .
*        ELSE.
*          WA_ESTRA-ESTADO       = ICON_LED_YELLOW .
*          WA_ESTRA-OPCOES       = ICON_SET_STATE  .
*        ENDIF.
*        VFLG_ICO = 'X'.
*      ENDIF.
*
*      IF VDEP_RESP IS INITIAL.
*        IF VFLG_ICO = 'X'.
*          VFLG_ICO = 'S'.
*        ENDIF.
*        APPEND WA_ESTRA TO TG_ESTRA.
*      ELSEIF VDEP_RESP = WA_ZADTO_APROVADOR-DEP_RESP.
*        IF VFLG_ICO = 'X'.
*          VFLG_ICO = 'S'.
*        ENDIF.
*        APPEND WA_ESTRA TO TG_ESTRA.
*      ENDIF.
*    ENDIF.
*  ENDLOOP.
ENDMODULE.                 " CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE ok-code.
    WHEN 'SAIR'.
      SET SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0200  INPUT
