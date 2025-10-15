*&---------------------------------------------------------------------*
*& Report  ZPMR0080
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zpmr0080.

TABLES: imrg, equi, equz.

TYPE-POOLS: slis, kkblo.

TYPES: BEGIN OF ty_saida,
         iwerk      TYPE v_equi-iwerk,
         equnr      TYPE v_equi-equnr,
         eqart      TYPE v_equi-eqart,
         categ      TYPE t370k_t-eartx,
         typbz      TYPE v_equi-typbz,
         herst      TYPE v_equi-herst,
         baujj      TYPE v_equi-baujj,
         cdiff      TYPE imrc_totac,
         dt_ini     TYPE imrg-idate,
         t_ini      TYPE imrg-itime,
         dt_fim     TYPE imrg-idate,
         t_fim      TYPE imrg-itime,
         recdu      TYPE imrg-recdu,
         pos_ini    TYPE imrc_totac,
         pos_fim    TYPE imrc_totac,
         pos_inic   TYPE imrc_totac,
         pos_final  TYPE imrc_totac,
         mrngu      TYPE imptt-mrngu,
         total      TYPE p DECIMALS 2,
         media      TYPE p DECIMALS 2,
         consumo    TYPE c LENGTH 8,
         datab      TYPE v_equi-datab,
         datbi      TYPE v_equi-datbi,
         timbi      TYPE v_equi-timbi,
         cell_color TYPE lvc_t_scol,    " Cor da Célula
       END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
       TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_inconsistencias,
         iwerk  TYPE v_equi-iwerk,
         iwerk2 TYPE v_equi-swerk,
         equnr  TYPE v_equi-equnr,
         categ  TYPE t370k_t-eartx,
         typbz  TYPE v_equi-typbz,
         herst  TYPE v_equi-herst,
         baujj  TYPE v_equi-baujj,
         idate  TYPE char10,
         itime  TYPE char10,
         l_msg  TYPE c LENGTH 255.
TYPES: END OF ty_inconsistencias.
*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA: t_saida  TYPE TABLE OF ty_saida,
      ws_saida TYPE ty_saida,
      t_incons TYPE TABLE OF ty_inconsistencias.

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
  wa_saida  TYPE ty_saida,
  wg_erro   TYPE c,
  wa_incons TYPE ty_inconsistencias.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events           TYPE          slis_alv_event,
      events              TYPE          slis_t_event,
      t_print             TYPE          slis_print_alv,
      estrutura           TYPE TABLE OF ty_estrutura,
      wa_estrutura        TYPE          ty_estrutura,
      estrutura_incons    TYPE TABLE OF ty_estrutura,
      wa_estrutura_incons TYPE          ty_estrutura,
      v_report            LIKE          sy-repid,
      t_top               TYPE          slis_t_listheader,
      t_sort              TYPE          slis_t_sortinfo_alv,
      wa_sort             TYPE          slis_sortinfo_alv,
      wa_color            TYPE          lvc_s_scol,  " Cor para célula
      it_color            TYPE TABLE OF lvc_s_scol,  " Cor para célula
      init.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_equnr  FOR  equi-equnr,
                s_docdat FOR imrg-idate NO-EXTENSION OBLIGATORY,
                s_typbz  FOR equi-typbz,
*                s_statio FOR wa_t370-station,
                s_eqart  FOR equi-eqart,
                s_iwerk  FOR equz-iwerk OBLIGATORY.

PARAMETERS:     p_inatel AS CHECKBOX.
SELECTION-SCREEN: END OF BLOCK a1.

SELECTION-SCREEN: BEGIN OF BLOCK a2 WITH FRAME TITLE text-002.
PARAMETERS: p_analit RADIOBUTTON GROUP b1,
            p_consol RADIOBUTTON GROUP b1,
            p_segreg RADIOBUTTON GROUP b1.
SELECTION-SCREEN: END OF BLOCK a2.



*&---------------------------------------------------------------------*
*& INITIALIZATION.
*&*&---------------------------------------------------------------------*

INITIALIZATION.

*  SELECT *
*    FROM tvarvc
*    INTO TABLE @DATA(t_tvarvc)
*    WHERE name = 'Z_FILIAL_ZPM0014'.
*  IF sy-subrc IS INITIAL.
*    LOOP AT t_tvarvc INTO DATA(ls_tvarv).
*      s_iwerk-sign = 'I'.
*      s_iwerk-option = 'EQ'.
*      s_iwerk-low = ls_tvarv-low.
*      APPEND s_iwerk.
*    ENDLOOP.
*  ENDIF.
*
*  s_docdat-low = sy-datum - 30.
*  s_docdat-high = sy-datum .
*  APPEND s_docdat.


START-OF-SELECTION.

  "Para Execução em backgound (jobs) """"""""""""""""""""""""""""
  IF sy-batch EQ abap_true.
    TRY .
        zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd = DATA(e_qtd) ).
      CATCH zcx_job.
        e_qtd = 1.
    ENDTRY.

    IF e_qtd GT 1.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

*-------------------------------------------
* execute ZPM0014
*-------------------------------------------

  PERFORM: submit_zpm0014.
*&---------------------------------------------------------------------*
*&      Form  SUBMIT_ZPM0014
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM submit_zpm0014 .

  DATA: wa_zpmt0067 TYPE zpmt0067.
  DATA: t_zpmt0067 TYPE TABLE OF zpmt0067,
        vg_init    TYPE sy-datum.

  DELETE FROM zpmt0067.
  COMMIT WORK.

  PERFORM f_prepare_run_time_info USING abap_false.

  "Passar data ultimos 30 dias.
  IF s_docdat-high IS NOT INITIAL.
    vg_init = s_docdat-high - 30.
    LOOP AT s_docdat ASSIGNING FIELD-SYMBOL(<ls_date>).
      <ls_date>-low = vg_init.
      <ls_date>-high = sy-datum.
    ENDLOOP.
  ENDIF.

  SUBMIT zpmr0002 WITH s_equnr  IN s_equnr
                  WITH s_docdat IN s_docdat
                  WITH s_typbz  IN s_typbz
                  WITH s_eqart  IN s_eqart
                  WITH s_iwerk  IN s_iwerk
                  WITH p_inatel EQ p_inatel
                  WITH p_analit EQ p_analit
                  WITH p_consol EQ p_consol
                  WITH p_segreg EQ p_segreg
*                  WITH p_call   = abap_true
                  AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR ws_saida.
      MOVE-CORRESPONDING <w_data> TO ws_saida.
      APPEND ws_saida TO  t_saida.
    ENDLOOP.
  ENDIF.

  IF t_saida[] IS NOT INITIAL.

    SORT t_saida BY equnr dt_ini t_ini.

    LOOP AT t_saida INTO DATA(wa_saida).
      CONDENSE wa_saida-cdiff NO-GAPS.
      wa_zpmt0067-equipamentos    = wa_saida-equnr.
      wa_zpmt0067-modelo_equip    = wa_saida-typbz.
      wa_zpmt0067-tipo_de_veiculo = wa_saida-eqart.
      wa_zpmt0067-filial          = wa_saida-iwerk.
      wa_zpmt0067-fabricante      = wa_saida-herst.
      wa_zpmt0067-ano             = wa_saida-baujj.
      wa_zpmt0067-consumo         = wa_saida-cdiff.
      wa_zpmt0067-data_inicio     = wa_saida-dt_ini.
      wa_zpmt0067-hora_inicio     = wa_saida-t_ini.
      wa_zpmt0067-odohorinicio    = wa_saida-pos_ini.
      wa_zpmt0067-data_fim        = wa_saida-dt_fim.
      wa_zpmt0067-hora_fim        = wa_saida-t_fim.
      wa_zpmt0067-mrngu           = wa_saida-mrngu.
      wa_zpmt0067-total           = wa_saida-total.
      wa_zpmt0067-media           = wa_saida-media.
      wa_zpmt0067-odohorfim       = wa_saida-pos_fim.
      APPEND wa_zpmt0067 TO t_zpmt0067.
      CLEAR: wa_zpmt0067.

    ENDLOOP.

    MODIFY zpmt0067 FROM TABLE t_zpmt0067.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_RUNTIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_runtime_info .
  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
      IMPORTING r_data_descr  = l_data_descr
                r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data  = <t_data>
                                                   t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RUN_TIME_INFO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ABAP_FALSE  text
*----------------------------------------------------------------------*
FORM f_prepare_run_time_info USING p_display TYPE c.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR: <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR: <t_data_line>.
  ENDIF.

  FREE: l_data,  l_data_line, l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = p_display
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.
