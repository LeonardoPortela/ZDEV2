************************************************************************
*              ******************************************              *
*              *                 AMAGGI                 *              *
*              *       CONFIDENCIAL E PROPRIETÁRIO      *              *
*              *      TODOS OS DIREITOS RESERVADOS      *              *
*              ******************************************              *
************************************************************************
* Projeto       : Projeto Controle Ferramentaria                       *
* Objetivo      : Relatório Gerencial Emprestimo                       *
* Analista      : Alexandre Suzan WAGON                                *
* Desenvolvedor : Alexandre Suzan WAGON                                *
* Data          : 02/12/2020                                           *
* Transação     : ZPM0068                                              *
* Observação    : Relatório Gerencial Emprestimo                       *
*----------------------------------------------------------------------*
REPORT zpmr0068 MESSAGE-ID z_mm NO STANDARD PAGE HEADING.

TABLES: mkpf,
        qmel,
        zpmt0039,
        zpmt0065,
        mara,
        equi,
        pa0001.
*----------------------------------------------------------------------*
* Tipos
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_reg,
         w_conteudo TYPE max_segm,
       END   OF ty_reg,

       BEGIN OF yg_alv,
         icon(4)     TYPE c,
         qmnum       TYPE zpmt0040-qmnum,
         mat_doc     TYPE zpmt0040-mat_doc,
         doc_year    TYPE zpmt0040-doc_year,
         bldat       TYPE bldat,
         data_cri    TYPE sy-datum,
         pernr       TYPE pa0001-pernr,
         name1(40)   TYPE c,
         caixa       TYPE dd07v-ddtext,
         werks       TYPE werks_d,                       " Centro
         matnr       TYPE matnr,                         " Material
         descr(40)   TYPE c,
         equnr       TYPE eqkt-equnr,
         eqktx       TYPE eqkt-eqktx,
         qtd_disp    TYPE c LENGTH 16,                   " Quantidade disponível
         labst_ini   TYPE c LENGTH 16,                   " Quantidade inicial
         labst_emp   TYPE c LENGTH 16,                   " Quantidade emprestado
         labst       TYPE c LENGTH 16,                   " Quantidade
         labst_dev   TYPE c LENGTH 16,                   " Quantidade devolvida
         labst_atual TYPE c LENGTH 16,                   " Quantidade atual
         proces      TYPE c LENGTH 01,                        " Processado
         obs(50)     TYPE c,
       END OF yg_alv.

*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
DATA: tg_alv      TYPE TABLE OF yg_alv,
      tg_fieldcat TYPE STANDARD TABLE OF lvc_s_fcat.

DATA: tg_zpmt0040 TYPE TABLE OF zpmt0040.

*----------------------------------------------------------------------*
* Estruturas
*----------------------------------------------------------------------*
DATA: eg_layout     TYPE lvc_s_layo.
DATA: eg_alv TYPE yg_alv.

DATA: eg_zpmt0040 TYPE zpmt0040.

*----------------------------------------------------------------------*
* Variáveis
*----------------------------------------------------------------------*
DATA:  vg_code TYPE sy-ucomm.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS:
  cg_erro_id TYPE c LENGTH 08 VALUE '@S_TL_R@',
  cg_suce_id TYPE c LENGTH 08 VALUE '@S_TL_G@'.

*local class to handle semantic checks
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: event TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS on_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING
        !e_row_id
        !es_row_no .

    CLASS-METHODS:
      handle_toolbar
                  FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command
                  FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD  on_hotspot_click.

    READ TABLE tg_alv INTO eg_alv INDEX e_row_id-index.
    IF sy-subrc = 0.
      SET PARAMETER ID 'IQM' FIELD eg_alv-qmnum.
      CALL TRANSACTION 'IW23' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.                    "handle_double_clickENDCLASS.

  METHOD handle_toolbar.
    PERFORM zf_handle_toolbar USING e_object.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM zf_handle_user_command USING e_ucomm.
  ENDMETHOD.
ENDCLASS.
*----------------------------------------------------------------------*
* ALV
*----------------------------------------------------------------------*
DATA: og_alv   TYPE REF TO cl_gui_alv_grid,
      o_cont   TYPE REF TO cl_gui_docking_container,
      o_parent TYPE REF TO cl_gui_container.

*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-t43.
PARAMETERS p_emp TYPE c RADIOBUTTON GROUP sel USER-COMMAND upd.
PARAMETERS p_dev TYPE c RADIOBUTTON GROUP sel.
PARAMETERS p_est TYPE c RADIOBUTTON GROUP sel.
PARAMETERS p_des TYPE c RADIOBUTTON GROUP sel.
PARAMETERS p_dis TYPE c RADIOBUTTON GROUP sel.
SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_werks FOR zpmt0039-werks,
                s_pernr FOR pa0001-pernr NO-EXTENSION NO INTERVALS,
                s_qmnum FOR qmel-qmnum NO-EXTENSION NO INTERVALS,
                s_matnr FOR mara-matnr NO-EXTENSION NO INTERVALS,
                s_equnr FOR equi-equnr NO-EXTENSION NO INTERVALS,
                s_budat FOR mkpf-budat,
                s_caixa FOR zpmt0065-caixa NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_qmnum-low.

  PERFORM qmnum_f4 CHANGING s_qmnum-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_pernr-low.

  PERFORM pernr_f4 CHANGING s_pernr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_matnr-low.

  PERFORM matnr_f4 CHANGING s_matnr-low.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_caixa-low.

  PERFORM caixa_f4 CHANGING s_caixa-low.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name CS 'S_CAIXA' AND p_dis IS NOT INITIAL.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
  IF p_emp IS INITIAL AND
     p_dev IS INITIAL AND
     p_est IS INITIAL AND
     p_des IS INITIAL AND
     p_dis IS INITIAL.
    MESSAGE s000(z_mm) WITH 'Flegar pelo menos um Status do Registro.' DISPLAY LIKE 'E'.
  ELSE.
    PERFORM: zf_seleciona_dados.
    PERFORM: zf_exibe_alv.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  zf_exibe_alv
*&---------------------------------------------------------------------*
FORM zf_exibe_alv.
  CALL SCREEN 0100.
ENDFORM.                    "zf_exibe_alv

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA tl_exclude TYPE ui_functions.                        "#EC *

  DATA el_variant TYPE disvariant.                          "#EC *

  SET PF-STATUS 'ZALV_STANDARD'.

  IF NOT o_cont IS INITIAL.
    CALL METHOD og_alv->refresh_table_display
      EXCEPTIONS
        finished = 1
        OTHERS   = 2.
  ELSE.
    CREATE OBJECT o_cont
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_top
        repid     = sy-repid
        dynnr     = '0100'
        extension = 1000
      EXCEPTIONS
        OTHERS    = 6.

    o_parent = o_cont.

    CREATE OBJECT og_alv
      EXPORTING
        i_parent = o_parent.

    PERFORM: zf_monta_fieldcat,
             zf_layout,
             zf_exclude_tb_functions TABLES tl_exclude.

    el_variant-report   = sy-repid.
    el_variant-username = sy-uname.

    eg_layout-cwidth_opt = 'X'.
    eg_layout-no_rowmark = 'X'.
    CREATE OBJECT event.
    SET HANDLER event->on_hotspot_click FOR og_alv.

    SET HANDLER event->handle_toolbar FOR ALL INSTANCES.
    SET HANDLER event->handle_user_command FOR og_alv.

    CALL METHOD og_alv->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_exclude
        is_variant           = el_variant
        is_layout            = eg_layout
        i_save               = 'A'
      CHANGING
        it_outtab            = tg_alv
        it_fieldcatalog      = tg_fieldcat.

    CALL METHOD og_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  zf_monta_fieldcat
*&---------------------------------------------------------------------*
FORM zf_monta_fieldcat.

  DATA el_fieldcat TYPE lvc_s_fcat.

  IF p_dis IS INITIAL.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'PROCES'.
    el_fieldcat-tabname   = 'TG_ALV'.
    el_fieldcat-coltext   = 'Selecionar'.
    el_fieldcat-checkbox  = abap_true.
    el_fieldcat-edit      = abap_true.
    el_fieldcat-outputlen = '10'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'ICON'.
    el_fieldcat-tabname   = 'TG_ALV'.
    el_fieldcat-coltext   = 'Status'.
    el_fieldcat-outputlen = '4'.
    el_fieldcat-icon      = 'X'.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    APPEND VALUE #(
    fieldname = 'WERKS'
    coltext   = 'Centro'
    outputlen = '08'
    hotspot   = ''
    key       = abap_true
    ) TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'QMNUM'.
    el_fieldcat-coltext   = 'No.Nota'.
    el_fieldcat-outputlen = '12'.
    el_fieldcat-hotspot   = 'X'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'BLDAT'.
    el_fieldcat-coltext   = 'Dt.Emprest.'.
    el_fieldcat-outputlen = '10'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'PERNR'.
    el_fieldcat-coltext   = 'No.Pessoal'.
    el_fieldcat-outputlen = '12'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'NAME1'.
    el_fieldcat-coltext   = 'Desc.No.Pessoal'.
    el_fieldcat-outputlen = '30'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

*** Inicio - Rubenilson Pereira - 13.10.22 - US 77010
    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'CAIXA'.
    el_fieldcat-coltext   = 'Caixa de Ferramenta'.
    el_fieldcat-outputlen = '5'.
    el_fieldcat-key       = abap_true.
    APPEND el_fieldcat TO tg_fieldcat.

  ENDIF.
*** Fim - Rubenilson Pereira - 13.10.22 - US 77010

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'MATNR'.
  el_fieldcat-coltext   = 'Material'.
  el_fieldcat-outputlen = '15'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'DESCR'.
  el_fieldcat-coltext   = 'Descrição'.
  el_fieldcat-outputlen = '30'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
    APPEND VALUE #(
    fieldname = 'WERKS'
    coltext   = 'Centro'
    outputlen = '08'
    hotspot   = ''
    key       = abap_true
    ) TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'EQUNR'.
  el_fieldcat-coltext   = 'Equipamento'.
  el_fieldcat-outputlen = '30'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'EQKTX'.
  el_fieldcat-coltext   = 'Descrição do equipamento'.
  el_fieldcat-outputlen = '30'.
  el_fieldcat-key       = abap_true.
  APPEND el_fieldcat TO tg_fieldcat.

*  CLEAR el_fieldcat.
*  el_fieldcat-fieldname = 'LABST_INI'.
*  el_fieldcat-ref_table = 'MARD'.
*  el_fieldcat-ref_field = 'LABST'.
*  el_fieldcat-coltext   = 'Estoque Inicial'.
*  el_fieldcat-outputlen = '8'.
*  APPEND el_fieldcat TO tg_fieldcat.

  IF p_emp IS NOT INITIAL.
    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'LABST_EMP'.
    el_fieldcat-ref_table = 'MARD'.
    el_fieldcat-ref_field = 'LABST'.
    el_fieldcat-coltext   = 'Qtde Emprestada'.
    el_fieldcat-outputlen = '8'.
    APPEND el_fieldcat TO tg_fieldcat.
  ENDIF.

  IF p_dis IS NOT INITIAL.
    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'QTD_DISP'.
    el_fieldcat-ref_table = 'MARD'.
    el_fieldcat-ref_field = 'LABST'.
    el_fieldcat-coltext   = 'Qtde Disponível'.
    el_fieldcat-outputlen = '8'.
    APPEND el_fieldcat TO tg_fieldcat.
  ENDIF.

  IF p_dis IS INITIAL.

    CLEAR el_fieldcat.
    el_fieldcat-fieldname = 'LABST_DEV'.
    el_fieldcat-ref_table = 'MARD'.
    el_fieldcat-ref_field = 'LABST'.
    IF p_est IS NOT INITIAL.
      el_fieldcat-coltext   = 'Qtde Estorn.'.
    ELSEIF p_des IS NOT INITIAL.
      el_fieldcat-coltext   = 'Qtde Descart.'.
    ELSEIF p_dev  IS NOT INITIAL.
      el_fieldcat-coltext   = 'Qtde Devolv.'.
    ENDIF.

*  el_fieldcat-edit      = abap_true.
    el_fieldcat-outputlen = '15'.
    APPEND el_fieldcat TO tg_fieldcat.

  ENDIF.

  CLEAR el_fieldcat.
  el_fieldcat-fieldname = 'DATA_CRI'.
  el_fieldcat-tabname   = 'TG_ALV'.
  el_fieldcat-coltext   = 'Data'.
  el_fieldcat-outputlen = '10'.
  APPEND el_fieldcat TO tg_fieldcat.
*
*  CLEAR el_fieldcat.
*  el_fieldcat-fieldname = 'LABST_ATUAL'.
*  el_fieldcat-ref_table = 'MARD'.
*  el_fieldcat-ref_field = 'LABST'.
*  el_fieldcat-coltext   = 'Estoque Atual'.
*  el_fieldcat-outputlen = '8'.
*  APPEND el_fieldcat TO tg_fieldcat.

ENDFORM.                    "zf_monta_fieldcat
*&---------------------------------------------------------------------*
*&      Form  zf_layout
*&---------------------------------------------------------------------*
FORM zf_layout.
  eg_layout-zebra = abap_true.
ENDFORM.                    " zf_layout

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CLEAR vg_code.
  vg_code = sy-ucomm.
  CASE vg_code.
    WHEN 'BACK' OR 'LEAVE'.
      CLEAR vg_code.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      CLEAR vg_code.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0100  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVA_LOG
*&---------------------------------------------------------------------*
FORM zf_seleciona_dados.

  TYPES: BEGIN OF ty_0041,
           werks        TYPE zpmt0041-werks,
           matnr        TYPE zpmt0041-matnr,
           pernr        TYPE zpmt0041-pernr,
           qmnum        TYPE zpmt0041-qmnum,
           qmnum_emp    TYPE zpmt0041-qmnum,
           mat_doc_emp  TYPE zpmt0041-mat_doc,
           doc_year_emp TYPE zpmt0041-doc_year,
           labst        TYPE zpmt0041-labst,
           estorno      TYPE zpmt0041-estorno,
           descarte     TYPE zpmt0041-descarte,
         END OF ty_0041.

  TYPES: BEGIN OF ty_sum,
           werks TYPE zpmt0039-werks,
           matnr TYPE zpmt0039-matnr,
           labst TYPE zpmt0039-labst,
         END OF ty_sum.

  TYPES: BEGIN OF ty_maktx,
           matnr TYPE mara-matnr,
           maktx TYPE makt-maktx,
         END OF ty_maktx.

  TYPES: BEGIN OF ty_pa0001,
           pernr TYPE pa0001-pernr,
           ename TYPE pa0001-ename,
         END OF ty_pa0001.

  DATA: lt_pa0001 TYPE TABLE OF ty_pa0001,
        ls_pa0001 TYPE ty_pa0001.

  DATA: lt_estoque_inicial TYPE TABLE OF ty_sum,
        ls_estoque_inicial TYPE ty_sum.

  DATA: lt_zpmt0039 TYPE TABLE OF zpmt0039,
        ls_zpmt0039 TYPE zpmt0039.

  DATA: lt_zpmt0041 TYPE TABLE OF zpmt0041,
        ls_zpmt0041 TYPE zpmt0041.

  DATA: lt_zpmt0042 TYPE TABLE OF zpmt0042,
        ls_zpmt0042 TYPE zpmt0042.

  DATA: lt_tot_0041 TYPE TABLE OF ty_0041,
        ls_tot_0041 TYPE ty_0041.

  DATA: lt_maktx TYPE TABLE OF ty_maktx,
        ls_maktx TYPE ty_maktx.

  DATA: lt_mard   TYPE TABLE OF mard,
        ls_mard   TYPE mard,
        lt_values TYPE TABLE OF dd07v.

  DATA: it_eqkt TYPE TABLE OF eqkt,
        ws_eqkt TYPE eqkt.

  DATA: lv_dif   TYPE i.

  SELECT * INTO TABLE lt_zpmt0039
    FROM zpmt0039
    WHERE matnr IN s_matnr
      AND werks IN s_werks
      AND equnr IN s_equnr.

  SELECT * INTO TABLE lt_zpmt0042
    FROM zpmt0042
    WHERE matnr IN s_matnr.

  SORT lt_zpmt0039 BY mat_doc.
  DELETE lt_zpmt0039 WHERE mat_doc IS INITIAL.

  IF lt_zpmt0039 IS NOT INITIAL.

    SELECT * FROM eqkt INTO TABLE it_eqkt FOR ALL ENTRIES IN lt_zpmt0039
    WHERE equnr EQ lt_zpmt0039-equnr.

  ENDIF.

  SORT lt_zpmt0039 BY matnr.

  LOOP AT lt_zpmt0039 INTO ls_zpmt0039.
    MOVE-CORRESPONDING ls_zpmt0039 TO ls_estoque_inicial.
    COLLECT ls_estoque_inicial INTO lt_estoque_inicial.
  ENDLOOP.

  SORT lt_estoque_inicial BY matnr.

  IF lt_estoque_inicial[] IS NOT INITIAL.
    SELECT matnr maktx FROM makt INTO TABLE lt_maktx
             FOR ALL ENTRIES IN lt_estoque_inicial
                  WHERE matnr = lt_estoque_inicial-matnr
                    AND spras = sy-langu.
    SORT lt_maktx BY matnr.

    SELECT * FROM mard INTO TABLE lt_mard
             FOR ALL ENTRIES IN lt_estoque_inicial
                  WHERE matnr = lt_estoque_inicial-matnr
                    AND werks = lt_estoque_inicial-werks.
    SORT lt_mard BY matnr werks.
  ENDIF.

  SELECT * INTO TABLE tg_zpmt0040
    FROM zpmt0040
    WHERE pernr IN s_pernr
      AND qmnum IN s_qmnum
      AND matnr IN s_matnr
      AND budat IN s_budat
      AND werks IN s_werks
      AND equnr IN s_equnr.

  SORT tg_zpmt0040 BY mat_doc.
  DELETE tg_zpmt0040 WHERE mat_doc IS INITIAL.

  SORT tg_zpmt0040 BY werks
                      matnr.

  SORT lt_zpmt0042 BY matnr werks.

  IF tg_zpmt0040[] IS NOT INITIAL.
    SELECT pernr ename INTO TABLE lt_pa0001
      FROM pa0001
      FOR ALL ENTRIES IN tg_zpmt0040
      WHERE pernr = tg_zpmt0040-pernr.
    SORT lt_pa0001 BY pernr.

    SELECT equnr, eqktx
      FROM eqkt
      INTO TABLE @DATA(lt_eqkt)
      FOR ALL ENTRIES IN @tg_zpmt0040
      WHERE equnr = @tg_zpmt0040-equnr.
  ENDIF.

  IF lt_zpmt0039 IS NOT INITIAL.

    SELECT equnr eqktx
      FROM eqkt
      APPENDING TABLE lt_eqkt
      FOR ALL ENTRIES IN lt_zpmt0039
      WHERE equnr = lt_zpmt0039-equnr.

    SORT: lt_eqkt BY equnr.
  ENDIF.

*** Inicio - Rubenilson Pereira - 13.10.22 - US 77010
  DATA(lt_zpmt0040_aux) = tg_zpmt0040.

  SORT lt_zpmt0040_aux BY werks pernr.

  SELECT *
    FROM zpmt0065
    INTO TABLE @DATA(lt_zpmt0065)
    FOR ALL ENTRIES IN @lt_zpmt0040_aux
    WHERE werks = @lt_zpmt0040_aux-werks
      AND pernr = @lt_zpmt0040_aux-pernr.
  IF sy-subrc IS INITIAL.
    SORT lt_zpmt0065 BY werks pernr.

    IF  s_caixa IS NOT INITIAL AND p_dis IS INITIAL.

      DELETE lt_zpmt0065 WHERE caixa NOT IN s_caixa.

      IF lt_zpmt0065 IS INITIAL.
        MESSAGE 'Não há dados para a caixa indicada' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

    ELSE.

      DATA(lt_zpmt0065_aux) = lt_zpmt0065.

      SORT lt_zpmt0065 BY caixa.
      DELETE ADJACENT DUPLICATES FROM lt_zpmt0065.

      SELECT *
        FROM zpmt0066
        INTO TABLE @DATA(lt_zpmt0066)
        FOR ALL ENTRIES IN @lt_zpmt0065_aux
        WHERE caixa = @lt_zpmt0065_aux-caixa.
      IF sy-subrc IS INITIAL.
        SORT lt_zpmt0066 BY caixa.
      ENDIF.

    ENDIF.

  ENDIF.

  CALL FUNCTION 'GET_DOMAIN_VALUES'
    EXPORTING
      domname         = 'ZDPM_CAIXA'
    TABLES
      values_tab      = lt_values
    EXCEPTIONS
      no_values_found = 1
      OTHERS          = 2.
  IF sy-subrc IS INITIAL.
    SORT lt_values BY domvalue_l.
  ENDIF.


*** Fim - Rubenilson Pereira - 13.10.22 - US 77010

  SELECT * INTO TABLE lt_zpmt0041
    FROM zpmt0041
    WHERE pernr IN s_pernr
      AND werks IN s_werks
      AND qmnum IN s_qmnum
      AND matnr IN s_matnr
      AND budat IN s_budat
      AND equnr IN s_equnr.

  LOOP AT lt_zpmt0041 INTO ls_zpmt0041.
    MOVE-CORRESPONDING ls_zpmt0041 TO ls_tot_0041.
    COLLECT ls_tot_0041 INTO lt_tot_0041.
  ENDLOOP.

  SORT lt_zpmt0041 BY werks
                      matnr
                      pernr
                      qmnum.



  IF p_dis IS NOT INITIAL.

    SORT tg_zpmt0040 BY werks matnr equnr.
    SORT lt_zpmt0041 BY werks matnr equnr.

    LOOP AT lt_zpmt0039 ASSIGNING FIELD-SYMBOL(<fs_zpmt0039>).
      READ TABLE tg_zpmt0040 TRANSPORTING NO FIELDS WITH KEY
                 werks = <fs_zpmt0039>-werks
                 matnr = <fs_zpmt0039>-matnr
                 equnr = <fs_zpmt0039>-equnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE lt_zpmt0041 ASSIGNING FIELD-SYMBOL(<fs_0041>)
        WITH KEY werks = <fs_zpmt0039>-werks
                 matnr = <fs_zpmt0039>-matnr
                 equnr = <fs_zpmt0039>-equnr
        BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ELSEIF <fs_0041>-descarte IS NOT INITIAL.
          CONTINUE.
        ENDIF.

      ENDIF.

      eg_alv-equnr     = <fs_zpmt0039>-equnr.
      eg_alv-werks     = <fs_zpmt0039>-werks.
      eg_alv-matnr     = <fs_zpmt0039>-matnr.
      eg_alv-qtd_disp  = 1.
      eg_alv-data_cri  = <fs_zpmt0039>-bldat.

      READ TABLE lt_maktx INTO ls_maktx WITH KEY matnr = <fs_zpmt0039>-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        eg_alv-descr     = ls_maktx-maktx.
      ENDIF.

      PERFORM f_output_equnr CHANGING eg_alv-equnr.
      READ TABLE lt_eqkt INTO DATA(ls_eqkt) WITH KEY equnr = <fs_zpmt0039>-equnr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        eg_alv-eqktx = ls_eqkt-eqktx.
      ENDIF.

      APPEND eg_alv TO tg_alv.
      CLEAR eg_alv.

    ENDLOOP.

  ELSE.


    LOOP AT lt_estoque_inicial INTO ls_estoque_inicial.
      READ TABLE tg_zpmt0040 TRANSPORTING NO FIELDS WITH KEY werks = ls_estoque_inicial-werks
                                                             matnr = ls_estoque_inicial-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        LOOP AT tg_zpmt0040 INTO eg_zpmt0040 FROM sy-tabix.
          IF eg_zpmt0040-werks NE ls_estoque_inicial-werks OR
             eg_zpmt0040-matnr NE ls_estoque_inicial-matnr.
            EXIT.
          ENDIF.

*** Inicio - Rubenilson Pereira - 13.10.22 - US 77010
          READ TABLE lt_zpmt0065 ASSIGNING FIELD-SYMBOL(<fs_zpmt005>)
          WITH KEY werks = eg_zpmt0040-werks
                   pernr = eg_zpmt0040-pernr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            READ TABLE lt_zpmt0066 ASSIGNING FIELD-SYMBOL(<fs_zpmt0066>)
            WITH KEY caixa = <fs_zpmt005>-caixa
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              eg_alv-caixa = <fs_zpmt0066>-descricao.
            ENDIF.

          ELSEIF s_caixa IS NOT INITIAL AND p_dis IS INITIAL.
            CONTINUE.
          ENDIF.
*** Fim - Rubenilson Pereira - 13.10.22 - US 77010

          eg_alv-equnr     = eg_zpmt0040-equnr.

          READ TABLE lt_eqkt INTO ls_eqkt WITH KEY equnr = eg_alv-equnr.
          IF sy-subrc IS INITIAL.
            eg_alv-eqktx = ls_eqkt-eqktx.
          ENDIF.

          eg_alv-qmnum     = eg_zpmt0040-qmnum.
          eg_alv-mat_doc   = eg_zpmt0040-mat_doc.
          eg_alv-doc_year  = eg_zpmt0040-doc_year.
          eg_alv-bldat     = eg_zpmt0040-budat.
          eg_alv-data_cri  = eg_zpmt0040-data_cri.
          eg_alv-pernr     = eg_zpmt0040-pernr.
          PERFORM f_output_equnr CHANGING eg_alv-equnr.

          READ TABLE lt_pa0001 INTO ls_pa0001 WITH KEY pernr = eg_zpmt0040-pernr BINARY SEARCH.
          IF sy-subrc = 0.
            eg_alv-name1     = ls_pa0001-ename.
          ENDIF.
          eg_alv-werks     = eg_zpmt0040-werks.
          eg_alv-matnr     = eg_zpmt0040-matnr.
          READ TABLE lt_maktx INTO ls_maktx WITH KEY matnr = eg_zpmt0040-matnr BINARY SEARCH.
          IF sy-subrc = 0.
            eg_alv-descr     = ls_maktx-maktx.
          ENDIF.
          eg_alv-labst_ini = ls_estoque_inicial-labst.
          eg_alv-labst_emp = eg_zpmt0040-labst.
          eg_alv-labst_dev = 0.
          READ TABLE lt_zpmt0041 TRANSPORTING NO FIELDS WITH KEY werks  = eg_zpmt0040-werks
                                                           matnr        = eg_zpmt0040-matnr
                                                           pernr        = eg_zpmt0040-pernr
                                                           qmnum        = eg_zpmt0040-qmnum BINARY SEARCH.
          IF sy-subrc = 0.
            LOOP AT lt_zpmt0041 INTO ls_zpmt0041 FROM sy-tabix.
              IF ls_zpmt0041-werks         NE eg_zpmt0040-werks
               OR ls_zpmt0041-matnr        NE eg_zpmt0040-matnr
               OR ls_zpmt0041-pernr        NE eg_zpmt0040-pernr
               OR ls_zpmt0041-qmnum        NE eg_zpmt0040-qmnum.
                EXIT.
              ENDIF.
              IF p_est IS NOT INITIAL.
                IF ( ls_zpmt0041-estorno IS NOT INITIAL OR ls_zpmt0041-descarte IS NOT INITIAL ) AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                  IF ls_zpmt0041-estorno IS NOT INITIAL.
                    eg_alv-labst_dev = eg_alv-labst_dev + ls_zpmt0041-labst.
                    CONTINUE.
                  ELSE.
                    CONTINUE.
                  ENDIF.
                ELSE.
                  IF ls_zpmt0041-estorno IS NOT INITIAL .
                    READ TABLE tg_zpmt0040 TRANSPORTING NO FIELDS WITH KEY mat_doc = ls_zpmt0041-mat_doc_emp.
                    IF sy-subrc EQ 0.
                      CONTINUE.
                    ENDIF.
                  ELSE.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF p_des IS NOT INITIAL.
                IF ( ls_zpmt0041-estorno IS NOT INITIAL OR ls_zpmt0041-descarte IS NOT INITIAL )  AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                  IF ls_zpmt0041-descarte IS NOT INITIAL.
                    eg_alv-labst_dev = eg_alv-labst_dev + ls_zpmt0041-labst.
                    CONTINUE.
                  ELSE.
                    CONTINUE.
                  ENDIF.
                ELSE.
                  IF  ls_zpmt0041-descarte IS NOT INITIAL .
                    READ TABLE tg_zpmt0040 TRANSPORTING NO FIELDS WITH KEY mat_doc = ls_zpmt0041-mat_doc_emp.
                    IF sy-subrc EQ 0.
                      CONTINUE.
                    ENDIF.
                  ELSE.
                    CONTINUE.
                  ENDIF.
                ENDIF.
              ENDIF.
              IF p_dev IS NOT INITIAL.
                IF ( ls_zpmt0041-estorno IS NOT INITIAL OR ls_zpmt0041-descarte IS NOT INITIAL )  AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                  CONTINUE.
                ENDIF.
                IF ( ls_zpmt0041-estorno IS NOT INITIAL OR ls_zpmt0041-descarte IS NOT INITIAL )  AND eg_zpmt0040-mat_doc NE ls_zpmt0041-mat_doc_emp.
                  CONTINUE.
                ENDIF.
              ENDIF.
              IF p_emp IS NOT INITIAL.
                IF ( ls_zpmt0041-estorno IS NOT INITIAL OR ls_zpmt0041-descarte IS NOT INITIAL )  AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                ENDIF.
                IF ( ls_zpmt0041-estorno IS INITIAL AND ls_zpmt0041-descarte IS INITIAL )  AND eg_zpmt0040-mat_doc = ls_zpmt0041-mat_doc_emp.
                  eg_alv-labst_emp = eg_alv-labst_emp - ls_zpmt0041-labst.
                ENDIF.
              ENDIF.
              eg_alv-labst_dev = eg_alv-labst_dev + ls_zpmt0041-labst.
            ENDLOOP.
          ENDIF.
          IF p_des IS NOT INITIAL OR p_est IS NOT INITIAL.
            IF eg_alv-labst_dev = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          IF p_dev IS NOT INITIAL.
            IF eg_alv-labst_dev = 0.
              CONTINUE.
            ENDIF.
          ENDIF.
          READ TABLE lt_mard INTO ls_mard WITH KEY matnr = eg_alv-matnr
                                                   werks = eg_alv-werks BINARY SEARCH.
          IF sy-subrc = 0.
            eg_alv-labst_atual = ls_mard-labst.
          ELSE.
            eg_alv-labst_atual = eg_alv-labst_ini - eg_alv-labst_emp + eg_alv-labst_dev.
          ENDIF.

          CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
            EXPORTING
              i_datum_bis             = sy-datum
              i_datum_von             = eg_zpmt0040-budat
*             I_KZ_EXCL_VON           = '0'
*             I_KZ_INCL_BIS           = '0'
*             I_KZ_ULT_BIS            = ' '
*             I_KZ_ULT_VON            = ' '
*             I_STGMETH               = '0'
*             I_SZBMETH               = '1'
            IMPORTING
              e_tage                  = lv_dif
            EXCEPTIONS
              days_method_not_defined = 1
              OTHERS                  = 2.

          IF eg_alv-labst_emp = eg_alv-labst_dev.
            eg_alv-icon = '@08@'. "VERDE
          ELSE.
            READ TABLE lt_zpmt0042 INTO ls_zpmt0042 WITH KEY matnr = eg_zpmt0040-matnr
                                                             werks = eg_zpmt0040-werks BINARY SEARCH.
            IF sy-subrc = 0 AND ls_zpmt0042-periodo < lv_dif.
              eg_alv-icon = '@0A@'. "vERMELHO
            ELSE.
              eg_alv-icon = '@09@'. "AMARELO
            ENDIF.
          ENDIF.
          IF p_emp IS NOT INITIAL.
            IF eg_alv-labst_dev = eg_alv-labst_emp.
              CONTINUE.
            ENDIF.
            IF eg_alv-labst_emp = 0.
              CONTINUE.
            ENDIF.
          ENDIF.

          IF p_dev IS NOT INITIAL.
            IF eg_alv-labst_dev < 0.
              CONTINUE.
            ENDIF.
          ENDIF.

          IF p_dis IS NOT INITIAL.
            eg_alv-qtd_disp = eg_alv-labst_ini + eg_alv-labst_dev - eg_alv-labst_emp.
          ENDIF.

*        IF p_est IS NOT INITIAL.
*          IF ls_tot_0041-estorno IS INITIAL.
*            CONTINUE.
*          ENDIF.
*        ENDIF.
*
*        IF p_des IS NOT INITIAL.
*          IF ls_tot_0041-descarte IS INITIAL.
*            CONTINUE.
*          ENDIF.
*        ENDIF.

*        IF p_emp IS NOT INITIAL.
*          eg_alv-labst_emp = eg_alv-labst_emp - eg_alv-labst_dev.
*          eg_alv-labst_dev = 0.
*        endif.
          PERFORM f_output_matnr CHANGING eg_alv-matnr.



          APPEND eg_alv TO tg_alv.
          CLEAR: eg_alv, ls_eqkt.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  zf_exclude_tb_functions
*&---------------------------------------------------------------------*
FORM zf_exclude_tb_functions TABLES pt_exclude TYPE ui_functions.

  REFRESH pt_exclude.
  APPEND og_alv->mc_fc_loc_cut           TO pt_exclude. "Botão Recortar
  APPEND og_alv->mc_fc_loc_paste         TO pt_exclude. "Botão Colar com Sobregravação
  APPEND og_alv->mc_fc_loc_paste_new_row TO pt_exclude. "Botão Colar em Nova Linha
  APPEND og_alv->mc_fc_loc_copy_row      TO pt_exclude. "Botão Duplicar Linha
  APPEND og_alv->mc_fc_loc_append_row    TO pt_exclude. "Botão Anexar Linha
  APPEND og_alv->mc_fc_loc_insert_row    TO pt_exclude. "Botão Inserir Linha
  APPEND og_alv->mc_fc_loc_delete_row    TO pt_exclude. "Botão Deletar Linha

ENDFORM.                                        " EXCLUDE_TB_FUNCTIONS .


FORM qmnum_f4
  CHANGING
    cv_qmnum TYPE qmel-qmnum.

  TYPES: BEGIN OF ty_qmnum,
           qmnum TYPE zpmt0040-qmnum,
         END OF ty_qmnum.

  DATA:
    lt_values TYPE STANDARD TABLE OF ty_qmnum,
    lt_return TYPE STANDARD TABLE OF ddshretval
    .

  SELECT qmnum                                          "#EC CI_NOWHERE
    FROM zpmt0040
    INTO TABLE lt_values.

  SORT lt_values BY qmnum.

  DELETE ADJACENT DUPLICATES FROM lt_values COMPARING qmnum.
  DELETE lt_values WHERE qmnum IS INITIAL.

  IF lt_values[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'QMNUM'    " Name of field in VALUE_TAB
      value_org       = 'S'        " Value return: C: cell by cell, S: structured
    TABLES
      value_tab       = lt_values  " Table of values: entries cell by cell
      return_tab      = lt_return  " Return the selected value
    EXCEPTIONS
      parameter_error = 1          " Incorrect parameter
      no_values_found = 2          " No values found
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  cv_qmnum = ls_return-fieldval.

ENDFORM.


FORM pernr_f4
  CHANGING
    cv_pernr TYPE zpmt0040-pernr.

  TYPES: BEGIN OF ty_pernr,
           pernr TYPE zpmt0040-pernr,
         END OF ty_pernr.

  DATA:
    lt_values TYPE STANDARD TABLE OF ty_pernr,
    lt_return TYPE STANDARD TABLE OF ddshretval
    .

  SELECT pernr                                          "#EC CI_NOWHERE
    FROM zpmt0040
    INTO TABLE lt_values.

  SORT lt_values BY pernr.

  DELETE ADJACENT DUPLICATES FROM lt_values COMPARING pernr.
  DELETE lt_values WHERE pernr IS INITIAL.

  IF lt_values[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'PERNR'    " Name of field in VALUE_TAB
      value_org       = 'S'        " Value return: C: cell by cell, S: structured
    TABLES
      value_tab       = lt_values  " Table of values: entries cell by cell
      return_tab      = lt_return  " Return the selected value
    EXCEPTIONS
      parameter_error = 1          " Incorrect parameter
      no_values_found = 2          " No values found
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  cv_pernr = ls_return-fieldval.

ENDFORM.


FORM matnr_f4
  CHANGING
    cv_matnr TYPE zpmt0040-matnr.

  TYPES: BEGIN OF ty_matnr,
           matnr TYPE zpmt0040-matnr,
         END OF ty_matnr.

  DATA:
    lt_values TYPE STANDARD TABLE OF ty_matnr,
    lt_return TYPE STANDARD TABLE OF ddshretval
    .

  SELECT matnr                                          "#EC CI_NOWHERE
    FROM zpmt0040
    INTO TABLE lt_values.

  SORT lt_values BY matnr.

  DELETE ADJACENT DUPLICATES FROM lt_values COMPARING matnr.
  DELETE lt_values WHERE matnr IS INITIAL.

  IF lt_values[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'MATNR'    " Name of field in VALUE_TAB
      value_org       = 'S'        " Value return: C: cell by cell, S: structured
    TABLES
      value_tab       = lt_values  " Table of values: entries cell by cell
      return_tab      = lt_return  " Return the selected value
    EXCEPTIONS
      parameter_error = 1          " Incorrect parameter
      no_values_found = 2          " No values found
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  cv_matnr = ls_return-fieldval.

ENDFORM.

FORM usercmnd USING r_ucomm LIKE sy-ucomm
                    rs_selfield TYPE slis_selfield .

  SET PARAMETER ID 'MAT' FIELD rs_selfield-value.

  LEAVE TO TRANSACTION 'MM03'.

ENDFORM.

*  &---------------------------------------------------------------------*
*  &      Form  zf_handle_toolbar
*  &---------------------------------------------------------------------*
FORM zf_handle_toolbar USING e_object TYPE REF TO cl_alv_event_toolbar_set.

  DATA: el_toolbar TYPE stb_button.
  IF p_dev IS NOT INITIAL.
    CLEAR el_toolbar.
    el_toolbar-function   = 'IMPR_DEV'.
    el_toolbar-icon       = icon_print.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo = 'Impressão Termo Devolução'(t44).
    el_toolbar-text      = 'Impressão Termo Devolução'(t44).
    APPEND el_toolbar TO e_object->mt_toolbar.

  ELSEIF p_emp IS NOT INITIAL.

    CLEAR el_toolbar.
    el_toolbar-function   = 'IMPR_DEV'.
    el_toolbar-icon       = icon_print.
    el_toolbar-butn_type  = 0.
    el_toolbar-quickinfo = 'Impressão Termo Responsabilidade'(t44).
    el_toolbar-text      = 'Impressão Termo Responsabilidade'(t44).
    APPEND el_toolbar TO e_object->mt_toolbar.

  ENDIF.
ENDFORM.                    "zf_handle_toolbar
*  &---------------------------------------------------------------------*
*  &      Form  zf_handle_user_command
*  &---------------------------------------------------------------------*
FORM zf_handle_user_command USING x_ucomm.

  CASE x_ucomm.


    WHEN 'IMPR_DEV'.

      PERFORM z_call_form_dev.


  ENDCASE.


ENDFORM.                    "zf_handle_user_command



*  &---------------------------------------------------------------------*
*  &      Form  Z_CALL_FORM                                              *
*  &---------------------------------------------------------------------*
*                              Chama Formulário                          *
*  ----------------------------------------------------------------------*
FORM z_call_form_dev.
  DATA: vl_formname TYPE tdsfname,
        vl_name     TYPE rs38l_fnam.

  DATA: lt_zpms0040 TYPE TABLE OF zpms0040,
        ls_zpms0040 TYPE zpms0040.

  DATA: lv_empregado(100) TYPE c.
  DATA: lv_dt_admissao(100) TYPE c.
  DATA: lv_unidade(100) TYPE c.
  DATA: lv_funcao(100) TYPE c.
  DATA: lv_setor(100) TYPE c.

  DATA: lt_pa0001 TYPE TABLE OF pa0001.
  DATA: ls_pa0001 TYPE pa0001.


  DATA: lt_zpms0041 TYPE TABLE OF zpms0041,
        ls_zpms0041 TYPE zpms0041.

  DATA: ls_alv TYPE yg_alv.

  IF p_emp IS NOT INITIAL.

    LOOP AT tg_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
      IF <fs_alv>-proces IS INITIAL.
        CONTINUE.
      ENDIF.
      MOVE-CORRESPONDING <fs_alv> TO ls_zpms0040.

      ls_zpms0040-budat = <fs_alv>-data_cri.
      ls_zpms0040-labst = <fs_alv>-labst_emp.
      APPEND ls_zpms0040 TO lt_zpms0040.
    ENDLOOP.

    CONCATENATE <fs_alv>-pernr  '-' <fs_alv>-name1 INTO lv_empregado SEPARATED BY space.

    IF lt_zpms0040[] IS NOT INITIAL.
      vl_formname = 'ZPMF0006'.

      CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
        EXPORTING
          formname           = vl_formname
        IMPORTING
          fm_name            = vl_name
        EXCEPTIONS
          no_form            = 1
          no_function_module = 2
          OTHERS             = 3.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        EXIT.
      ENDIF.

      IF <fs_alv>-pernr IS NOT INITIAL.
        SELECT * INTO TABLE lt_pa0001
          FROM pa0001
          WHERE pernr = <fs_alv>-pernr.

        SORT  lt_pa0001 BY begda.
        READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
        lv_dt_admissao = ls_pa0001-begda+6(2)  && '/' &&  ls_pa0001-begda+4(2) && '/' && ls_pa0001-begda(4).

        SORT  lt_pa0001 BY endda DESCENDING.
        READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
        lv_empregado = ls_pa0001-ename.

        SELECT SINGLE orgtx INTO lv_setor
          FROM t527x
          WHERE orgeh = ls_pa0001-orgeh
            AND sprsl = sy-langu
            AND endda > sy-datum.

        SELECT SINGLE stltx INTO lv_funcao
          FROM t513s
          WHERE stell = ls_pa0001-stell
            AND sprsl = sy-langu
            AND endda > sy-datum.

      ENDIF.

      IF <fs_alv>-werks IS NOT INITIAL.
        SELECT SINGLE name1 INTO lv_unidade
          FROM t001w
          WHERE werks = <fs_alv>-werks.
        CONCATENATE <fs_alv>-werks '-' lv_unidade INTO lv_unidade SEPARATED BY space.
      ENDIF.
      CALL FUNCTION vl_name
        EXPORTING
          p_unidade        = lv_unidade
          p_empregado      = lv_empregado
          p_funcao         = lv_funcao
          p_setor          = lv_setor
          p_dt_admissao    = lv_dt_admissao
        TABLES
          t_emprest        = lt_zpms0040
        EXCEPTIONS
          formatting_error = 1
          internal_error   = 2
          send_error       = 3
          user_canceled    = 4
          OTHERS           = 5.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

  ELSE.

    LOOP AT  tg_alv INTO eg_alv WHERE proces = 'X'.
      REFRESH lt_zpms0041.
      LOOP AT tg_alv INTO ls_alv WHERE qmnum = eg_alv-qmnum
                                   AND mat_doc = eg_alv-mat_doc
                                   AND doc_year = eg_alv-doc_year.
        IF ls_alv-labst_dev IS INITIAL.
          CONTINUE.
        ENDIF.
        MOVE-CORRESPONDING ls_alv TO ls_zpms0041.
        ls_zpms0041-labst = ls_alv-labst_emp.
        ls_zpms0041-budat = ls_alv-bldat.
        ls_zpms0041-budat_dev = ls_alv-data_cri.
        ls_zpms0041-labst_dev = ls_alv-labst_emp.
        APPEND ls_zpms0041 TO lt_zpms0041.
      ENDLOOP.

      CONCATENATE eg_alv-pernr  '-' eg_alv-name1 INTO lv_empregado SEPARATED BY space.

      IF lt_zpms0041[] IS NOT INITIAL.

        vl_formname = 'ZPMF0007'.

        CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
          EXPORTING
            formname           = vl_formname
          IMPORTING
            fm_name            = vl_name
          EXCEPTIONS
            no_form            = 1
            no_function_module = 2
            OTHERS             = 3.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          EXIT.
        ENDIF.

        IF eg_alv-pernr IS NOT INITIAL.
          SELECT * INTO TABLE lt_pa0001
            FROM pa0001
            WHERE pernr = eg_alv-pernr.

          SORT  lt_pa0001 BY begda.
          READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.
          lv_dt_admissao = ls_pa0001-begda+6(2)  && '/' &&  ls_pa0001-begda+4(2) && '/' && ls_pa0001-begda(4).
          lv_empregado = ls_pa0001-ename.

          SORT  lt_pa0001 BY endda DESCENDING.
          READ TABLE lt_pa0001 INTO ls_pa0001 INDEX 1.

          SELECT SINGLE orgtx INTO lv_setor
            FROM t527x
            WHERE orgeh = ls_pa0001-orgeh
              AND sprsl = sy-langu
              AND endda > sy-datum.

          SELECT SINGLE stltx INTO lv_funcao
            FROM t513s
            WHERE stell = ls_pa0001-stell
              AND sprsl = sy-langu
              AND endda > sy-datum.

          CONDENSE lv_funcao.

        ENDIF.

        IF eg_alv-werks IS NOT INITIAL.
          SELECT SINGLE name1 INTO lv_unidade
            FROM t001w
            WHERE werks = eg_alv-werks.
          CONCATENATE eg_alv-werks '-' lv_unidade INTO lv_unidade SEPARATED BY space.
        ENDIF.
        CALL FUNCTION vl_name
          EXPORTING
            p_unidade        = lv_unidade
            p_empregado      = lv_empregado
            p_funcao         = lv_funcao
            p_setor          = lv_setor
            p_dt_admissao    = lv_dt_admissao
          TABLES
            t_dev            = lt_zpms0041
          EXCEPTIONS
            formatting_error = 1
            internal_error   = 2
            send_error       = 3
            user_canceled    = 4
            OTHERS           = 5.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.
ENDFORM.                    " Z_CALL_FORM
*&---------------------------------------------------------------------*
*&      Form  F_OUTPUT_EQUNR
*&---------------------------------------------------------------------*
FORM f_output_equnr  CHANGING p_equnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_equnr
    IMPORTING
      output = p_equnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_OUTPUT_MATNR
*&---------------------------------------------------------------------*
FORM f_output_matnr  CHANGING p_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
    EXPORTING
      input  = p_matnr
    IMPORTING
      output = p_matnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CAIXA_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_S_CAIXA_LOW  text
*----------------------------------------------------------------------*
FORM caixa_f4  CHANGING p_s_caixa_low.

  TYPES: BEGIN OF ty_caixa,
           caixa TYPE zpmt0065-caixa,
         END OF ty_caixa.

  DATA:
    lt_values TYPE STANDARD TABLE OF ty_caixa,
    lt_return TYPE STANDARD TABLE OF ddshretval
    .

  SELECT caixa                                          "#EC CI_NOWHERE
    FROM zpmt0065
    INTO TABLE lt_values.

  SORT lt_values BY caixa.

  DELETE ADJACENT DUPLICATES FROM lt_values COMPARING caixa.
  DELETE lt_values WHERE caixa IS INITIAL.

  IF lt_values[] IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'CAIXA'    " Name of field in VALUE_TAB
      value_org       = 'S'        " Value return: C: cell by cell, S: structured
    TABLES
      value_tab       = lt_values  " Table of values: entries cell by cell
      return_tab      = lt_return  " Return the selected value
    EXCEPTIONS
      parameter_error = 1          " Incorrect parameter
      no_values_found = 2          " No values found
      OTHERS          = 3.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
  p_s_caixa_low = ls_return-fieldval.

ENDFORM.
