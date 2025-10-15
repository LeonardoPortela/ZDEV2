**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Rodrigo Carvalho ( rodrigo.sa@amaggi.com.br)                         |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Wellington.pereira ( wellington.pereira@amaggi.com.br )              |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Extrator KSB1N- Interface GEO                                             |*
**/===========================================================================\*
REPORT zcor024.



**********************************************************************
* Tables
**********************************************************************
TABLES: t54c6, pernr, /tmf/d_lanc_cont, pa0001, pa0000, rsvar.



**********************************************************************
* Types
**********************************************************************

DATA: BEGIN OF t_dados_ksb1n OCCURS 0.
        INCLUDE STRUCTURE kaep_coac.
DATA: END OF t_dados_ksb1n.


**********************************************************************
* Tabela interna
**********************************************************************
DATA: t_zcot0015                   TYPE TABLE OF zcot0015,
      wa_zcot0015                  TYPE zcot0015,
      t_brg_cstg_intdg             TYPE TABLE OF zcot0016,
      wa_brg_cstg_intdg            TYPE zcot0016,
      t_brg_cstg_intdg_rfc         TYPE TABLE OF zcoe_despesas_ccusto,
      wa_brg_cstg_intdg_rfc        TYPE zcoe_despesas_ccusto,
      t_brg_os_material            TYPE TABLE OF zcot0017,
      wa_brg_os_material           TYPE zcot0017,
      t_brg_os_material_rfc        TYPE TABLE OF zmme_return_ordem_servico,
      wa_brg_os_material_rfc       TYPE zmme_return_ordem_servico,
      t_dados_ksb1n_sem_parametro  TYPE TABLE OF zcot0018,
      wa_dados_ksb1n_sem_parametro TYPE zcot0018.



**********************************************************************
* Constant
**********************************************************************
CONSTANTS  c_variante_zcor024 TYPE tvarvc-name VALUE 'Z_CO_VARIANTE_ZCOR024'.


**********************************************************************
* Variável
**********************************************************************
DATA: l_leave        TYPE syst_ucomm,
      v_p_db_tab(8)  TYPE c,
      v_p_stcnam(12) TYPE c,
      v_p_scmant(4)  TYPE c,
      v_p_title(40)  TYPE c,
      g_sel_var      TYPE rsvar-variant,
      g_sel_vartxt   TYPE rsvar-vtext,
      l_opcao        TYPE char1.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: p_tcode  TYPE sy-tcode,
      p_maxsel TYPE kaep_sett-maxsel.


DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.


**********************************************************************
* ranges
**********************************************************************
DATA: rg_begda TYPE RANGE OF pa0001-begda,
      wa_begda LIKE LINE  OF rg_begda.



**********************************************************************
* Parâmetros de seleção
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_varia LIKE rsvar-variant.

SELECTION-SCREEN END OF BLOCK b1.





**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE: l_leave.

  CASE sy-ucomm.
    WHEN 'BT001'.
      CALL TRANSACTION 'ZCO0032'.
    WHEN 'ONLI'.
      "PERFORM f_start_selection.
    WHEN 'VARIANT'.
      PERFORM carrega_variantes.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

  IF v_p_db_tab IS NOT INITIAL.
    l_opcao = '1'.
    SUBMIT zregister_data       WITH p_db_tab = v_p_db_tab
                                WITH p_stcnam = v_p_stcnam
                                WITH p_scmant = v_p_scmant
                                WITH p_title  = v_p_title
    AND RETURN.
    CLEAR: v_p_db_tab, v_p_stcnam, v_p_scmant, v_p_title.
  ENDIF.



**********************************************************************
*SELECTION-SCREEN output
**********************************************************************
AT SELECTION-SCREEN OUTPUT.

  "Seleciona nome da variante padrao
  IF p_varia IS INITIAL.
    SELECT low UP TO 1 ROWS
           FROM tvarvc
           INTO p_varia
           WHERE name EQ c_variante_zcor024.
    ENDSELECT.
  ENDIF.

  IF l_opcao = 1.
    LOOP AT SCREEN.
    ENDLOOP.

    FREE MEMORY ID 'ZCOR024'.
    l_leave = 'LEAVE'.
    EXPORT l_leave FROM l_leave TO MEMORY ID 'ZCOR024'.

    l_opcao                = '1'.

  ENDIF.


**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.


  SET PF-STATUS '1000'.

  FREE MEMORY ID 'ZCOR024'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZCOR024'.

  l_opcao                = '1'.


**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.


  PERFORM f_dados_seleciona_ksb1n.

  PERFORM f_processa_dados_ksb1n.

  PERFORM f_envia_registros_rfc.

  PERFORM f_envia_email.

  MESSAGE 'Processamento concluido com sucesso!' TYPE 'S'.


*&---------------------------------------------------------------------*
*& Form carrega_variantes
*&---------------------------------------------------------------------*
FORM carrega_variantes .

  PERFORM escolher_variante CHANGING g_sel_var.

  IF g_sel_var NE space.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-repid
        variant              = g_sel_var
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form escolher_variante
*&---------------------------------------------------------------------*
FORM escolher_variante  CHANGING p_g_sel_var.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
      masked               = 'X'
    IMPORTING
      sel_variant          = p_g_sel_var
      sel_variant_text     = g_sel_vartxt
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_dados_seleciona_ksb1n
*&---------------------------------------------------------------------*
FORM f_dados_seleciona_ksb1n .

  DATA: lt_seltab TYPE TABLE OF rsparams,
        ls_seltab LIKE LINE OF lt_seltab.

  DATA: lt_valutab LIKE   TABLE OF rsparams WITH HEADER LINE.

  "Seleciona Parâmetros Interface GEO - KSB1N
  SELECT *
         FROM zcot0015
         INTO TABLE t_zcot0015.


  p_tcode = 'KSB1N'.
  p_maxsel = '999999999'.


  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report               = 'RKAEP000'
      variant              = p_varia
    TABLES
      valutab              = lt_valutab
    EXCEPTIONS
      variant_non_existent = 1
      variant_obsolete     = 2
      OTHERS               = 3.

  READ TABLE lt_valutab INTO DATA(lwa_valutab_rbudat) WITH KEY selname = 'R_BUDAT'.
  IF sy-subrc NE 0.
    MESSAGE |Variante { p_varia } não possui parametro de periode de lançamento! | TYPE 'I'.
    STOP.
  ENDIF.

  IF lwa_valutab_rbudat-low IS NOT INITIAL.
    lwa_valutab_rbudat-low = lwa_valutab_rbudat-low+6(4) && lwa_valutab_rbudat-low+3(2) && lwa_valutab_rbudat-low+(2).
  ENDIF.

  IF lwa_valutab_rbudat-high IS NOT INITIAL.
    lwa_valutab_rbudat-high = lwa_valutab_rbudat-high+6(4) && lwa_valutab_rbudat-high+3(2) && lwa_valutab_rbudat-high+(2).
  ENDIF.

  APPEND  VALUE #(  selname = 'P_TCODE'  kind = 'P' sign    = 'I' option  = 'EQ' low     = p_tcode ) TO lt_seltab.
  APPEND  VALUE #(  selname = 'P_MAXSEL' kind = 'P' sign    = 'I' option  = 'EQ' low     = p_maxsel ) TO lt_seltab.
  "Necessário essa passagem de parametro porque o submit não reconhece parametros calculados em variante.
  APPEND  VALUE #(  selname = 'R_BUDAT'
                    kind    = lwa_valutab_rbudat-kind
                    sign    = lwa_valutab_rbudat-sign
                    option  = lwa_valutab_rbudat-option
                    low     = lwa_valutab_rbudat-low
                    high    = lwa_valutab_rbudat-high ) TO lt_seltab.

  PERFORM f_prepare_run_time_info.

  SUBMIT rkaep000
           WITH SELECTION-TABLE lt_seltab
           USING SELECTION-SET p_varia
           EXPORTING LIST TO MEMORY AND RETURN.

  PERFORM f_get_runtime_info.

  IF <t_data> IS ASSIGNED.
    LOOP AT <t_data> ASSIGNING <w_data>.
      CLEAR: t_dados_ksb1n.
      MOVE-CORRESPONDING <w_data> TO t_dados_ksb1n.
      APPEND t_dados_ksb1n.
    ENDLOOP.
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_prepare_run_time_info
*&---------------------------------------------------------------------*
FORM f_prepare_run_time_info .

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

  FREE: l_data,  l_data_line,  l_data_descr,  l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_false
                                          data     = abap_true ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_runtime_info
*&---------------------------------------------------------------------*
FORM f_get_runtime_info .

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data_descr      = l_data_descr
          r_data_line_descr = l_data_line_descr ).

      CHECK ( l_data_descr IS NOT INITIAL ) OR ( l_data_line_descr IS  NOT INITIAL ).

      CREATE DATA l_data      TYPE HANDLE  l_data_descr.
      CREATE DATA l_data_line TYPE HANDLE  l_data_line_descr.

      ASSIGN l_data->* TO <t_data>.
      ASSIGN l_data_line->* TO <t_data_line>.

      cl_salv_bs_runtime_info=>get_data( IMPORTING t_data      = <t_data>
                                                   t_data_line = <t_data_line> ).
    CATCH cx_salv_bs_sc_runtime_info.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).

  ASSIGN l_data->*        TO <w_data>.
  ASSIGN l_data_line->*   TO <w_data_line>.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_dados_ksb1n
*&---------------------------------------------------------------------*
FORM f_processa_dados_ksb1n .

  DATA: vl_operacao	TYPE j_vorgang,
        vl_tp_doc	  TYPE blart.

  DATA(_data_processamento) = sy-datum.
  DATA(_hora_processamento) = sy-uzeit.
  DATA(_user_processamento) = sy-uname.


  LOOP AT t_dados_ksb1n INTO DATA(wa_dados_ksb1n).

    READ TABLE t_zcot0015 INTO wa_zcot0015   WITH KEY operacao = wa_dados_ksb1n-vrgng tp_doc = wa_dados_ksb1n-blart wrttp = wa_dados_ksb1n-wrttp.
    IF sy-subrc NE 0.
      READ TABLE t_zcot0015 INTO wa_zcot0015 WITH KEY operacao = vl_operacao          tp_doc = wa_dados_ksb1n-blart wrttp = wa_dados_ksb1n-wrttp.
    ENDIF.
    IF sy-subrc NE 0.
      READ TABLE t_zcot0015 INTO wa_zcot0015 WITH KEY operacao = wa_dados_ksb1n-vrgng tp_doc = vl_tp_doc wrttp = wa_dados_ksb1n-wrttp.
    ENDIF.
    IF sy-subrc NE 0.
      READ TABLE t_zcot0015 INTO wa_zcot0015 WITH KEY operacao = vl_operacao           tp_doc = vl_tp_doc wrttp = wa_dados_ksb1n-wrttp.
    ENDIF.
    IF sy-subrc EQ 0.

      IF NOT wa_zcot0015-brg_cstg_intdg IS INITIAL.

        zcl_co_utils=>converte_ccusto_agro_otelhar( CHANGING c_kostl = wa_dados_ksb1n-kostl ).

        wa_brg_cstg_intdg-operacao  = wa_dados_ksb1n-vrgng.
        wa_brg_cstg_intdg-tp_doc    = wa_dados_ksb1n-blart.
        wa_brg_cstg_intdg-wrttp     = wa_dados_ksb1n-wrttp.
        wa_brg_cstg_intdg-budat     = wa_dados_ksb1n-budat.
        wa_brg_cstg_intdg-hkont     = wa_dados_ksb1n-kstar.
        "wa_brg_cstg_intdg-dmbtr     = wa_dados_ksb1n-wrgbtr.
        wa_brg_cstg_intdg-dmbtr     = wa_dados_ksb1n-wogbtr.
        wa_brg_cstg_intdg-kostl     = wa_dados_ksb1n-kostl.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wa_brg_cstg_intdg-kostl
          IMPORTING
            output = wa_brg_cstg_intdg-kostl.


        wa_brg_cstg_intdg-data      = _data_processamento.
        wa_brg_cstg_intdg-hora      = _hora_processamento.
        wa_brg_cstg_intdg-usuario   = _user_processamento.
        wa_brg_cstg_intdg-variante  = p_varia.
        COLLECT wa_brg_cstg_intdg INTO t_brg_cstg_intdg.
      ENDIF.

      IF NOT wa_zcot0015-brg_os_material IS INITIAL.

        zcl_co_utils=>converte_ccusto_agro_otelhar( CHANGING c_kostl = wa_dados_ksb1n-kostl
                                                             c_bukrs = wa_dados_ksb1n-bukrs ).

        wa_brg_os_material-operacao      = wa_dados_ksb1n-vrgng.
        wa_brg_os_material-tp_doc        = wa_dados_ksb1n-blart.
        wa_brg_os_material-wrttp         = wa_dados_ksb1n-wrttp.
        wa_brg_os_material-no_os_number  = wa_dados_ksb1n-prtau.
        wa_brg_os_material-erdat         = wa_dados_ksb1n-budat.
        "wa_brg_os_material-erzet         = wa_dados_ksb1n-cputm.
        wa_brg_os_material-docmat        = wa_dados_ksb1n-bukrs.
        wa_brg_os_material-maktx         = wa_dados_ksb1n-cel_ltxt.
        wa_brg_os_material-hkont         = wa_dados_ksb1n-kstar.
        wa_brg_os_material-menge         = wa_dados_ksb1n-mbgbtr.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            input  = wa_dados_ksb1n-kstar
          IMPORTING
            output = wa_brg_os_material-matnr.

        wa_brg_os_material-dmbtr         = wa_dados_ksb1n-wogbtr.
        wa_brg_os_material-dmbe2         = wa_dados_ksb1n-wkgbtr.
        wa_brg_os_material-kostl         = wa_dados_ksb1n-kostl.
        wa_brg_os_material-data          = _data_processamento.
        wa_brg_os_material-hora          = _hora_processamento.
        wa_brg_os_material-usuario       = _user_processamento.
        wa_brg_os_material-variante      = p_varia.

        COLLECT wa_brg_os_material INTO t_brg_os_material.
      ENDIF.

    ELSE.

      wa_dados_ksb1n_sem_parametro-operacao  = wa_dados_ksb1n-vrgng.
      wa_dados_ksb1n_sem_parametro-tp_doc    = wa_dados_ksb1n-blart.
      wa_dados_ksb1n_sem_parametro-wrttp     = wa_dados_ksb1n-wrttp.
      wa_dados_ksb1n_sem_parametro-data      = _data_processamento.
      wa_dados_ksb1n_sem_parametro-hora      = _hora_processamento.
      wa_dados_ksb1n_sem_parametro-usuario   = _user_processamento.
      wa_dados_ksb1n_sem_parametro-variante  = p_varia.
      APPEND wa_dados_ksb1n_sem_parametro TO t_dados_ksb1n_sem_parametro.

    ENDIF.

  ENDLOOP.

  "Tratamentos Tabela t_brg_os_material
  LOOP AT t_brg_os_material ASSIGNING FIELD-SYMBOL(<fs_brg_os_material>).
    IF <fs_brg_os_material>-dmbtr < 0 AND <fs_brg_os_material>-dmbe2 < 0.
      <fs_brg_os_material>-dmbtr = abs( <fs_brg_os_material>-dmbtr ).
      <fs_brg_os_material>-dmbe2 = abs( <fs_brg_os_material>-dmbe2 ).
      <fs_brg_os_material>-shkzg = '2'.
    ELSE.
      <fs_brg_os_material>-shkzg = '1'.
    ENDIF.

    <fs_brg_os_material>-erzet = _hora_processamento.

    IF <fs_brg_os_material>-menge IS INITIAL.
      <fs_brg_os_material>-menge = 1.
    ENDIF.

  ENDLOOP.

*  MOVE-CORRESPONDING t_brg_os_material[] TO t_brg_os_material_rfc[].
*  MOVE-CORRESPONDING t_brg_cstg_intdg[]  TO t_brg_cstg_intdg_rfc[].

  LOOP AT t_brg_cstg_intdg INTO wa_brg_cstg_intdg.
    CLEAR: wa_brg_cstg_intdg_rfc.
    MOVE-CORRESPONDING wa_brg_cstg_intdg TO wa_brg_cstg_intdg_rfc.
    COLLECT wa_brg_cstg_intdg_rfc INTO t_brg_cstg_intdg_rfc.
  ENDLOOP.

  LOOP AT t_brg_os_material INTO wa_brg_os_material.
    CLEAR: wa_brg_os_material_rfc.
    MOVE-CORRESPONDING wa_brg_os_material TO wa_brg_os_material_rfc.
    COLLECT wa_brg_os_material_rfc INTO t_brg_os_material_rfc.
  ENDLOOP.

  LOOP AT t_brg_os_material_rfc ASSIGNING FIELD-SYMBOL(<fs_brg_os_material_rfc>).
    <fs_brg_os_material_rfc>-buzei       = 1.
    <fs_brg_os_material_rfc>-competencia = <fs_brg_os_material_rfc>-erdat+4(2) && <fs_brg_os_material_rfc>-erdat(4).
  ENDLOOP.

  LOOP AT t_brg_cstg_intdg_rfc ASSIGNING FIELD-SYMBOL(<fs_brg_cstg_intdg_rfc>).
    <fs_brg_cstg_intdg_rfc>-competencia = <fs_brg_cstg_intdg_rfc>-budat+4(2) && <fs_brg_cstg_intdg_rfc>-budat(4).
  ENDLOOP.

  "DELETE t_brg_os_material_rfc WHERE menge IS INITIAL OR dmbtr IS INITIAL.

  IF NOT t_dados_ksb1n_sem_parametro[] IS INITIAL.
    MODIFY zcot0018 FROM TABLE t_dados_ksb1n_sem_parametro.
    IF sy-subrc EQ 0.
      COMMIT WORK.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_envia_registros_rfc
*&---------------------------------------------------------------------*
FORM f_envia_registros_rfc .

  DATA: lv_rfc    TYPE rfcdest.

  CONSTANTS: c_fm1 TYPE rs38l_fnam VALUE 'Z_CO_OUTBOUND_DESPESAS_CC'.
  CONSTANTS: c_fm2 TYPE rs38l_fnam VALUE 'Z_MM_OUTBOUND_ORDEM_SERVICO'.

*------------------------------------------------------------------------------------------------------------------------*
*  Envia Registros via RFC Tabela t_brg_cstg_intdg
*------------------------------------------------------------------------------------------------------------------------*

  DATA(t_brg_cstg_intdg_rfc_list)  = t_brg_cstg_intdg_rfc[].
  DATA(t_brg_cstg_intdg_rfc_group) = t_brg_cstg_intdg_rfc[].

  SORT t_brg_cstg_intdg_rfc_group BY competencia.
  DELETE ADJACENT DUPLICATES FROM t_brg_cstg_intdg_rfc_group COMPARING competencia.

  LOOP AT t_brg_cstg_intdg_rfc_group INTO DATA(lwa_brg_cstg_intdg_rfc_group).

    CLEAR: t_brg_cstg_intdg_rfc[].

    LOOP AT t_brg_cstg_intdg_rfc_list INTO DATA(wa_brg_cstg_intdg_rfc_list) WHERE competencia = lwa_brg_cstg_intdg_rfc_group-competencia.
      APPEND wa_brg_cstg_intdg_rfc_list TO t_brg_cstg_intdg_rfc.
    ENDLOOP.

    CHECK t_brg_cstg_intdg_rfc[] IS NOT INITIAL.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm1
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm1 IN BACKGROUND TASK
        AS SEPARATE UNIT
        DESTINATION lv_rfc
        TABLES
          return = t_brg_cstg_intdg_rfc.
    ELSE.
      CALL FUNCTION c_fm1 IN BACKGROUND TASK
        TABLES
          return = t_brg_cstg_intdg_rfc.
    ENDIF.

    COMMIT WORK.

  ENDLOOP.


  MODIFY zcot0016 FROM TABLE t_brg_cstg_intdg.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.


*------------------------------------------------------------------------------------------------------------------------*
*  Envia Registros via RFC Tabela t_brg_os_material
*------------------------------------------------------------------------------------------------------------------------*

  CLEAR lv_rfc.

  DATA(t_brg_os_material_rfc_list)  = t_brg_os_material_rfc[].
  DATA(t_brg_os_material_rfc_group) = t_brg_os_material_rfc[].

  SORT t_brg_os_material_rfc_group BY competencia.
  DELETE ADJACENT DUPLICATES FROM t_brg_os_material_rfc_group COMPARING competencia.

  LOOP AT t_brg_os_material_rfc_group INTO DATA(lwa_brg_os_material_rfc_group).

    CLEAR: t_brg_os_material_rfc[].

    LOOP AT t_brg_os_material_rfc_list INTO DATA(wa_brg_os_material_rfc_list) WHERE competencia = lwa_brg_os_material_rfc_group-competencia.
      APPEND wa_brg_os_material_rfc_list TO t_brg_os_material_rfc.
    ENDLOOP.

    CHECK t_brg_os_material_rfc[] IS NOT INITIAL.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm2
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm2 IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          return_sucess = t_brg_os_material_rfc.
    ELSE.
      CALL FUNCTION c_fm2 IN BACKGROUND TASK
        TABLES
          return_sucess = t_brg_os_material_rfc.
    ENDIF.

    COMMIT WORK.

  ENDLOOP.

  MODIFY zcot0017 FROM TABLE t_brg_os_material.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_envia_email
*&---------------------------------------------------------------------*
FORM f_envia_email .

  "Class data declaration
  DATA: go_send_request  TYPE REF TO cl_bcs,
        go_document      TYPE REF TO cl_document_bcs,
        lo_senderint     TYPE REF TO cl_cam_address_bcs,
        lo_bcs_exception TYPE REF TO cx_bcs.

*Bcs CLASS for sending mail
  DATA: lo_bcs       TYPE REF TO cl_bcs,
        lo_doc_bcs   TYPE REF TO cl_document_bcs,
        lo_sender    TYPE REF TO if_sender_bcs,
        lo_recipient TYPE REF TO if_recipient_bcs,
        lv_subject   TYPE so_obj_des,
        lv_email     TYPE adr6-smtp_addr,
        lt_soli      TYPE soli_tab,
        ls_soli      TYPE soli.

  DATA: vg_cnt         TYPE i,
        vg_string(700) TYPE c,
        vg_assunto     TYPE so_obj_des,
        vg_assunto_aux TYPE string,
        vg_sent_to_all TYPE os_boolean,
        vg_exc         TYPE REF TO cx_root,
        gv_datum(10)   TYPE c.

*Class for cobining HMTL & Image
  DATA : lo_mime_helper   TYPE REF TO cl_gbt_multirelated_service.


  IF NOT t_dados_ksb1n_sem_parametro[] IS INITIAL.

    SELECT email
           FROM zmail
           INTO TABLE @DATA(t_zmail)
           WHERE tcode EQ 'ZCO0033'.


    LOOP AT t_zmail INTO DATA(wa_zmail).

      TRY.

          "Create persistent send request
          go_send_request = cl_bcs=>create_persistent( ).


          vg_assunto = 'Interface SAP x GEO - Registros não Integrados'.


          CLEAR: ls_soli, lt_soli[].
          ls_soli = '<body>'.
          APPEND ls_soli TO lt_soli.

          CLEAR ls_soli.
          ls_soli = '<p><font size="2" face="verdana">'.
          APPEND ls_soli TO lt_soli.

          CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO gv_datum.
          CLEAR ls_soli.
          CONCATENATE '<p>' 'No processamento do dia' gv_datum ', alguns registros não foram integrados devido falta de parâmetro: Verificar logs de processamento na transação ZCO0033.' '<br>'
          INTO ls_soli SEPARATED BY space.
          APPEND ls_soli TO lt_soli.


          " Titulos até 50 caractes
          go_document = cl_document_bcs=>create_document(
            i_type    = 'HTM'
            i_text    = lt_soli
            i_subject = vg_assunto ).



          "Add document to send request
          CALL METHOD go_send_request->set_document( go_document ).

          "Add recipient with its respective attributes to send request
          IF NOT wa_zmail-email IS INITIAL.

            lv_email =  wa_zmail-email.

            "Quem irá receber o e-mail
            lo_recipient = cl_cam_address_bcs=>create_internet_address( lv_email ).

            CALL METHOD go_send_request->add_recipient
              EXPORTING
                i_recipient = lo_recipient
                i_express   = 'X'.

          ENDIF.

          "Send document
          CALL METHOD go_send_request->set_send_immediately
            EXPORTING
              i_send_immediately = 'X'.

          CALL METHOD go_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = vg_sent_to_all ).

          IF vg_sent_to_all = 'X'.

            COMMIT WORK.

            "MESSAGE e002(sy) WITH TEXT-e06.

          ELSEIF vg_sent_to_all IS INITIAL.

            "MESSAGE e002(sy) WITH TEXT-e06.

          ENDIF.

        CATCH cx_bcs INTO lo_bcs_exception.

      ENDTRY.


    ENDLOOP.

  ENDIF.


ENDFORM.
