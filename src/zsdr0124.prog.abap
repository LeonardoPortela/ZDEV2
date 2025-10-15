*&---------------------------------------------------------------------*
*& Report  ZSDR0124
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0124.


DATA: BEGIN OF tg_entrada OCCURS 0,
        docnum                 TYPE j_1bnflin-docnum,
        itmnum                 TYPE j_1bnflin-itmnum,
        credat                 TYPE j_1bnfdoc-credat,
        docdat                 TYPE j_1bnfdoc-docdat,
        model                  TYPE j_1bnfdoc-model,
        series                 TYPE j_1bnfdoc-series,
        nfnum                  TYPE j_1bnfdoc-nfnum,
        bukrs                  TYPE j_1bnfdoc-bukrs,
        branch                 TYPE j_1bnfdoc-branch,
        parid                  TYPE j_1bnfdoc-parid,
        nfenum                 TYPE j_1bnfdoc-nfenum,
        matnr                  TYPE j_1bnflin-matnr,
        cfop                   TYPE j_1bnflin-cfop,
        nbm                    TYPE j_1bnflin-nbm,
        menge                  TYPE j_1bnflin-menge,
        meins                  TYPE j_1bnflin-meins,
        entrad                 TYPE j_1bnfdoc-entrad,
        name1                  TYPE lfa1-name1,
        regio                  TYPE lfa1-regio,
        maktx                  TYPE makt-maktx,
        dt_recepcao_cct        TYPE zlest0146-dt_recepcao,
        peso_aferido_recepcao  TYPE zlest0146-peso_aferido_recepcao,
        peso_cct               TYPE zlest0146-peso_aferido_recepcao,
        peso_fiscal_cct        TYPE zlest0146-peso_aferido_recepcao,
        term_cct               TYPE zsdt0168-lifnr,
        ds_term_cct            TYPE lfa1-name1,
        dif_peso_cct_nf        TYPE zlest0146-peso_aferido_recepcao,
        conf_cct_portal        TYPE c LENGTH 4,
        dt_recepcao_portal     TYPE zlest0186-dt_recepcao,
        term_cct_portal        TYPE zsdt0168-lifnr,
        ds_term_cct_portal     TYPE lfa1-name1,
        rom_completo           TYPE c LENGTH 4,
        tp_nf_rem              TYPE znom_reme_notas-tp_nf_rem,
        docnum_rfl             TYPE j_1bnfdoc-docnum,
        nfenum_rfl             TYPE j_1bnfdoc-nfenum,
        cct_rfl                TYPE c LENGTH 4,
        peso_cct_rfl           TYPE zlest0146-peso_aferido_recepcao,
        dt_cct_rfl             TYPE zlest0146-dt_recepcao,
        term_cct_rfl           TYPE zsdt0168-lifnr,
        ds_term_cct_rfl        TYPE lfa1-name1,
        terminal_rfl           TYPE lfa1-lifnr,
        ds_terminal_rfl        TYPE lfa1-name1,
        chave_nfe_rfl          TYPE zib_nfe_dist_itm-chave_nfe,
        conf_cct_portal_rfl    TYPE c LENGTH 4,
        dt_recepcao_portal_rfl TYPE zlest0186-dt_recepcao,
        term_cct_portal_rfl    TYPE zsdt0168-lifnr,
        ds_term_cct_portal_rfl TYPE lfa1-name1,
        id_due                 TYPE zsdt0170-id_due,
        numero_due             TYPE zsdt0170-numero_due,
        chave_acesso           TYPE zsdt0170-chave_acesso,
        dt_due                 TYPE zsdt0170-dt_registro,
        qtde_vinc_due          TYPE zsdt0173-peso_liq_total,
        id_due_ret             TYPE zsdt0170-id_due,
        docnum_exp             TYPE j_1bnfdoc-docnum,
        nfenum_exp             TYPE j_1bnfdoc-nfenum,
        fatura_id              TYPE zsdt0172-fatura_id,
        navio                  TYPE znom_transporte-ds_nome_transpor,
        saldo_exportar         TYPE zsdt0173-peso_liq_total,
        chave_nfe              TYPE zib_nfe_dist_itm-chave_nfe,
        und_trib_xml           TYPE zib_nfe_dist_itm-prod_und_trib,
        ncm_xml                TYPE zib_nfe_dist_itm-prod_ncm,
        cfop_xml               TYPE zib_nfe_dist_itm-prod_cfop,
        restricao              TYPE c LENGTH 150.
DATA:  END OF tg_entrada.

TYPES: BEGIN OF ty_jbranch,
         bukrs  TYPE j_1bbranch-bukrs,
         branch TYPE j_1bbranch-branch,
       END OF ty_jbranch.

DATA: r_bukrs TYPE RANGE OF t001-bukrs,
      s_bukrs LIKE LINE OF r_bukrs.

DATA: git_zsdt0263 TYPE TABLE OF zsdt0263,
      gwa_zsdt0263 TYPE zsdt0263,
      git_jbranch  TYPE TABLE OF ty_jbranch,
      gwa_jbranch  TYPE ty_jbranch.

DATA: git_rsparams TYPE TABLE OF rsparams,
      gwa_rsparams TYPE rsparams.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.


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


  PERFORM: f_extracao_dados.




*&---------------------------------------------------------------------*
*&      Form  F_EXTRACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_extracao_dados .

  DATA: lv_erdat_l TYPE sy-datum,
        lv_erdat_h TYPE sy-datum,
        i_data     TYPE TABLE OF setleaf.

****Inicio ajuste BUG 69626 - Paulo Quevedo / 15.05.2023
*  s_bukrs-sign   = 'I'.
*  s_bukrs-option = 'EQ'.
*  s_bukrs-low    = '0001'.
*  APPEND s_bukrs TO r_bukrs.
*  s_bukrs-low    = '0015'.
*  APPEND s_bukrs TO r_bukrs.
*  CLEAR s_bukrs.

  FREE: i_data.
  SELECT * FROM setleaf INTO TABLE i_data WHERE setname EQ 'ZSDR0124_BUKRS'.
  IF i_data IS NOT INITIAL.
    r_bukrs = VALUE #( FOR l IN  i_data ( sign = 'I' option = 'EQ' low  = l-valfrom ) ).
  ENDIF.
****Fim ajuste BUG 69626 - Paulo Quevedo / 15.05.2023



  SELECT bukrs branch FROM j_1bbranch INTO TABLE git_jbranch
    WHERE bukrs IN r_bukrs.

  SORT git_jbranch BY bukrs branch ASCENDING.


  LOOP AT r_bukrs INTO s_bukrs.

    LOOP AT git_jbranch INTO gwa_jbranch
      WHERE bukrs EQ s_bukrs-low.

      CLEAR: gwa_zsdt0263, gwa_rsparams, lv_erdat_l, lv_erdat_h.
      REFRESH: git_rsparams[].

      gwa_rsparams-selname = 'P_BUKRS'.
      gwa_rsparams-kind    = 'S'.
      gwa_rsparams-sign    = 'I'.
      gwa_rsparams-option  = 'EQ'.
      gwa_rsparams-low     = s_bukrs-low.
      APPEND gwa_rsparams TO git_rsparams.
      CLEAR gwa_rsparams.

      gwa_rsparams-selname = 'P_BRANCH'.
      gwa_rsparams-kind    = 'S'.
      gwa_rsparams-sign    = 'I'.
      gwa_rsparams-option  = 'EQ'.
      gwa_rsparams-low     = gwa_jbranch-branch.
      APPEND gwa_rsparams TO git_rsparams.
      CLEAR gwa_rsparams.

      gwa_rsparams-selname = 'P_MATNR'.
      gwa_rsparams-kind    = 'S'.
      gwa_rsparams-sign    = 'I'.
      gwa_rsparams-option  = 'CP'.
      gwa_rsparams-low     = '*'.
      APPEND gwa_rsparams TO git_rsparams.
      CLEAR gwa_rsparams.

      lv_erdat_l = sy-datum - 365.
      lv_erdat_h = sy-datum.

      gwa_rsparams-selname = 'P_DOCDAT'.
      gwa_rsparams-kind    = 'S'.
      gwa_rsparams-sign    = 'I'.
      gwa_rsparams-option  = 'BT'.
      gwa_rsparams-low     = lv_erdat_l.
      gwa_rsparams-high    = lv_erdat_h.
      APPEND gwa_rsparams TO git_rsparams.
      CLEAR gwa_rsparams.

      PERFORM f_prepare_run_time_info.

      TRY.
          SUBMIT zsdr0100  WITH SELECTION-TABLE git_rsparams
*                           WITH P_SRVCCT EQ 'X'
                           WITH p_report EQ 'ZSDR0124' AND RETURN.
        CATCH cx_salv_bs_sc_runtime_info.
      ENDTRY.

      PERFORM f_get_runtime_info.

      IF <t_data> IS ASSIGNED.
        LOOP AT <t_data> ASSIGNING <w_data>.
          CLEAR: tg_entrada.
          MOVE-CORRESPONDING <w_data> TO tg_entrada.

          MOVE-CORRESPONDING tg_entrada TO gwa_zsdt0263.
          gwa_zsdt0263-dt_job = sy-datum.
          APPEND gwa_zsdt0263 TO git_zsdt0263.
        ENDLOOP.

        IF  git_zsdt0263[] IS NOT INITIAL.
          PERFORM f_save_dados USING s_bukrs-low gwa_jbranch-branch.
        ENDIF.

      ENDIF.

      CLEAR gwa_jbranch.
    ENDLOOP.

    CLEAR s_bukrs.
  ENDLOOP.


ENDFORM.


FORM f_prepare_run_time_info.

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


FORM f_get_runtime_info.

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
*&      Form  F_SAVE_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_save_dados USING p_bukrs p_branch.

  DELETE FROM zsdt0263 WHERE bukrs EQ p_bukrs
                         AND branch EQ p_branch.

  MODIFY zsdt0263 FROM TABLE git_zsdt0263.
  COMMIT WORK.

  REFRESH git_zsdt0263.

ENDFORM.
