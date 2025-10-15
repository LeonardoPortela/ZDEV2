*&---------------------------------------------------------------------*
*& Report  ZMMR0033
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr0033.

TABLES: mseg, zmmt0133.

*---------------------------------------------------------------------*
* TYPES                                                               *
*---------------------------------------------------------------------*
TYPES: BEGIN OF tp_mseg,
         bukrs      TYPE mseg-bukrs,
         werks      TYPE mseg-werks,
         charg      TYPE mseg-charg,
         matnr      TYPE mseg-matnr,
         lifnr      TYPE mseg-lifnr,
         menge      TYPE mseg-menge,
         budat_mkpf TYPE mseg-budat_mkpf,
         monat      TYPE zmmt0133-monat,
         mjahr      TYPE mseg-mjahr,
         shkzg      TYPE mseg-shkzg,
       END OF tp_mseg.


*---------------------------------------------------------------------*
* TABLES                                                              *
*---------------------------------------------------------------------*
DATA: git_zmmt0133     TYPE TABLE OF zmmt0133,
      git_zmmt0133_aux TYPE TABLE OF zmmt0133,
      git_mseg         TYPE TABLE OF mseg,
      git_mseg_aux     TYPE TABLE OF tp_mseg,
      git_mseg_tot     TYPE TABLE OF tp_mseg,
      git_line         TYPE REF TO data,
      git_zficag       TYPE REF TO data.

*---------------------------------------------------------------------*
* WORKAREA                                                            *
*---------------------------------------------------------------------*
DATA: gwa_mseg         LIKE LINE OF git_mseg,
      gwa_mseg_aux     LIKE LINE OF git_mseg_aux,
      gwa_mseg_tot     LIKE LINE OF git_mseg_tot,
      gwa_zmmt0133     LIKE LINE OF git_zmmt0133,
      gwa_zmmt0133_aux LIKE LINE OF git_zmmt0133.

*---------------------------------------------------------------------*
* RANGES                                                              *
*---------------------------------------------------------------------*
RANGES: rg_budat_mkpf FOR mseg-budat_mkpf,
        rg_mjahr      FOR mseg-mjahr.

*---------------------------------------------------------------------*
* FIELD-SYMBOLS                                                       *
*---------------------------------------------------------------------*
FIELD-SYMBOLS: <gft_zficag> TYPE ANY TABLE,
               <gfs_zficag> TYPE any,
               <gfs_custom> TYPE any.

*---------------------------------------------------------------------*
* VARIAVEIS                                                           *
*---------------------------------------------------------------------*

DATA: gva_salk3   TYPE zmmt0133-custo_un_brl,
      gva_salk3_u TYPE zmmt0133-custo_un_brl,
      gva_lbkum   TYPE zmmt0133-custo_un_brl.


*---------------------------------------------------------------------*
* SELECTION SCREEN                                                    *
*---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_bukrs  FOR mseg-bukrs     NO INTERVALS, "NO-EXTENSION,
                s_werks  FOR mseg-werks     NO INTERVALS, "NO-EXTENSION,
                s_matnr  FOR mseg-matnr     NO INTERVALS, "NO-EXTENSION,
                s_lifnr  FOR mseg-lifnr     NO INTERVALS, "NO-EXTENSION,
                s_mjahr  FOR mseg-mjahr     NO INTERVALS NO-EXTENSION,
                s_monat  FOR zmmt0133-monat   NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_set_screen.

*---------------------------------------------------------------------*
* START-OF-SELECTION                                                  *
*---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fm_seleciona_dados.
  PERFORM fm_manipula_dados.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FM_SET_SCREEN
*&---------------------------------------------------------------------*
FORM fm_set_screen .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM fm_seleciona_dados .

  DATA: gva_data     TYPE sy-datum,
        gva_data_ini TYPE sy-datum,
        gva_data_fim TYPE sy-datum,
        lva_month    TYPE zmmt0133-monat,
        lva_year     TYPE zmmt0133-mjahr,
        lva_mjahr    TYPE mseg-mjahr.

* Se nenhum campo dos parâmetros foi preenchido então fazer o processo abaixo de seleção:

  IF s_bukrs IS INITIAL AND
     s_werks IS INITIAL AND
     s_matnr IS INITIAL AND
     s_lifnr IS INITIAL AND
     s_mjahr IS INITIAL AND
     s_monat IS INITIAL.

    SELECT *
    INTO TABLE git_zmmt0133_aux
    FROM zmmt0133 UP TO 1 ROWS
    ORDER BY monat mjahr ASCENDING.

    gva_data = sy-datum.

*Identifica qual foi o mês anterior
    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = -1
        olddate = gva_data
      IMPORTING
        newdate = gva_data_ini.

    CONCATENATE gva_data_ini+0(6) '01' INTO gva_data_ini.


    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = -1
        olddate = gva_data
      IMPORTING
        newdate = gva_data_fim.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gva_data_fim
      IMPORTING
        last_day_of_month = gva_data_fim.

* Monta range dde períodos
    REFRESH: rg_budat_mkpf, rg_mjahr.
    rg_budat_mkpf-sign = 'I'.
    rg_budat_mkpf-option = 'BT'.
    rg_budat_mkpf-low  =  gva_data_ini.
    rg_budat_mkpf-high =  gva_data_fim.
    APPEND  rg_budat_mkpf.

    rg_mjahr-sign = 'I'.
    rg_mjahr-option = 'BT'.
    rg_mjahr-low  =  gva_data_ini+0(4).
    rg_mjahr-high =  gva_data_fim+0(4).
    APPEND  rg_mjahr.
* Fim Range de períodos.

    SELECT * FROM mseg INTO TABLE git_mseg
     WHERE mjahr IN rg_mjahr
       AND budat_mkpf  IN rg_budat_mkpf
    AND lgort = ''.

  ELSE.

    IF s_mjahr-low IS INITIAL.
      MESSAGE 'O ano deve ser informado!' TYPE 'S'.
      STOP.
    ENDIF.

    IF s_monat-low IS INITIAL.
      MESSAGE 'O mês deve ser informado!' TYPE 'S'.
      STOP.
    ENDIF.

    CLEAR: gva_data_ini.
    CONCATENATE s_mjahr-low s_monat-low '01' INTO gva_data_ini.

*Identifica qual foi o mês anterior
    CLEAR: gva_data.
    CALL FUNCTION 'RE_ADD_MONTH_TO_DATE'
      EXPORTING
        months  = -1
        olddate = gva_data_ini
      IMPORTING
        newdate = gva_data.

    lva_month = gva_data+4(2).
    lva_mjahr = gva_data+0(4).

* Pego o mês anterior
    SELECT * FROM zmmt0133 INTO TABLE git_zmmt0133_aux
     WHERE mjahr       EQ lva_mjahr
       AND monat       EQ lva_month
       AND bukrs       IN s_bukrs
       AND werks       IN s_werks
       AND matnr       IN s_matnr
       AND lifnr       IN s_lifnr.


    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = gva_data_ini
      IMPORTING
        last_day_of_month = gva_data_fim.

    REFRESH: rg_budat_mkpf.
    rg_budat_mkpf-sign = 'I'.
    rg_budat_mkpf-option = 'BT'.
    rg_budat_mkpf-low  =  gva_data_ini.
    rg_budat_mkpf-high =  gva_data_fim.
    APPEND  rg_budat_mkpf.

    SELECT * FROM mseg INTO TABLE git_mseg
     WHERE mjahr       IN s_mjahr
       AND budat_mkpf  IN rg_budat_mkpf
       AND lgort       EQ ''
       AND bukrs       IN s_bukrs
       AND werks       IN s_werks
       AND matnr       IN s_matnr
       AND lifnr       IN s_lifnr.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_MANIPULA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_manipula_dados .

  DATA: lit_data TYPE REF TO data.
  FIELD-SYMBOLS: <fs_data>  TYPE table,
                 <lwa_data> TYPE any.

  DATA  : lit_selection TYPE TABLE OF rsparams.
  DATA  : lwa_selection LIKE LINE OF lit_selection.

  SORT: git_mseg BY bukrs werks charg mjahr matnr lifnr.

  IF git_mseg IS NOT INITIAL.


    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-tabix
        text       = 'Processando Dados.'.


    LOOP AT git_mseg INTO gwa_mseg.

      gwa_mseg_aux-bukrs      =  gwa_mseg-bukrs.
      gwa_mseg_aux-werks      =  gwa_mseg-werks.
      gwa_mseg_aux-charg      =  gwa_mseg-charg.
      gwa_mseg_aux-matnr      =  gwa_mseg-matnr.
      gwa_mseg_aux-lifnr      =  gwa_mseg-lifnr.
      gwa_mseg_aux-menge      =  gwa_mseg-menge.
      gwa_mseg_aux-budat_mkpf =  gwa_mseg-budat_mkpf.
      gwa_mseg_aux-mjahr      =  gwa_mseg-budat_mkpf+0(4).
      gwa_mseg_aux-monat      =  gwa_mseg-budat_mkpf+4(2).
      gwa_mseg_aux-shkzg      =  gwa_mseg-shkzg.

      APPEND gwa_mseg_aux TO git_mseg_aux.
      CLEAR: gwa_mseg_aux.

    ENDLOOP.

    SORT: git_mseg_aux BY bukrs werks charg mjahr monat matnr lifnr.

    git_mseg_tot = git_mseg_aux.

    DELETE ADJACENT DUPLICATES FROM git_mseg_tot
    COMPARING bukrs werks charg mjahr monat matnr lifnr.

    LOOP AT git_mseg_tot INTO gwa_mseg_tot.

      LOOP AT git_mseg_aux INTO gwa_mseg_aux WHERE bukrs = gwa_mseg_tot-bukrs AND
                                                   werks = gwa_mseg_tot-werks AND
                                                   charg = gwa_mseg_tot-charg AND
                                                   matnr = gwa_mseg_tot-matnr AND
                                                   lifnr = gwa_mseg_tot-lifnr AND
                                                   mjahr = gwa_mseg_tot-mjahr AND
                                                   monat = gwa_mseg_tot-monat.

        IF gwa_mseg_aux-shkzg = 'H'.
          gwa_zmmt0133-menge = gwa_zmmt0133-menge  + ( gwa_mseg_aux-menge * -1 ).
        ELSE.
          gwa_zmmt0133-menge = gwa_zmmt0133-menge + gwa_mseg_aux-menge.
        ENDIF.

      ENDLOOP.

      gwa_zmmt0133-mjahr   = gwa_mseg_tot-mjahr.
      gwa_zmmt0133-monat   = gwa_mseg_tot-monat.
      gwa_zmmt0133-bukrs   = gwa_mseg_tot-bukrs.
      gwa_zmmt0133-werks   = gwa_mseg_tot-werks.
      gwa_zmmt0133-charg   = gwa_mseg_tot-charg.
      gwa_zmmt0133-matnr   = gwa_mseg_tot-matnr.
      gwa_zmmt0133-lifnr   = gwa_mseg_tot-lifnr.

      CLEAR: gwa_zmmt0133_aux.
      READ TABLE git_zmmt0133_aux INTO gwa_zmmt0133_aux WITH KEY bukrs = gwa_zmmt0133-bukrs
                                                                 werks = gwa_zmmt0133-werks
                                                                 charg = gwa_zmmt0133-charg
                                                                 matnr = gwa_zmmt0133-matnr
                                                                 lifnr = gwa_zmmt0133-lifnr.

      IF sy-subrc = 0.
        gwa_zmmt0133-menge = gwa_zmmt0133-menge + gwa_zmmt0133_aux-menge.
      ENDIF.

      gwa_zmmt0133-dt_anul = sy-datum.
      gwa_zmmt0133-hr_anul = sy-uzeit.

      lwa_selection-selname = 'R_MATNR'.
      lwa_selection-kind    = 'S'.
      lwa_selection-sign    = 'I'.
      lwa_selection-option  = 'EQ'.
      lwa_selection-low     = gwa_zmmt0133-matnr.
      APPEND lwa_selection TO lit_selection.

      lwa_selection-selname = 'S_WERKS'.
      lwa_selection-kind    = 'S'.
      lwa_selection-sign    = 'I'.
      lwa_selection-option  = 'EQ'.
      lwa_selection-low     = gwa_zmmt0133-werks.
      APPEND lwa_selection TO lit_selection.


      lwa_selection-selname = 'P_POPER'.
      lwa_selection-kind    = 'S'.
      lwa_selection-sign    = 'I'.
      lwa_selection-option  = 'EQ'.
      lwa_selection-low     = gwa_zmmt0133-monat.
      APPEND lwa_selection TO lit_selection.

      lwa_selection-selname = 'P_BDATJ'.
      lwa_selection-kind    = 'S'.
      lwa_selection-sign    = 'I'.
      lwa_selection-option  = 'EQ'.
      lwa_selection-low     = gwa_zmmt0133-mjahr.
      APPEND lwa_selection TO lit_selection.

      lwa_selection-selname = 'R_ST_D'.
      lwa_selection-kind    = 'P'.
      lwa_selection-low     = 'X'.
      APPEND lwa_selection TO lit_selection.


      cl_salv_bs_runtime_info=>set(
      display = abap_false
      metadata = abap_false
      data = abap_true ).

      SUBMIT zcor011 WITH SELECTION-TABLE lit_selection AND RETURN .

      TRY.
          cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING r_data = git_zficag  ).

          IF git_zficag IS NOT INITIAL.
            ASSIGN git_zficag->* TO <gft_zficag>.

          ENDIF.
        CATCH cx_salv_bs_sc_runtime_info.
          MESSAGE 'Unable to retrieve ALV data' TYPE 'E'.
      ENDTRY.

      IF <gft_zficag> IS NOT INITIAL.

        CLEAR: gva_salk3,
               gva_salk3_u,
               gva_lbkum.

        CREATE DATA git_line LIKE LINE OF <gft_zficag>.
        ASSIGN git_line->* TO <gfs_zficag>.

        LOOP AT <gft_zficag> ASSIGNING <gfs_zficag>.
          ASSIGN COMPONENT 'SALK3' OF STRUCTURE <gfs_zficag> TO <gfs_custom>.
          IF sy-subrc EQ 0.
            gva_salk3 = <gfs_custom>.
          ENDIF.
          ASSIGN COMPONENT 'SALK3_U' OF STRUCTURE <gfs_zficag> TO <gfs_custom>.
          IF sy-subrc EQ 0.
            gva_salk3_u = <gfs_custom>.
          ENDIF.
          ASSIGN COMPONENT 'LBKUM' OF STRUCTURE <gfs_zficag> TO <gfs_custom>.
          IF sy-subrc EQ 0.
            gva_lbkum = <gfs_custom>.
          ENDIF.
        ENDLOOP.
      ENDIF.

      gwa_zmmt0133-custo_un_brl = gva_salk3  / gva_lbkum.
      gwa_zmmt0133-custo_un_usd = gva_salk3_u  / gva_lbkum.

      APPEND gwa_zmmt0133 TO git_zmmt0133.

      CLEAR:  gwa_zmmt0133, gwa_mseg_aux.

    ENDLOOP.

    IF git_zmmt0133 IS NOT INITIAL.
      LOOP AT git_zmmt0133 INTO gwa_zmmt0133.
        MODIFY zmmt0133 FROM gwa_zmmt0133.
        COMMIT WORK.
        CLEAR: gwa_zmmt0133.
      ENDLOOP.
    ENDIF.

    MESSAGE 'Dados Processados com Sucesso' TYPE 'S'.

  ELSE.
    MESSAGE 'Dados não encontrados' TYPE 'S'.
  ENDIF.

ENDFORM.
