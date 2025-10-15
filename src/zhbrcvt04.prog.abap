*----------------------------------------------------------------------*
*   INCLUDE HBRCVT04         " General subroutines
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       FORM INIT
*----------------------------------------------------------------------*
FORM init.

*  IF tree = 'X'.
*    p_mode = 1.
*  ENDIF.
*  IF list = 'X'.
*    p_mode = 2.
*  ENDIF.
*      PERFORM append_error USING pernr-pernr
*                                       'W'
*                                       '012'
*                                       pernr-pernr
*                                       space
*                                       space
*                                       space.



ENDFORM.                               " INIT

*----------------------------------------------------------------------*
*       FORM COLLECT_STEP_1
*----------------------------------------------------------------------*
*Description: Calculates the amounts of each transportation that the *
*employee will take during the period
*Last changes: macro call RP-EDIT-NAME replaced by function call
*RP_EDIT_NAME.
*Last modified: 01.11.1999
*Last modified by: Mario Sancho Graca
*----------------------------------------------------------------------*
FORM collect_step_1.

   DATA: days TYPE i,                   "Number of working days
         ause TYPE i,
         feri TYPE i,
         valtt LIKE pc207-betrg,        "Transportation value
         w_nbtrp TYPE i,                "Number of trips
         par_begda TYPE d,
         par_endda TYPE d,
         proc_begda TYPE d,
         proc_endda TYPE d,
  employee_name_formatted(40),
  return_code LIKE sy-subrc,
  l_btrtl_text LIKE t001p-btext.


  DATA R_SUBTY TYPE RANGE OF SUBTY.
  DATA l_subty TYPE SUBTY.

  CLEAR l_subty.

  SELECT *
    FROM tvarvc
    INTO @DATA(wTvarvc)
    WHERE name EQ 'ZHRBN_VT_CITY' OR name EQ 'ZHRBN_VT_BR' OR name EQ 'ZHRBN_VT_PIRAC'.

    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO r_subty.

  ENDSELECT.


  "rp-provide-from-last p0001 space pn-begda pn-endda.
  "rp-provide-from-last p0002 space pn-begda pn-endda.
  "rp-provide-from-last p0465 '0001' pn-begda pn-endda.

  IF MES EQ 'X'.
    proc_begda = pn-endda + 1.

    CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
      EXPORTING
        IM_DATE                =  proc_begda
      IMPORTING
        EX_LAST_DAY_OF_MONTH   = proc_endda.
  ELSE.
    proc_begda = pn-begda.
    proc_endda = pn-endda.
  ENDIF.


  CLEAR w_p0410_valid.
  PROVIDE * FROM p0410 BETWEEN proc_begda AND proc_endda.
    IF p0410-subty IN r_subty.
      CLEAR coll_tab.
      w_p0410_valid = 'X'.
      par_begda = proc_begda.
      par_endda = proc_endda.
*  // IT410 Proportion dates handling


      IF p0410-begda BETWEEN proc_begda AND proc_endda.
        par_begda = p0410-begda.
      ELSE.
        par_begda = proc_begda.
      ENDIF.

      IF p0410-endda BETWEEN proc_begda AND proc_endda.
        par_endda = p0410-endda.
      ELSE.
        par_endda = proc_endda.
      ENDIF.

      PERFORM number_days USING par_begda par_endda days ause feri.


      PERFORM re7brtr USING p0410-trans par_begda par_endda valtt days.
*  // Divides factor by days - proportion.
      valtt = valtt / days.


      MOVE-CORRESPONDING p0001  TO coll_tab.
      MOVE-CORRESPONDING p0002  TO coll_tab.
      MOVE-CORRESPONDING p0410  TO coll_tab.
      MOVE pernr-pernr          TO coll_tab-pernr.

      MOVE  p0410-SUBTY TO coll_tab-CODAQ.

      MOVE p0465-cpf_nr TO coll_tab-cpf.

      coll_tab-days = days.
      coll_tab-ause = ause.
      coll_tab-feri = feri.

      IF i7brtr-perio <> '1'.          " transp.ticket is not daily
        IF days > 0.         " work schedule of the period has at least
          " one working day
          days = 1.          " one monthly ticket will be provided for
          " each trip recorded in p0410-tviag
        ELSE.
          days = 0.          " no working days in the period = no
          " monthly ticket will be necessary
        ENDIF.
      ENDIF.

      w_nbtrp = p0410-tviag * days.
      coll_tab-valtt_tot = valtt * w_nbtrp.
      coll_tab-nbtrp = w_nbtrp.
      coll_tab-valtt = valtt.

*   Format Name
      PERFORM re001p USING coll_tab-werks coll_tab-btrtl
                     CHANGING l_btrtl_text.

      CALL FUNCTION 'RP_EDIT_NAME'
        EXPORTING
          format    = employee_name_format
          langu     = sy-langu
          molga     = t001p-molga
          pp0002    = p0002
        IMPORTING
          edit_name = employee_name_formatted
          retcode   = return_code.

      IF return_code <> 0.
        PERFORM append_error USING pernr-pernr
                                         'W'
                                         '012'
                                         pernr-pernr
                                         space
                                         space
                                         space.
      ENDIF.

      coll_tab-ename = employee_name_formatted.

      PERFORM re7brtp USING    p0410-trans
                      CHANGING q0410-tdesc.
      MOVE-CORRESPONDING q0410 TO coll_tab.

      APPEND coll_tab.

      w_index = sy-tabix.    " need to know the last record of this
      " employee, to be updated later, with
      " the totals
    ENDIF.
  ENDPROVIDE.

ENDFORM.                               " COLLECT_STEP_1

*----------------------------------------------------------------------*
*       FORM COLLECT_STEP_2
*----------------------------------------------------------------------*
*  Calculates total of transportation by employee and the amounts that
*  will be payed by the employee and by the employer
*  (The difference between total_tickets and real_disc is the amount
*  that the employer will pay)
*  This values are only a prevision, since the employer can input
*  manually new occurrences of infotype 15 in the database.
*  In the payroll run of the next month, all these informations will
*  be taken into account, to calculate the real values
*  (values generated now + values that will be inputed manually
*  until the run of the next report)
*----------------------------------------------------------------------*
FORM collect_step_2.

  DATA: BEGIN OF wpbp OCCURS 1.
          INCLUDE STRUCTURE pc205.
  DATA: END OF wpbp.

  DATA: BEGIN OF it OCCURS 100.
          INCLUDE STRUCTURE pc207.
  DATA: END OF it.

  DATA: BEGIN OF tab_values OCCURS 1,
          bukrs         LIKE pernr-bukrs,
          werks         LIKE pernr-werks,
          btrtl         LIKE pernr-btrtl,
          pernr         LIKE pernr-pernr,
          salary        LIKE pc207-betrg,    " contractual  or actual
                                             " salary
          waers         LIKE t500c-waers,
          percentage    LIKE t7br1b-trded,   " percentage the employee
                                             " will pay for the tickets
          total_tickets LIKE pc207-betrg,    " total value of tickets
          max_disc_perm LIKE pc207-betrg,    " maximum discount
                                             " permitted = salary * %
          real_disc    LIKE pc207-betrg,     " real discount (total of
                                             " tickets or % over salary)
        END OF tab_values.

  DATA lt_t7br1b TYPE STANDARD TABLE OF t7br1b.
  DATA ls_t7br1b TYPE t7br1b.
* get currency in table t500c
  PERFORM get_currency   USING    p0001-werks
                         CHANGING t500c-waers.

* the contractual salary will be taken into account for the
* calculation of transportation ticket discount
  CALL FUNCTION 'HR_BR_CONTRACTUAL_SALARY'
    EXPORTING
      begda         = pn-endda
      endda         = pn-endda
      perno         = pernr-pernr
      fc_sw_dec     = 'X'
      calc_currency = t500c-waers
    TABLES
      pp0001        = p0001
      pp0007        = p0007
      pp0008        = p0008
      it            = it.

  IF sy-subrc <> 0.
    PERFORM append_error USING pernr-pernr
                               'E'
                               '013'
                               pernr-pernr
                               space
                               space
                               space.
  ENDIF.

  CLEAR tab_values.

  MOVE t500c-waers TO tab_values-waers.
  MOVE-CORRESPONDING p0001 TO tab_values.
  MOVE-CORRESPONDING p0002 TO tab_values.
  MOVE pernr-pernr TO tab_values-pernr.

* contractual salary hourly & monthly empl.

*  rp-provide-from-last p0000 space pn-begda pn-endda.
*   RP-PROVIDE-FROM-LAST P0001 SPACE PN-BEGDA PN-ENDDA.
  MOVE-CORRESPONDING p0001 TO wpbp.
  MOVE-CORRESPONDING p0007 TO wpbp.
  MOVE-CORRESPONDING p0008 TO wpbp.
  MOVE-CORRESPONDING p0000 TO wpbp.
  PERFORM re503 USING p0001-persg p0001-persk.
  wpbp-abart = t503-abart.
  APPEND wpbp.

*    loop at wpbp.
*    endloop.

  LOOP AT it.
    IF wpbp-abart = '1'.
      tab_values-salary = tab_values-salary + it-betrg * p0007-mostd.
    ELSE.
      ADD it-betrg TO tab_values-salary.
    ENDIF.
  ENDLOOP.

* read union table to get the percentage that the employee will pay
* for the tickets
*  rp-provide-from-last p0057 space pn-begda pn-endda.

* check if there is an IT57 for this employee ??
*   Same logic used in the Payroll Function BRVTR (include PCVTRBR0)
    IF pnp-sw-found = '1'.
      CALL FUNCTION 'HR_BR_READ_T7BR1B'
        EXPORTING
          emfsl                 = p0057-emfsl
          begda                 = p0057-begda
          endda                 = p0057-endda
        TABLES
          out_t7br1b            = lt_t7br1b[]
        EXCEPTIONS
          no_entry_found_t7br1b = 1
          table_t7br1b_empty    = 2
          pass_exception        = 3
          OTHERS                = 4.
    ENDIF.

    IF SY-SUBRC <> 0 OR lt_t7br1b[] IS INITIAL.
    WRITE p0057-begda TO beg_dat DD/MM/YY.
    WRITE p0057-endda TO end_dat DD/MM/YY.
    PERFORM append_error USING space
                               'E'
                               '014'
                               p0057-emfsl
                               beg_dat
                               end_dat
                               space.
    ELSE.
      CLEAR ls_t7br1b.
      SORT lt_t7br1b[] BY begda DESCENDING.
      READ TABLE lt_t7br1b[] INDEX 1 INTO ls_t7br1b.
      MOVE ls_t7br1b-trded TO tab_values-percentage.
  ENDIF.



  LOOP AT coll_tab
    WHERE bukrs = tab_values-bukrs
    AND   werks = tab_values-werks
    AND   btrtl = tab_values-btrtl
    AND   pernr = tab_values-pernr.
    ADD coll_tab-valtt_tot TO tab_values-total_tickets.
  ENDLOOP.

  tab_values-max_disc_perm = tab_values-percentage *
                             tab_values-salary / 100.

* the maximum discount that the employee can have concerning the
* transportation tickets will be the percentage applyed over his/her
* salary
  IF tab_values-total_tickets > tab_values-max_disc_perm.
    tab_values-real_disc = tab_values-max_disc_perm.
  ELSE.
    tab_values-real_disc = tab_values-total_tickets.
  ENDIF.

  APPEND tab_values.

* update table coll_tab with the calculated values (only the last
* record of the current employee)
  READ TABLE coll_tab INDEX w_index.
  CLEAR coll_tab-inf15_exs.
  PERFORM check_it15_exists.
  MOVE tab_values-salary        TO coll_tab-salary.
  MOVE tab_values-waers         TO coll_tab-waers.
  MOVE tab_values-percentage    TO coll_tab-percentage.
  MOVE tab_values-total_tickets TO coll_tab-total_tickets.
  MOVE tab_values-max_disc_perm TO coll_tab-max_disc_perm.
  MOVE tab_values-real_disc     TO coll_tab-real_disc.
  MODIFY coll_tab INDEX w_index.

ENDFORM.                  " COLLECT_STEP_2

*----------------------------------------------------------------------*
*       Form  CHECK_IT15_EXISTS
*----------------------------------------------------------------------*
*       Check if already exists some entry of wage type MVTR in
*       infotype 15 for this period
*----------------------------------------------------------------------*
FORM check_it15_exists.

*  rp-provide-from-last p0015 space pn-begda pn-endda.
* loop at p0015.   " as the dates above are not working, had to be
*                  " substitued by loop at p0015 where dates...
  LOOP AT p0015
    WHERE subty = '9002'
    AND   begda >= pn-begda
    AND   endda <= pn-endda.
*   if p0015-subty = 'MVTR'.
    ADD 1 TO coll_tab-inf15_exs.
*   endif.
  ENDLOOP.

ENDFORM.                                   " CHECK_IT15_EXISTS


*&---------------------------------------------------------------------*
*&      Form  UPDATE_INFOTYPE15
*&---------------------------------------------------------------------*
*       Updates infotype 15 with the total amount of transportation
*       tickets that the the employee will receive. This information
*       will be used in the payroll run of the next month, to calculate
*       the amounts that will be payed by the employee and the
*       employer
*----------------------------------------------------------------------*
FORM update_infotype15.

  DATA: tab_lines TYPE i VALUE 0.

  IF NOT bti_15 IS INITIAL.
    CLEAR inf0015. REFRESH inf0015.

    LOOP AT coll_tab.
      CLEAR inf0015.
      MOVE coll_tab-pernr         TO inf0015-pernr.
      MOVE '9002'                 TO inf0015-subty.
      MOVE pn-endda               TO inf0015-begda.
      MOVE pn-endda               TO inf0015-endda.
      MOVE coll_tab-total_tickets TO inf0015-betrg.
      MOVE coll_tab-waers         TO inf0015-waers.
      MOVE coll_tab-inf15_exs     TO inf0015-inf15_exs.
      AT END OF pernr.
        APPEND inf0015.
      ENDAT.
    ENDLOOP.

    DESCRIBE TABLE inf0015 LINES tab_lines.

    CHECK tab_lines > 0.

*   refresh bdcdata.
    PERFORM open_batch_input.

    LOOP AT inf0015.

      IF NOT inf0015-inf15_exs IS INITIAL.
        WRITE inf0015-begda TO beg_dat DD/MM/YY.
        WRITE inf0015-endda TO end_dat DD/MM/YY.
        PERFORM append_error USING inf0015-pernr
                                   'W'
                                   '020'
                                   inf0015-pernr
                                   inf0015-inf15_exs
                                   beg_dat
                                   end_dat.
        PERFORM append_error USING space
                                   'W'
                                   '021'
                                   inf0015-pernr
                                   space
                                   space
                                   space.
      ENDIF.

      REFRESH bdcdata.
      PERFORM batch_input_table.
      PERFORM insert_batch_input.

    ENDLOOP.

*     perform open_batch_input.
*     perform insert_batch_input.
    PERFORM close_batch_input.

  ENDIF.

ENDFORM.                    " UPDATE_INFOTYPE15

*----------------------------------------------------------------------*
*       Form  PROGRESS_INDICATOR                                       *
*----------------------------------------------------------------------*
FORM progress_indicator USING  value(text).

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = text
    EXCEPTIONS
      OTHERS = 0.

ENDFORM.                               " PROGRESS_INDICATOR

*---------------------------------------------------------------------*
*       FORM RE503                                                    *
*---------------------------------------------------------------------*
*  -->  $PERSG    Personengruppe                                      *
*       $PERSK    Personenkreis                                       *
*---------------------------------------------------------------------*
FORM re503 USING
            $persg       LIKE p0001-persg
            $persk       LIKE p0001-persk.

  DATA: key(5) TYPE c.

  CHECK t503-persg NE $persg OR t503-persk NE $persk.
  SELECT SINGLE * FROM t503 WHERE persg EQ $persg
                            AND   persk EQ $persk.
  IF sy-subrc NE 0.
    PERFORM append_error USING space
                             'W'
                             '023'
                             $persg
                             $persk
                             space
                             space.
*    write: / text-p13, '503 ', $persg, $persk.   // usability
  ENDIF.
ENDFORM.                                                    "RE503

*---------------------------------------------------------------------*
*       FORM verify_hire_fire                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM verify_hire_fire USING    p_pernr_pernr LIKE pernr-pernr
                               p_pnpbegda    LIKE pnpbegda
                               p_pnpendda    LIKE pnpendda
                      CHANGING p_hire_date   TYPE d
                               p_fire_date   TYPE d.


  CALL FUNCTION 'HR_ENTRY_DATE'
    EXPORTING
      persnr               = p_pernr_pernr
      endda                = p_pnpendda
    IMPORTING
      entrydate            = p_hire_date
    EXCEPTIONS
      entry_date_not_found = 1
      OTHERS               = 2.

  CALL FUNCTION 'HR_LEAVING_DATE'
    EXPORTING
      persnr                 = pernr-pernr
      begda                  = p_hire_date
      endda                  = p_pnpendda
    IMPORTING
      leavingdate            = p_fire_date
    EXCEPTIONS
      leaving_date_not_found = 1
      OTHERS                 = 2.

ENDFORM.                                           "verify_hire_fire


*---------------------------------------------------------------------*
*       FORM zf_get_path                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_get_path CHANGING ch_path TYPE localfile.

  DATA: l_field  TYPE tabname,
        l_title  TYPE string,
        l_folder TYPE string.

  l_title = sy-title.

  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = l_title
    CHANGING
      selected_folder      = l_folder
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ch_path = l_folder.

ENDFORM.                                          "zf_get_path


*---------------------------------------------------------------------*
*       FORM zf_fill_file_city                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_fill_file_city.

  CLEAR:  layout_hd_city[],
          layout_dt_city[].

  DATA l_dt LIKE LINE OF layout_dt_city.
  DATA l_hd LIKE LINE OF layout_hd_city.
  DATA l_cpf TYPE char20.


  DATA R_SUBTY TYPE RANGE OF SUBTY.
  DATA l_subty TYPE SUBTY.

  CLEAR l_subty.

  SELECT *
    FROM tvarvc
    INTO @DATA(wTvarvc)
    WHERE name EQ 'ZHRBN_VT_CITY'.

    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO r_subty.

  ENDSELECT.


  CLEAR: layout_dt_city,
         layout_hd_city.

  l_hd-versao = '0800'.
  APPEND l_hd TO layout_hd_city.

  LOOP AT coll_tab INTO DATA(l_item) where CODAQ in r_subty.

    CLEAR l_cpf.

    l_cpf = l_item-cpf.
    REPLACE ALL OCCURENCES OF '.'  IN l_cpf WITH ''.
    REPLACE ALL OCCURENCES OF '-'  IN l_cpf WITH ''.

    l_dt-cpf        = l_cpf.
    l_dt-quant_dias = '1'.
    l_dt-valor_vt   = l_item-valtt_tot.
    l_dt-nome_func  = l_item-ename.
    l_dt-dsgn       = '04'.
    l_dt-aplic      = '400'.

    REPLACE ALL OCCURENCES OF '.'  IN l_dt-valor_vt WITH ''.
    REPLACE ALL OCCURENCES OF ','  IN l_dt-valor_vt WITH ''.

    TRANSLATE l_dt-nome_func to UPPER CASE.

    APPEND l_dt TO layout_dt_city.

  ENDLOOP.

ENDFORM.                        "zf_fill_file_city.


*---------------------------------------------------------------------*
*       FORM zf_fill_file_br                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_fill_file_br.

  CLEAR:  layout_dt_br[],
          layout_hd_br[].

  DATA l_dt LIKE LINE OF layout_dt_br.
  DATA l_hd LIKE LINE OF layout_hd_br.
  DATA l_cpf TYPE char20.



  DATA R_SUBTY TYPE RANGE OF SUBTY.
  DATA l_subty TYPE SUBTY.

  CLEAR l_subty.

  SELECT *
    FROM tvarvc
    INTO @DATA(wTvarvc)
    WHERE name EQ 'ZHRBN_VT_BR'.

    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO r_subty.

  ENDSELECT.



  l_hd-versao = '0200'.
  APPEND l_hd TO layout_hd_br.

  LOOP AT coll_tab INTO DATA(l_item) where CODAQ in r_subty.

    CLEAR l_cpf.

    l_cpf = l_item-cpf.
    REPLACE ALL OCCURENCES OF '.'  IN l_cpf WITH ''.
    REPLACE ALL OCCURENCES OF '-'  IN l_cpf WITH ''.

    l_dt-cpf        = l_cpf.
    l_dt-quant_dias = '1'.
    l_dt-valor_vt   = l_item-valtt_tot.
    l_dt-nome_func  = l_item-ename.

    REPLACE ALL OCCURENCES OF '.'  IN l_dt-valor_vt WITH ''.
    REPLACE ALL OCCURENCES OF ','  IN l_dt-valor_vt WITH ''.

    TRANSLATE l_dt-nome_func to UPPER CASE.

    APPEND l_dt TO layout_dt_br.

  ENDLOOP.

ENDFORM.                        "zf_fill_file_br.

*---------------------------------------------------------------------*
*       FORM zf_fill_file_pir                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_fill_file_pir.

  CLEAR:  layout_dt_pir[],
          layout_hd_pir[].

  DATA l_dt LIKE LINE OF layout_dt_pir.
  DATA l_hd LIKE LINE OF layout_hd_pir.
  DATA l_cpf TYPE char20.


  DATA R_SUBTY TYPE RANGE OF SUBTY.
  DATA l_subty TYPE SUBTY.

  CLEAR l_subty.

  SELECT *
    FROM tvarvc
    INTO @DATA(wTvarvc)
    WHERE name EQ 'ZHRBN_VT_PIRAC'.

    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO r_subty.

  ENDSELECT.




  l_hd-versao = '0200'.
  APPEND l_hd TO layout_hd_pir.

  LOOP AT coll_tab INTO DATA(l_item) where CODAQ in r_subty.

    CLEAR l_cpf.

    l_cpf = l_item-cpf.
    REPLACE ALL OCCURENCES OF '.'  IN l_cpf WITH ''.
    REPLACE ALL OCCURENCES OF '-'  IN l_cpf WITH ''.

    l_dt-cpf        = l_cpf.
    l_dt-quant_dias = '1'.
    l_dt-valor_vt   = l_item-valtt_tot.
    l_dt-nome_func  = l_item-ename.

    REPLACE ALL OCCURENCES OF '.'  IN l_dt-valor_vt WITH ''.
    REPLACE ALL OCCURENCES OF ','  IN l_dt-valor_vt WITH ''.

    TRANSLATE l_dt-nome_func to UPPER CASE.

    APPEND l_dt TO layout_dt_pir.

  ENDLOOP.

ENDFORM.                        "zf_fill_file_pir.

*---------------------------------------------------------------------*
*       FORM zf_download_file_city                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_download_file_city USING im_path TYPE localfile.

  DATA: i_tab_converted_data TYPE truxs_t_text_data,
        t_header             TYPE truxs_t_text_data,
        l_file_name          TYPE localfile,
        l_path               TYPE string,
        ch_file              TYPE truxs_t_text_data.


  DESCRIBE TABLE layout_dt_city LINES data(Linhas).

  CHECK Linhas gt 0.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
  EXPORTING
    i_field_seperator    = '|'
    " i_line_header      = 'x'
    i_filename           = im_path
  TABLES
    i_tab_sap_data       = layout_hd_city
  CHANGING
    i_tab_converted_data = t_header
  EXCEPTIONS
    conversion_failed    = '4'.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
  EXPORTING
    i_field_seperator    = '|'
    " i_line_header      = 'x'
    i_filename           = im_path
  TABLES
    i_tab_sap_data       = layout_dt_city
  CHANGING
    i_tab_converted_data = i_tab_converted_data
  EXCEPTIONS
    conversion_failed    = '4'.

  INSERT LINES OF t_header INTO i_tab_converted_data INDEX 1.

  CONCATENATE im_path '\' 'VT_CITY_' pn-begda+4(2) pn-begda(4) '.TXT' INTO l_file_name.
  l_path = l_file_name.

  ch_file = i_tab_converted_data[].

  CHECK ( NOT l_file_name IS INITIAL )
    AND ( NOT ch_file[] IS INITIAL ).

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_path
      filetype                = 'ASC'
    TABLES
      data_tab                = ch_file
    EXCEPTIONS
      file_write_error        = 1
      invalid_type            = 2
      no_batch                = 3
      unknown_error           = 4
      gui_refuse_filetransfer = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    "MESSAGE text-002 TYPE 'S'.
  ENDIF.

ENDFORM.                        "zf_download_file_city.

*---------------------------------------------------------------------*
*       FORM zf_download_file_br                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_download_file_br USING im_path TYPE localfile.

  DATA: i_tab_converted_data TYPE truxs_t_text_data,
        t_header             TYPE truxs_t_text_data,
        l_file_name          TYPE localfile,
        l_path               TYPE string,
        ch_file              TYPE truxs_t_text_data.



  DESCRIBE TABLE layout_dt_br LINES data(Linhas).

  CHECK Linhas gt 0.


  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
  EXPORTING
    i_field_seperator    = '|'
    " i_line_header      = 'x'
    i_filename           = im_path
  TABLES
    i_tab_sap_data       = layout_hd_br
  CHANGING
    i_tab_converted_data = t_header
  EXCEPTIONS
    conversion_failed    = '4'.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
  EXPORTING
    i_field_seperator    = '|'
    " i_line_header      = 'x'
    i_filename           = im_path
  TABLES
    i_tab_sap_data       = layout_dt_br
  CHANGING
    i_tab_converted_data = i_tab_converted_data
  EXCEPTIONS
    conversion_failed    = '4'.

  INSERT LINES OF t_header INTO i_tab_converted_data INDEX 1.

  CONCATENATE im_path '\' 'VT_BR_' pn-begda+4(2) pn-begda(4) '.TXT' INTO l_file_name.
  l_path = l_file_name.

  ch_file = i_tab_converted_data[].

  CHECK ( NOT l_file_name IS INITIAL )
    AND ( NOT ch_file[] IS INITIAL ).

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_path
      filetype                = 'ASC'
    TABLES
      data_tab                = ch_file
    EXCEPTIONS
      file_write_error        = 1
      invalid_type            = 2
      no_batch                = 3
      unknown_error           = 4
      gui_refuse_filetransfer = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    "MESSAGE text-002 TYPE 'S'.
  ENDIF.

ENDFORM.                        "zf_download_file_br.

*---------------------------------------------------------------------*
*       FORM zf_download_file_pir                                     *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_download_file_pir USING im_path TYPE localfile.

  DATA: i_tab_converted_data TYPE truxs_t_text_data,
        t_header             TYPE truxs_t_text_data,
        l_file_name          TYPE localfile,
        l_path               TYPE string,
        ch_file              TYPE truxs_t_text_data.


  DESCRIBE TABLE layout_dt_pir LINES data(Linhas).

  CHECK Linhas gt 0.


  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
  EXPORTING
    i_field_seperator    = '|'
    " i_line_header      = 'x'
    i_filename           = im_path
  TABLES
    i_tab_sap_data       = layout_hd_pir
  CHANGING
    i_tab_converted_data = t_header
  EXCEPTIONS
    conversion_failed    = '4'.

  CALL FUNCTION 'SAP_CONVERT_TO_TEX_FORMAT'
  EXPORTING
    i_field_seperator    = '|'
    " i_line_header      = 'x'
    i_filename           = im_path
  TABLES
    i_tab_sap_data       = layout_dt_pir
  CHANGING
    i_tab_converted_data = i_tab_converted_data
  EXCEPTIONS
    conversion_failed    = '4'.

  INSERT LINES OF t_header INTO i_tab_converted_data INDEX 1.

  CONCATENATE im_path '\' 'VT_Piracicabana_' pn-begda+4(2) pn-begda(4) '.TXT' INTO l_file_name.
  l_path = l_file_name.

  ch_file = i_tab_converted_data[].

  CHECK ( NOT l_file_name IS INITIAL )
    AND ( NOT ch_file[] IS INITIAL ).

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = l_path
      filetype                = 'ASC'
    TABLES
      data_tab                = ch_file
    EXCEPTIONS
      file_write_error        = 1
      invalid_type            = 2
      no_batch                = 3
      unknown_error           = 4
      gui_refuse_filetransfer = 5
      OTHERS                  = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    "MESSAGE text-002 TYPE 'S'.
  ENDIF.

ENDFORM.                        "zf_download_file_pir.

FORM GET_TVARVC CHANGING P_BURKS TYPE BUKRS .

    CONSTANTS: c_type TYPE tvarvc-type VALUE 'P'.
    DATA lv_result TYPE tvarvc-low.


    SELECT SINGLE low INTO lv_result
           FROM tvarvc
           WHERE  name EQ 'Z_HR_EMP_VAVR'
             AND  type EQ c_type.

    P_BURKS = lv_result.


ENDFORM.
