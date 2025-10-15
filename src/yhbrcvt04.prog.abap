*&---------------------------------------------------------------------*
*&  Include           YHBRCVT04
*&---------------------------------------------------------------------*
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

FORM collect_step_1.

  DATA: days                        TYPE i,                   "Number of working days
        ause                        TYPE i,
        feri                        TYPE i,
        valtt                       LIKE pc207-betrg,        "Transportation value
        w_nbtrp                     TYPE i,                "Number of trips
        par_begda                   TYPE d,
        par_endda                   TYPE d,
        proc_begda                  TYPE d,
        proc_endda                  TYPE d,
        tempensao                   TYPE c,
        employee_name_formatted(40),
        return_code                 LIKE sy-subrc,
        l_btrtl_text                LIKE t001p-btext.


  DATA r_subty TYPE RANGE OF subty.
  DATA l_subty TYPE subty.
  DATA dia TYPE i.
  DATA diames TYPE i.

  CLEAR l_subty.


  diames = pn-endda+6(2).

  IF adm EQ 'X'.
* BUG - 103946 - CBRAND - Inicio.
    dia = l_hire_date+6(2).
    days = diames - dia.
* BUG - 103946 - CBRAND - Fim.
  ELSE.
* BUG - 103946 - CBRAND - Inicio.
    "IF l_fire_date GT pn-begda AND l_fire_date LT pn-endda.
    IF l_fire_date GE pn-begda AND l_fire_date LE pn-endda.
      dia = l_fire_date+6(2).
* BUG - 103946 - CBRAND - Fim.
      days = dia.
    ELSE.
      days = diames.
    ENDIF.
  ENDIF.

  rp-provide-from-last p0001 space pn-begda pn-endda.
  rp-provide-from-last p0002 space pn-begda pn-endda.
  rp-provide-from-last p0465 '0001' pn-begda pn-endda.

  IF mes EQ 'X'.
    proc_begda = pn-endda + 1.

    CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
      EXPORTING
        im_date              = proc_begda
      IMPORTING
        ex_last_day_of_month = proc_endda.
  ELSE.
    proc_begda = pn-begda.
    proc_endda = pn-endda.
  ENDIF.

  LOOP AT p0736 WHERE endda EQ hr_high_date AND iosts EQ '1'.
  ENDLOOP.
  IF sy-subrc EQ 0.
    tempensao = 'X'.

    SELECT SINGLE pernr
      FROM zhcmt_bn_0007
      INTO @DATA(lva_pernr)
        WHERE pernr = @pernr-pernr.
    IF sy-subrc <> 0.
      CLEAR tempensao.
    ENDIF.
  ENDIF.

  PROVIDE * FROM p0377 BETWEEN proc_begda AND proc_endda.

    LOOP AT p0021 WHERE subty EQ '12'.
      coll_tab-mae = p0021-fcnam.
    ENDLOOP.

    coll_tab-days = days.
    coll_tab-ause = ause.
    coll_tab-feri = feri.

    MOVE-CORRESPONDING p0001  TO coll_tab.
    MOVE-CORRESPONDING p0002  TO coll_tab.

    MOVE p0465-cpf_nr TO coll_tab-cpf.

    DATA no_msg       TYPE sy-msgty VALUE 'N'.
    DATA subrc        LIKE sy-subrc.
    DATA error_table  LIKE rpbenerr OCCURS 0.
    DATA ee_benefit_data     LIKE rpbeneedat.
    DATA lq0377        TYPE q0377.

    ee_benefit_data-pernr = p0377-pernr.
    ee_benefit_data-barea = p0377-barea = p0171-barea.
    ee_benefit_data-bengr = p0377-barea = p0171-bengr.
    ee_benefit_data-bstat = p0377-barea = p0171-bstat.

    CALL FUNCTION 'HR_BEN_GET_PLAN_COST'
      EXPORTING
        ee_benefit_data = ee_benefit_data
        bplan           = p0377-bplan
        bcost           = p0377-levl1 "h74fl-bcost
        datum           = sy-datum
        cover           = p0377-covov
        out_period      = '01' "q0377-eeper
        out_curre       = 'BRL' "q0377-curr5
        reaction        = no_msg
      IMPORTING
        eecst           = lq0377-eecst
        ercst           = lq0377-ercst
        accst           = lq0377-accst
        flxcr           = lq0377-flxcr
        subrc           = subrc
      TABLES
        error_table     = error_table.

**** BUG - 109183 - CBRAND - Inicio
    DELETE p2001 WHERE subty NOT IN absences.
*----------------------------------------
* Verifica se está com ausências (acidente de trabalho,
* auxílio doença e licença maternidade)
*----------------------------------------
    IF p2001[] IS NOT INITIAL.

      DATA: l_dias_trab TYPE i,
            l_valor_vr  TYPE zhrst_de_vr_p.

*---Se ausência começou antes do início e
*---termina depois do fim (Está afastado)
      LOOP AT p2001 INTO DATA(w_2001) WHERE   begda < pn-begda
                                        AND   endda > pn-endda.
      ENDLOOP.
      IF sy-subrc = 0.
        l_afastado = 'X'.
      ENDIF.

*---Se ausência começou depois do início e
*---termina depois do fim (Afastou esse mês )
      LOOP AT p2001 INTO w_2001 WHERE begda > pn-begda
                                   AND   endda > pn-endda.
      ENDLOOP.
      IF sy-subrc = 0.
        days = ( w_2001-begda - pn-begda ).
      ENDIF.

*---Se ausência começou antes do início e
*---termina antes do fim (Retornou esse mês )
      LOOP AT p2001 INTO w_2001 WHERE begda < pn-begda
                                   AND endda < pn-endda.
      ENDLOOP.
      IF sy-subrc = 0.
        days = ( pn-endda - w_2001-endda ).
      ENDIF.

*---Se ausência começou depois do início e
*---termina antes do fim (Afast. dentro do mês)
      LOOP AT p2001 INTO w_2001 WHERE  begda > pn-begda
                                   AND   endda < pn-endda.
      ENDLOOP.
      IF sy-subrc = 0.
        days = ( w_2001-begda  - pn-begda )
                    + ( pn-endda      - w_2001-endda ).
      ENDIF.
    ENDIF.

* --- Regra para o afastamento DOENÇA
    SELECT *
      FROM pa2001
      INTO TABLE @DATA(lit_pa2001)
        WHERE pernr = @pernr-pernr
         AND begda <= @pn-endda
         AND endda >= @pn-begda
         AND subty = '0210'.

    IF sy-subrc = 0.

* --- Retorno Afastamento. ( Recebe proporcional )
      READ TABLE lit_pa2001 INTO DATA(lwa_2001) INDEX 1.
      IF  ( lwa_2001-endda <= pn-endda ) AND ( lwa_2001-endda >= pn-begda ).
        DATA(v_dia_final) = |{ lwa_2001-endda+6(2) }|.
        days = ( diames - v_dia_final ).
      ENDIF.

* --- Afastado no mês. ( Recebe normal ) - Não retornou no mes
      IF  ( ( ( lwa_2001-begda >=  pn-begda  ) AND ( lwa_2001-begda <= pn-endda ) ) AND lwa_2001-endda > pn-endda ) .
        days = diames.
      ENDIF.

* --- Afastado com mais de 90 dias ( Não recebe )
      IF  ( ( ( lwa_2001-begda <  pn-begda  ) AND ( lwa_2001-endda <= pn-endda ) ) AND lwa_2001-endda > pn-endda ) .
        CLEAR days.

        days = lwa_2001-begda - pn-begda.
        IF days >= 90.
          l_afastado = 'X'.
        ELSE.
          days = diames.
        ENDIF.
      ENDIF.

      IF  ( lwa_2001-begda <  pn-begda  ) AND ( lwa_2001-endda > pn-endda ).
        CLEAR days.
        days =  pn-begda - lwa_2001-begda.
        IF days >= 90.
          l_afastado = 'X'.
        ELSE.
          days = diames.
        ENDIF.
      ENDIF.
    ENDIF.



**** BUG - 109183 - CBRAND - fim


    IF l_afastado EQ 'X'.
      lq0377-accst = 0.
    ENDIF.

    IF tempensao EQ 'X'.
      coll_tab-alim = lq0377-ercst / 2.
      coll_tab-lanche = lq0377-accst / 2.
      coll_tab-total = coll_tab-alim + coll_tab-lanche.
    ELSE.
      coll_tab-alim = lq0377-ercst.
      coll_tab-lanche = lq0377-accst.
      coll_tab-total = lq0377-ercst + lq0377-accst.
    ENDIF.

    IF days LT diames.
      coll_tab-alim = coll_tab-alim / diames *  days.
      coll_tab-lanche = coll_tab-lanche / diames *  days.
      coll_tab-total = coll_tab-total / diames *  days.
    ENDIF.


* BUG - 103946 - CBRAND - Inicio
*    IF adm EQ 'X'.
*      IF tempensao EQ 'X'.
*        coll_tab-lanche =  ( lq0377-accst / 2 ).
*        coll_tab-alim = coll_tab-alim + ( lq0377-ercst / 2 ).
*      ELSE.
*        coll_tab-lanche = lq0377-accst.
*        coll_tab-alim = coll_tab-alim + lq0377-ercst.
*      ENDIF.
*    ENDIF.
* BUG - 103946 - CBRAND - Fim




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

*      PERFORM re7brtp USING    p0410-trans
*                      CHANGING q0410-tdesc.
*      MOVE-CORRESPONDING q0410 TO coll_tab.

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

    coll_tab-entrydate   = l_hire_date.
    coll_tab-leavingdate = l_fire_date.
    coll_tab-kostl       = p0001-kostl.

    IF l_afastado IS INITIAL.
      APPEND coll_tab.
    ENDIF.
    IF tempensao EQ 'X'.
      MOVE-CORRESPONDING p0736  TO coll_tab.
      "coll_tab-alim = lQ0377-ercst / 2.
      "coll_tab-lanche = lQ0377-accst / 2.
      "coll_tab-total = coll_tab-alim + coll_tab-lanche.
      coll_tab-endereco = p0736-stras .
      coll_tab-ename = p0736-emftx.
      employee_name_format = p0736-emftx.
      coll_tab-cpf = p0736-cpfnr.
      coll_tab-numero =  p0736-hsnmr.
      coll_tab-cep =  p0736-pstlz .
      coll_tab-municipio =  p0736-ort01.
      coll_tab-bairro =  p0736-ort02.
      coll_tab-estado =  p0736-state.
      APPEND coll_tab.
    ENDIF.
  ENDPROVIDE.




ENDFORM.



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
          real_disc     LIKE pc207-betrg,     " real discount (total of
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

  IF sy-subrc <> 0 OR lt_t7br1b[] IS INITIAL.
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



*  LOOP AT coll_tab
*    WHERE bukrs = tab_values-bukrs
*    AND   werks = tab_values-werks
*    AND   btrtl = tab_values-btrtl
*    AND   pernr = tab_values-pernr.
*    ADD coll_tab-valtt_tot TO tab_values-total_tickets.
*  ENDLOOP.

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
  "CLEAR coll_tab-inf15_exs.
  PERFORM check_it15_exists.
  "MOVE tab_values-salary        TO coll_tab-salary.
  "MOVE tab_values-waers         TO coll_tab-waers.
  "MOVE tab_values-percentage    TO coll_tab-percentage.
  "MOVE tab_values-total_tickets TO coll_tab-total_tickets.
  "MOVE tab_values-max_disc_perm TO coll_tab-max_disc_perm.
  "MOVE tab_values-real_disc     TO coll_tab-real_disc.
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
    WHERE subty = '9015'
    AND   begda >= pn-begda
    AND   endda <= pn-endda.
*   if p0015-subty = 'MVTR'.
    ADD 1 TO coll_tab-inf15_exs.
*   endif.
  ENDLOOP.

ENDFORM.                                       " CHECK_IT15_EXISTS


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
      MOVE '9015'                 TO inf0015-subty.
      MOVE pn-endda               TO inf0015-begda.
      MOVE pn-endda               TO inf0015-endda.
      MOVE coll_tab-total         TO inf0015-betrg.
      MOVE 'BRL'                  TO inf0015-waers.
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
FORM progress_indicator USING  VALUE(text).

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


"zf_fill_file_br.

*---------------------------------------------------------------------*
*       FORM zf_fill_file_pir                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_fill_file.

  DATA dataefpedido TYPE datum.
  DATA datacompet TYPE datum.
  DATA seq        TYPE i.
  DATA nomeinter  TYPE rvari_val_255.

  PERFORM get_tvarvc USING 'ZHRBNAL_INTER' CHANGING nomeinter.

  seq = 1.
  LOOP AT coll_tab INTO DATA(wdata)
                   GROUP BY ( cnpj = coll_tab-cnpj )
                ASCENDING
                ASSIGNING FIELD-SYMBOL(<group>).
    " Registro Header
    layout_hearder-tipo = '0'.
    layout_hearder-data_pedido = |{ sy-datum+6(2) }{ sy-datum+4(2) }{ sy-datum(4) }|.
    layout_hearder-canal = 'A001'.
    layout_hearder-razao_soc = wdata-rz_soc.
    layout_hearder-cnpj = wdata-cnpj.
    layout_hearder-cpf = '00000000000'.
    layout_hearder-contrato = '00013689821'.
    layout_hearder-num_pedido = '000000'.
    layout_hearder-data_efet = ''.

    dataefpedido = |{ sy-datum(6) }{ '07' }|.
    PERFORM dia_util CHANGING dataefpedido.

    layout_hearder-data_efet = dataefpedido.
    layout_hearder-codigo = '1'.
    layout_hearder-tipo_pedido = SWITCH #( mes
                                    WHEN 'X' THEN '1'
                                    ELSE '2').

    datacompet = pn-begda.
    layout_hearder-mes_comp = |{ datacompet+4(2) }{ datacompet(4) }|.
    layout_hearder-versao = '007'.
    layout_hearder-seq = seq.
    COLLECT layout_hearder.

    seq = seq + 1.

    " Registro da filial
    layout_filial-tipo = '1'.
    layout_filial-cnpj = wdata-cnpj.
    layout_filial-codigopj = '0000000000'.
    layout_filial-nomefilial = 'TGG'.
    layout_filial-ddd = '0000'.
    layout_filial-nomeinterl1 =  nomeinter.
    layout_filial-telefone1 = '000000000000'.
    layout_filial-ramal1 = '000000'.
    layout_filial-telefone2 = '000000000000'.
    layout_filial-ramal2 = '000000'.
    layout_filial-telefone3 = '000000000000'.
    layout_filial-ramal3 = '000000'.
    layout_filial-sequencia = seq.
    COLLECT layout_filial.
    seq = seq + 1.


    LOOP AT GROUP <group> INTO DATA(wgroup).
      layout_detalhe-tipo = '5'.
      layout_detalhe-valor = wdata-total.
      layout_detalhe-matricula = SWITCH #( wdata-pnalt
                                    WHEN '00000000' THEN wdata-pernr
                                    ELSE wdata-pnalt ).

      layout_detalhe-datanasc =  |{ p0002-gbdat+6(2) }{ p0002-gbdat+4(2) }{ p0002-gbdat(4) }|.
      layout_detalhe-cpf = wdata-cpf.
      layout_detalhe-pis = wdata-pis.
      layout_detalhe-sexo = wdata-sexo.
      layout_detalhe-estadocivil = wdata-estadocivil.
      layout_detalhe-endereco = wdata-endereco.
      layout_detalhe-complemento = wdata-complemento.
      layout_detalhe-numero = wdata-numero.
      layout_detalhe-cep = wdata-cep.
      layout_detalhe-municipio = wdata-municipio.
      layout_detalhe-bairro =  wdata-bairro.
      layout_detalhe-estado =  wdata-estado.
      layout_detalhe-nomemae =  wdata-mae.
      layout_detalhe-codopcaoend = 'R'.
      layout_detalhe-dddresid = '0000'.
      layout_detalhe-telefone  = '00000000'.
      layout_detalhe-ramal = '0000'.
      layout_detalhe-dddresid = '0000'.
      layout_detalhe-telresid = '00000000'.

      layout_detalhe-usu = wdata-ename.
      layout_detalhe-sequencia = seq.
      COLLECT layout_detalhe.
      seq = seq + 1.

    ENDLOOP.

  ENDLOOP.


*  CLEAR:  layout_dt_pir[],
*          layout_hd_pir[].
*
*  DATA l_dt LIKE LINE OF layout_dt_pir.
*  DATA l_hd LIKE LINE OF layout_hd_pir.
*  DATA l_cpf TYPE char20.
*
*
*  DATA R_SUBTY TYPE RANGE OF SUBTY.
*  DATA l_subty TYPE SUBTY.
*
*  CLEAR l_subty.
*
*  SELECT *
*    FROM tvarvc
*    INTO @DATA(wTvarvc)
*    WHERE name EQ 'ZHRBN_VT_PIRAC'.
*
*    APPEND VALUE #( sign = wTvarvc-sign OPTION = wTvarvc-OPTI LOW = wTvarvc-LOW high = wTvarvc-HIGH  ) TO r_subty.
*
*  ENDSELECT.
*
*
*
*
*  l_hd-versao = '0200'.
*  APPEND l_hd TO layout_hd_pir.
*
*  LOOP AT coll_tab INTO DATA(l_item).
*
*    CLEAR l_cpf.
*
*    l_cpf = l_item-cpf.
*    REPLACE ALL OCCURENCES OF '.'  IN l_cpf WITH ''.
*    REPLACE ALL OCCURENCES OF '-'  IN l_cpf WITH ''.
*
*    l_dt-cpf        = l_cpf.
*    l_dt-quant_dias = '1'.
*    "l_dt-valor_vt   = l_item-valtt_tot.
*    l_dt-nome_func  = l_item-ename.
*
*    REPLACE ALL OCCURENCES OF '.'  IN l_dt-valor_vt WITH ''.
*    REPLACE ALL OCCURENCES OF ','  IN l_dt-valor_vt WITH ''.
*
*    TRANSLATE l_dt-nome_func to UPPER CASE.
*
*    APPEND l_dt TO layout_dt_pir.
*
*  ENDLOOP.

ENDFORM.                        "zf_fill_file_pir.

*---------------------------------------------------------------------*
*       FORM zf_download_file_city                                         *
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM zf_download_file USING im_path TYPE localfile.

  DATA: i_tab_converted_data TYPE truxs_t_text_data,
        t_header             TYPE truxs_t_text_data,
        t_filial             TYPE truxs_t_text_data,
        t_detalhe            TYPE truxs_t_text_data,
        l_file_name          TYPE localfile,
        l_path               TYPE string,
        ch_file              TYPE truxs_t_text_data.


  DESCRIBE TABLE layout_hearder LINES DATA(linhas).

  CHECK linhas GT 0.

  SORT layout_detalhe BY sequencia.

  INSERT LINES OF layout_detalhe INTO i_tab_converted_data INDEX 1.
  INSERT LINES OF layout_filial INTO i_tab_converted_data INDEX 1.
  INSERT LINES OF layout_hearder INTO i_tab_converted_data INDEX 1.






  CONCATENATE im_path '\' 'Alimentacao_Alelo_' pn-begda+4(2) pn-begda(4) '.TXT' INTO l_file_name.
  l_path = l_file_name.

  ch_file = i_tab_converted_data[].

  CHECK ( NOT l_file_name IS INITIAL )
    AND ( NOT ch_file[] IS INITIAL ).
*
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



FORM  get_filial_data CHANGING pcoll LIKE coll_tab.

  DATA ls_p0001        TYPE p0001.
  DATA ls_comp_addr    TYPE addr1_val.
  DATA ls_t7brb1       TYPE t7brb1.
  DATA lv_bukrs        TYPE bapibranch-bukrs.
  DATA lv_branch       TYPE bapibranch-branch.
  DATA lv_obra         TYPE t7brc0-obra.
  DATA lv_cgc          TYPE pbr_num_cnpj.
  DATA lv_comp_name    TYPE name1.
  DATA lv_cei          TYPE t7brc0-cei.

* Read the current infotype 0001 to the current company
  ls_p0001 = p0001.

*--------------------------------------------------------------------*
* If not found, then it is necessary to create it
*--------------------------------------------------------------------*

* Read the Branch
  CALL FUNCTION 'HR_BR_GET_FILIAL_PER_AREA'
    EXPORTING
      p_werks        = ls_p0001-werks
      p_btrtl        = ls_p0001-btrtl
    IMPORTING
      bukrs          = lv_bukrs
      branch         = lv_branch
      obra           = lv_obra
    EXCEPTIONS
      no_link_areas  = 1
      no_group_found = 2
      OTHERS         = 3.


* Read the information from the Branch
  CALL FUNCTION 'HR_BR_LER_FILIAL_GERAL'
    EXPORTING
      company_code           = lv_bukrs
      branch                 = lv_branch
      obra                   = lv_obra
      date                   = sy-datum
    IMPORTING
      cgc                    = lv_cgc
      cei                    = lv_cei
      comp_name              = lv_comp_name
      comp_addr              = ls_comp_addr
    EXCEPTIONS
      branch_not_found       = 1
      address_not_found      = 2
      company_not_found      = 3
      general_data_not_found = 4
      OTHERS                 = 5.



**--------------------------------------------------------------------*
** Structure key
**--------------------------------------------------------------------*
*  ls_company_data-werks = ls_p0001-werks.
*  ls_company_data-btrtl = ls_p0001-btrtl.
*
**--------------------------------------------------------------------*
** Company Data
**--------------------------------------------------------------------*
*  IF lv_cgc IS INITIAL.
*    ls_company_data-cnpj_cei = lv_cei.
*  ELSE.
*    ls_company_data-cnpj_cei = lv_cgc.
*  ENDIF.
*
*  ls_company_data-comp_name = lv_comp_name.
*
**--------------------------------------------------------------------*
** Address
**--------------------------------------------------------------------*
*  CONCATENATE ls_comp_addr-street ls_comp_addr-house_num1
*              ls_comp_addr-floor ls_comp_addr-roomnumber
*         INTO ls_company_data-comp_addr
* SEPARATED BY space.
*
*  ls_company_data-comp_district = ls_comp_addr-city2.
*  ls_company_data-comp_city     = ls_comp_addr-city1.
*  ls_company_data-comp_state    = ls_comp_addr-region.
*  ls_company_data-comp_zip_code = ls_comp_addr-post_code1.
*
**--------------------------------------------------------------------*
** CNAE
**--------------------------------------------------------------------*
*  SELECT SINGLE * FROM t7brb1 INTO ls_t7brb1
*          WHERE bukrs  = lv_bukrs
*            AND filia  = lv_branch
*            AND begda <= mv_endda
*            AND endda >= mv_endda. "#EC *
*
*  IF sy-subrc EQ 0.
*    SELECT SINGLE econi FROM t7brae INTO ls_company_data-cnae
*            WHERE econa  = ls_t7brb1-econa
*              AND begda <= mv_endda
*              AND endda >= mv_endda. "#EC *
*  ENDIF.
*
**--------------------------------------------------------------------*
** CNPJ Tomador
**--------------------------------------------------------------------*
*  ls_company_data-cnpj_toma = mv_cnpj_tomador.
*
*  INSERT ls_company_data INTO TABLE mt_company_data[].



ENDFORM.

FORM dia_util CHANGING data TYPE datum.
  DATA workday TYPE c.
  workday = ''.

  WHILE workday EQ ''.

    CALL FUNCTION 'DATE_CHECK_WORKINGDAY'
      EXPORTING
        date                       = data
        factory_calendar_id        = 'BR'
        message_type               = 'E'
      EXCEPTIONS
        date_after_range           = 1
        date_before_range          = 2
        date_invalid               = 3
        date_no_workingday         = 4
        factory_calendar_not_found = 5
        message_type_invalid       = 6
        OTHERS                     = 7.
    IF sy-subrc EQ 0.
      workday = 'X'.
    ELSE.
      data = data - 1.
    ENDIF.

  ENDWHILE.


ENDFORM.

FORM get_tvarvc USING const TYPE rvari_vnam CHANGING retorno TYPE rvari_val_255.

  SELECT *
    FROM tvarvc
    INTO @DATA(wtvarvc)
    WHERE name EQ @const.

    retorno = wtvarvc-low.


  ENDSELECT.


ENDFORM.


FORM get_tvarvc_bukrs CHANGING p_burks TYPE bukrs .

  CONSTANTS: c_type TYPE tvarvc-type VALUE 'P'.
  DATA lv_result TYPE tvarvc-low.


  SELECT SINGLE low INTO lv_result
         FROM tvarvc
         WHERE  name EQ 'Z_HR_EMP_VAVR'
           AND  type EQ c_type.

  p_burks = lv_result.


ENDFORM.
