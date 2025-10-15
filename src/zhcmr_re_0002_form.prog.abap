*======================================================================*
* PROJETO            : HCM                                             *
* PROGRAMA           : ZHCMR_RE_0002                                   *
* TRANSACAO          : ZHCM_RE0004                                     *
* DESCRICAO          : Relatório de Conferência Prêmio/Gratificação    *
*======================================================================*
* AUTOR              : Ronaldo Freitas                                 *
* Solicitante        : Bruna Macedo                                    *
* DATA               : 24.06.2024                                      *
*======================================================================*
*                      HISTORICO DE MUDANÇAS                           *
*======================================================================*
*   DATA   |  AUTOR   |   REQUEST   |           DESCRICAO              *
*======================================================================*
*&---------------------------------------------------------------------*
*&  Include           ZHCMR_RE_0001_FORM
*&---------------------------------------------------------------------*

FORM fm_selecao.

  DATA:
    lv_dat        TYPE sy-datum,
    lv_mes1       TYPE char02,
    lv_datma(6)   TYPE n,
    lv_datac(7)   TYPE c,
    v_ultimo_dia  TYPE sy-datum,
    v_ultimo_diac TYPE sy-datum,
    v_ultimo_diab TYPE hrp1000-begda,
    v_ultimo_diae TYPE hrp1000-endda.

  FIELD-SYMBOLS: <fs_saida2> TYPE ty_saida2.
  DATA: wa_saida2 TYPE ty_saida2,
        v_return  TYPE sy-subrc.

  DATA(lv_data) = lv_ano-low && lv_mes-low && '01'.
  DATA(lv_compm) = lv_mes-low.
  DATA(lv_compa) = lv_ano-low.

  SELECT * FROM hrp9666
    INTO TABLE @DATA(it_hrp9666)
    WHERE objid IN @p_plans
      AND plvar EQ '01'
      AND otype EQ 'S'
      AND endda GE @lv_data.

  SELECT domvalue_l, ddtext FROM dd07t
  INTO TABLE @DATA(it_dd07t)
  WHERE domname EQ 'ZTP_BENEF'.


  SELECT * FROM t503t
  INTO TABLE @DATA(it_t503t)
  WHERE sprsl EQ 'P'.

* Vamos usar o campo OBJID como base para montar a ALV de saída, (OBJID é posição).
  IF it_hrp9666 IS NOT INITIAL.
    SELECT * FROM pa0001
      INTO TABLE @DATA(it_pa0001)
      FOR ALL ENTRIES IN @it_hrp9666
      WHERE pernr IN @p_pernr
        AND plans EQ @it_hrp9666-objid
        AND bukrs IN @p_bukrs
        AND werks IN @p_werks
        AND kostl IN @p_kostl
        AND endda GE @sy-datum.
    IF sy-subrc IS INITIAL.

*Colocar restrição por objeto de acesso : objeto de acesso P_ORGIN
* Autorização area HCM
      LOOP AT it_pa0001 ASSIGNING FIELD-SYMBOL(<fs_pa0001>).
        PERFORM check_authorization USING
                                           'P_ORGIN'
                                            <fs_pa0001>-werks
                                            'PERSA'
                                    CHANGING
                                            v_return.
        IF ( v_return IS NOT INITIAL ).
          MESSAGE s836(sd) WITH TEXT-004 <fs_pa0001>-werks '.' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*Selecionar registros
  IF lv_mes-low EQ '01'.
    DATA: lv_anox(4) TYPE n.
    lv_anox = lv_ano-low - 1.
    lv_datma = lv_anox && '12'.
    lv_dat =  lv_datma && 01.
  ELSE.
    lv_datma = lv_ano-low && lv_mes-low.
    lv_datma = lv_datma - 1.
    lv_dat =  lv_datma && 01.
  ENDIF.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_dat
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  IF it_pa0001 IS NOT INITIAL.

    SELECT * FROM pa0001
      INTO TABLE @DATA(it_pa0001x)
      FOR ALL ENTRIES IN @it_pa0001
      WHERE pernr EQ @it_pa0001-pernr
        AND plans EQ @it_pa0001-plans
        AND bukrs EQ @it_pa0001-bukrs
        AND werks EQ @it_pa0001-werks
        AND kostl EQ @it_pa0001-kostl
        AND endda GE @v_ultimo_dia. "Ultima data mês anterior

    IF sy-subrc IS INITIAL AND it_pa0001x[] IS NOT INITIAL.
* Porém para os dados de saída, eu preciso da linha atual dessas pessoas, então com essa mesma lista que retornou acima , buscar a linha atual da pa0001
      DELETE it_pa0001x[] WHERE endda LE sy-datum.
    ENDIF.
  ENDIF.
* Buscar situação do empregado:
  IF it_pa0001x IS NOT INITIAL.
    SELECT * FROM pa0000 "stat2, descricao
      INTO TABLE @DATA(it_pa0000x)
      FOR ALL ENTRIES IN @it_pa0001x
      WHERE pernr EQ @it_pa0001x-pernr
        AND endda GE @sy-datum.
    IF sy-subrc IS INITIAL.
      SORT it_pa0000x BY pernr.

      SELECT * FROM t529u
        INTO TABLE @DATA(it_t529u)
        WHERE sprsl EQ @sy-langu
          AND statn EQ '2'.
      IF sy-subrc IS INITIAL.
        SORT it_t529u BY statv.
      ENDIF.
    ENDIF.
  ENDIF.

  CLEAR lv_dat.
  lv_dat = lv_ano-low && lv_mes-low && '01'.

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_dat
    IMPORTING
      last_day_of_month = v_ultimo_dia.

  IF it_pa0001 IS NOT INITIAL.
    SELECT * FROM hrp1000
      INTO TABLE @DATA(it_hrp1000)
      FOR ALL ENTRIES IN @it_pa0001
      WHERE plvar EQ '01'
        AND otype EQ 'C'
        AND langu EQ @sy-langu
        AND objid EQ @it_pa0001-stell
        AND begda LE @v_ultimo_dia
        AND endda GE @v_ultimo_dia.
  ENDIF.
*Buscar desc. da posição
  IF it_hrp9666 IS NOT INITIAL.
    v_ultimo_diab = v_ultimo_dia.
    v_ultimo_diae = v_ultimo_dia.
    SELECT * FROM hrp1000
      INTO TABLE @DATA(it_hrp1000x)
      FOR ALL ENTRIES IN @it_hrp9666
      WHERE plvar EQ '01'
        AND otype EQ 'S'
        AND langu EQ @sy-langu
        AND objid EQ @it_hrp9666-objid
        AND begda LE @v_ultimo_dia
        AND endda GE @v_ultimo_dia.
  ENDIF.

* Buscar salário
  IF it_pa0001 IS NOT INITIAL.
    SELECT * FROM pa0008
      INTO TABLE @DATA(it_pa0008)
      FOR ALL ENTRIES IN @it_pa0001
      WHERE pernr EQ @it_pa0001-pernr
        AND begda LE @v_ultimo_dia
        AND endda GE @v_ultimo_dia.
    IF sy-subrc IS INITIAL.

* Buscar data de admissão
      LOOP AT it_pa0008 INTO DATA(wa_pa0008).
        DATA(lv_tabix) = sy-tabix.
        CLEAR: wa_pa0008-aedtm.

        CALL FUNCTION 'HR_ENTRY_DATE'
          EXPORTING
            persnr    = wa_pa0008-pernr
*           begda     = wa_pa0008-begda
*           endda     = wa_pa0008-endda
          IMPORTING
            entrydate = wa_pa0008-aedtm. "saida

        IF wa_pa0008-aedtm IS NOT INITIAL OR wa_pa0008-aedtm NE '00000000'.
          MODIFY it_pa0008 FROM wa_pa0008 INDEX lv_tabix TRANSPORTING aedtm.
          CLEAR wa_pa0008.
        ENDIF.
      ENDLOOP.
    ENDIF.

*Buscar nome
    SELECT * FROM pa0002
      INTO TABLE @DATA(it_pa0002)
      FOR ALL ENTRIES IN @it_pa0001
      WHERE pernr EQ @it_pa0001-pernr
        AND endda GE @sy-datum.

* Buscar data fim contrato determinado
    SELECT * FROM pa0016
      INTO TABLE @DATA(it_pa0016)
      FOR ALL ENTRIES IN @it_pa0001
      WHERE pernr EQ @it_pa0001-pernr
        AND cttyp = '02'
        AND  endda GE @lv_data "ctedt entre o mês do parâmetro
        AND begda LE @v_ultimo_dia.
    IF sy-subrc IS INITIAL.
      SORT it_pa0016 BY ctedt.

      SELECT cttyp, cttxt FROM t547s
        INTO TABLE @DATA(it_t547s)
        FOR ALL ENTRIES IN @it_pa0016
        WHERE sprsl EQ 'P'
          AND cttyp EQ @it_pa0016-cttyp.

    ENDIF.

*Buscar advertência/suspensão
    SELECT * FROM zhcmt0004 "selecionar initm e fimtm
      INTO TABLE @DATA(it_zhcmt0004)
     WHERE anopr IN @p_ano    "parâmetro
       AND mespr IN @p_mes.   "parâmetro

*Ir na tabela pa0030 com

    IF it_zhcmt0004 IS NOT INITIAL.
      SELECT * FROM pa0030
        INTO TABLE @DATA(it_pa0030)
        FOR ALL ENTRIES IN @it_zhcmt0004
        WHERE pernr IN @p_pernr
          AND ( subty EQ '01'
          OR   subty EQ '10' )
          AND begda GE @it_zhcmt0004-initm
          AND endda LE @it_zhcmt0004-fimtm.
    ENDIF.
  ENDIF.

  IF it_pa0001 IS NOT INITIAL.

    IF lv_mes-low IS NOT INITIAL.
      DATA: lv_mest(2) TYPE n.
      lv_mest = lv_mes-low.
    ENDIF.

    DATA: lv_u_dia  TYPE endda,
          lv_ctedt  TYPE endda,
          lv_subty  TYPE subty,
          lv_betrg  TYPE pad_amt7s,
          lv_p_dia  TYPE begda,
          it_pa0015 TYPE STANDARD TABLE OF pa0015.

    lv_u_dia = lv_ano-low && lv_mest && '31'.
    lv_p_dia = lv_ano-low && lv_mest && '01'.

    lv_betrg = '1'.
    lv_subty = '9031'.

    SELECT * FROM pa0015
    INTO TABLE @it_pa0015
        FOR ALL ENTRIES IN @it_pa0001
        WHERE pernr EQ @it_pa0001-pernr
          AND subty EQ @lv_subty
          AND begda GE @lv_p_dia
          AND endda LE @lv_u_dia
          AND betrg EQ @lv_betrg.
  ENDIF.

* CLUSTER FOLHA
  DATA:
    cl_read_payroll TYPE REF TO cl_hr_br_read_payroll,
    tl_rgdir        TYPE TABLE OF pc261,
    wl_rgdir        TYPE pc261,
    tl_payroll      TYPE TABLE OF paybr_result,
    wl_payroll      TYPE          paybr_result,
    tl_rt           TYPE TABLE OF pc207,
    wl_rt           TYPE pc207.

  RANGES:
          rg_ocrsn FOR pc261-ocrsn.

  rg_ocrsn-sign   = 'I'.
  rg_ocrsn-option = 'EQ'.
  rg_ocrsn-low    = ''.
  APPEND rg_ocrsn.
  CLEAR rg_ocrsn.

  rg_ocrsn-sign   = 'I'.
  rg_ocrsn-option = 'EQ'.
  rg_ocrsn-low    = 'RESC'.
  APPEND rg_ocrsn.
  CLEAR rg_ocrsn.

  DATA: lv_cont  TYPE i,
        lv_falta TYPE i.

  CASE p_check.
    WHEN abap_false. "Opção 1

      LOOP AT it_pa0001 ASSIGNING <fs_pa0001>.
        APPEND INITIAL LINE TO it_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida1>).

        <fs_saida1>-bukrs      = <fs_pa0001>-bukrs.
        <fs_saida1>-cod_filial = <fs_pa0001>-werks.
        <fs_saida1>-matricula  = <fs_pa0001>-pernr.
        <fs_saida1>-persk      = <fs_pa0001>-persk.

        READ TABLE it_t503t INTO DATA(wa_t503t) WITH KEY persk = <fs_pa0001>-persk.
        IF sy-subrc IS INITIAL.
          <fs_saida1>-persk   = wa_t503t-persk && ' - ' &&  wa_t503t-ptext.
        ENDIF.

        READ TABLE it_pa0000x INTO DATA(wa_pa0000x) WITH KEY pernr = <fs_pa0001>-pernr
                                                    BINARY SEARCH.
        IF sy-subrc IS INITIAL.
*         Selecionar stat2 + descrição
          READ TABLE it_t529u INTO DATA(wa_t529u) WITH KEY statv = wa_pa0000x-stat2
                                                  BINARY SEARCH.
          <fs_saida1>-status = wa_pa0000x-stat2 && '-' && wa_t529u-text1.
        ENDIF.

        IF <fs_pa0001>-bukrs IS NOT INITIAL.
          CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
            EXPORTING
              bukrs      = <fs_pa0001>-bukrs
              langu      = sy-langu
            IMPORTING
              bukrs_text = <fs_saida1>-bukrs_text.
        ENDIF.

        IF <fs_pa0001>-werks IS NOT INITIAL.
          CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
            EXPORTING
              werks      = <fs_pa0001>-werks
            IMPORTING
              werks_text = <fs_saida1>-nome_filial.
        ENDIF.

        READ TABLE it_hrp1000 INTO DATA(wa_hrp1000) WITH KEY objid = <fs_pa0001>-stell.
        IF sy-subrc IS INITIAL.
          <fs_saida1>-cargo      = <fs_pa0001>-stell.
          <fs_saida1>-desc_cargo = wa_hrp1000-stext.
        ENDIF.

        READ TABLE it_hrp1000x INTO DATA(wa_hrp1000x) WITH KEY objid = <fs_pa0001>-plans.
        IF sy-subrc IS INITIAL.
*          <fs_saida1>-plans          = <fs_pa0001>-plans.
*          <fs_saida1>-des_pos_ant    = wa_hrp1000x-stext.
          <fs_saida1>-pos_atual          = <fs_pa0001>-plans.
          <fs_saida1>-desc_pos_atual     = wa_hrp1000x-stext.
        ENDIF.

        READ TABLE it_pa0008 INTO wa_pa0008 WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          <fs_saida1>-bet01  =  wa_pa0008-bet01.
          <fs_saida1>-edates =  wa_pa0008-aedtm+6(2) && '/' && wa_pa0008-aedtm+4(2) && '/' && wa_pa0008-aedtm(4). "entrydate
        ENDIF.

        READ TABLE it_pa0002 INTO DATA(wa_pa0002) WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          <fs_saida1>-cname  =  wa_pa0002-cname.
        ENDIF.

        CLEAR lv_ctedt.
        READ TABLE it_pa0016 INTO DATA(wa_pa0016) WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          <fs_saida1>-cttyp  =  wa_pa0016-cttyp.
          READ TABLE it_t547s INTO DATA(wa_t547s) WITH KEY cttyp = wa_pa0016-cttyp.
          IF sy-subrc IS INITIAL.
            <fs_saida1>-cttyp  =  wa_pa0016-cttyp && ' - ' && wa_t547s-cttxt.
          ENDIF.
          lv_ctedt = wa_pa0016-ctedt.
          <fs_saida1>-ctedt  =  wa_pa0016-ctedt+6(2) && '/' && wa_pa0016-ctedt+4(2) && '/' && wa_pa0016-ctedt(4).
        ENDIF.

* ADVERT  "*ADVERTENCIA/SUSPENSAO  Contador da pa0030 quantidade de linhas
        CLEAR lv_cont.
        LOOP AT it_pa0030 INTO DATA(wa_pa0030) WHERE pernr EQ <fs_pa0001>-pernr.
          lv_cont = lv_cont + 1.
        ENDLOOP.
        <fs_saida1>-advert  =  lv_cont.

        READ TABLE it_hrp9666 INTO DATA(wa_hrp9666) WITH KEY objid = <fs_pa0001>-plans.
        IF sy-subrc IS INITIAL.

          READ TABLE it_pa0001x INTO DATA(wa_pa0001x) WITH KEY plans = <fs_pa0001>-plans
                                                               bukrs = <fs_pa0001>-bukrs
                                                               werks = <fs_pa0001>-werks
                                                               kostl = <fs_pa0001>-kostl
                                                               endda = v_ultimo_dia.
          IF sy-subrc IS INITIAL.
            <fs_saida1>-movimenta = abap_true.
          ENDIF.

          <fs_saida1>-tp_p_atual   =  wa_hrp9666-ztpbenef.
          READ TABLE it_dd07t INTO DATA(wa_dd07t) WITH KEY domvalue_l = wa_hrp9666-ztpbenef.
          IF sy-subrc IS INITIAL.
            <fs_saida1>-tp_p_atual   = wa_dd07t-domvalue_l && ' - ' && wa_dd07t-ddtext.
          ENDIF.

          IF <fs_saida1>-tp_p_atual(2) EQ 'GR'.

            IF <fs_saida1>-bet01 IS NOT INITIAL AND wa_hrp9666-zperc IS NOT INITIAL.
              <fs_saida1>-vlr_p_atual = ( <fs_saida1>-bet01 * wa_hrp9666-zperc ) / 100.
            ENDIF.

          ELSE.

            IF wa_hrp9666-zvalor IS NOT INITIAL.
              <fs_saida1>-vlr_p_atual  =  wa_hrp9666-zvalor.
            ENDIF.

          ENDIF.

          <fs_saida1>-gratif = wa_hrp9666-zperc.

        ENDIF.

        READ TABLE it_pa0015 INTO DATA(wa_pa0015) WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          <fs_saida1>-vlr_p_z  = abap_true.
        ENDIF.

* Objeto
        CLEAR: cl_read_payroll.
        CREATE OBJECT cl_read_payroll
          EXPORTING
            iv_pernr = <fs_pa0001>-pernr.

* Lista de Clusters
        REFRESH: tl_rgdir.
        CALL METHOD cl_read_payroll->get_rgdir
          EXPORTING
            iv_begda = lv_dat
            iv_endda = v_ultimo_diae
          IMPORTING
            et_rgdir = tl_rgdir.

        CLEAR: wl_rgdir.

        DELETE tl_rgdir WHERE ocrsn NOT IN rg_ocrsn.

        REFRESH tl_payroll.
        CALL METHOD cl_read_payroll->get_pay_result_table
          EXPORTING
            it_rgdir        = tl_rgdir
          IMPORTING
            et_paybr_result = tl_payroll.

        REFRESH: tl_rt.

        CLEAR: wl_payroll.
        LOOP AT tl_payroll INTO wl_payroll.
          APPEND LINES OF wl_payroll-inter-rt TO tl_rt.
        ENDLOOP.

        CLEAR: lv_falta.

        LOOP AT tl_rt INTO wl_rt.

          IF wl_rt-lgart = '1231'.
            <fs_saida1>-acumul_avos = ( <fs_saida1>-acumul_avos ) + wl_rt-betpe.
            <fs_saida1>-acumul_vlr =  ( <fs_saida1>-acumul_vlr ) + ( ( wl_rt-betpe / 100000 ) * wl_rt-anzhl ).
          ENDIF.

          IF <fs_saida1>-tp_p_atual(2) EQ 'GR' AND wl_rt-lgart =  '1917'.
            <fs_saida1>-avos_p_folha = ( <fs_saida1>-avos_p_folha ) + wl_rt-anzhl.
            <fs_saida1>-vlr_p_folha =  ( <fs_saida1>-vlr_p_folha ) + wl_rt-betrg.
          ELSEIF wl_rt-lgart =  '1031'. "selecionar anzhl e betrg
            <fs_saida1>-avos_p_folha = ( <fs_saida1>-avos_p_folha ) + wl_rt-anzhl.
            <fs_saida1>-vlr_p_folha =  ( <fs_saida1>-vlr_p_folha ) + wl_rt-betrg.
          ENDIF.
          IF wl_rt-lgart =  '3032'. "faltas
            <fs_saida1>-falta = abap_true.
            lv_falta = lv_falta + 1.
          ENDIF.

        ENDLOOP.
*** BUG - 149504 - Inicio - CBRAND
*** Inicializo com zero
        <fs_saida1>-avos_p_m = 0.
        <fs_saida1>-vlr_p_m = 0.
*** BUG - 149504 - Fim - CBRAND

        IF lv_falta GE 1
         OR <fs_saida1>-vlr_p_z IS NOT INITIAL
         OR <fs_saida1>-advert GE 1
         OR lv_ctedt LT lv_p_dia. "lv_u_dia

          <fs_saida1>-avos_p_m = 0. "lv_ctedt+6(2).
          <fs_saida1>-vlr_p_m = 0."( ( <fs_saida1>-vlr_p_atual / 30 ) * ( lv_ctedt+6(2) ) ).
          DATA(lv_c) = abap_true.
        ELSE. " * BUG - 149504 - CBRAND
* Se data término contato det. < ultimo dia do mês do parâmetro
          IF lv_ctedt LT lv_u_dia.
*** BUG - 149504 - Inicio - CBRAND
            IF ( lv_ctedt+4(2) = lv_u_dia+4(2) ) AND ( lv_ctedt+0(4) = lv_u_dia+0(4) ).
*** BUG - 149504 - Fim - CBRAND
              <fs_saida1>-avos_p_m = lv_ctedt+6(2).
              <fs_saida1>-vlr_p_m = ( ( <fs_saida1>-vlr_p_atual / 30 ) * ( lv_ctedt+6(2) ) ).
              lv_c = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.

* Se não cair em nenhuma condição acima, valor integral
        IF lv_c IS INITIAL.
          <fs_saida1>-avos_p_m = 30.
          <fs_saida1>-vlr_p_m = <fs_saida1>-vlr_p_atual.
        ENDIF.
      ENDLOOP.

      IF <fs_saida1> IS ASSIGNED. "Bug 158063 - CBRAND
        IF <fs_saida1>-tp_p_atual(2) EQ 'PR'.
          <fs_saida1>-avos_p_m = 0.
          <fs_saida1>-vlr_p_m = 0.
        ENDIF.
      ENDIF.
*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Início de Alteração
*** Bug 158063 - CBRAND - Inicio
*      IF it_saida1 IS INITIAL.
*        MESSAGE 'Dados não encontrados.' TYPE 'I' DISPLAY LIKE 'E'.
*        CALL SELECTION-SCREEN 1000.
*      ENDIF.
*** Bug 158063 - CBRAND - Fim
*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Fim de Alteração

    WHEN abap_true.  "Opção ALV 2

      LOOP AT it_pa0001 ASSIGNING <fs_pa0001>.

        wa_saida2-bukrs      = <fs_pa0001>-bukrs.
        wa_saida2-cod_filial = <fs_pa0001>-werks.
        wa_saida2-matricula  = <fs_pa0001>-pernr.
        wa_saida2-persk      = <fs_pa0001>-persk.

        READ TABLE it_t503t INTO wa_t503t WITH KEY persk = <fs_pa0001>-persk.
        IF sy-subrc IS INITIAL.
          wa_saida2-persk   = wa_t503t-persk && ' - ' &&  wa_t503t-ptext.
        ENDIF.

        READ TABLE it_pa0000x INTO wa_pa0000x WITH KEY pernr = <fs_pa0001>-pernr
                                                    BINARY SEARCH.
        IF sy-subrc IS INITIAL.
*         Selecionar stat2 + descrição
          READ TABLE it_t529u INTO wa_t529u WITH KEY statv = wa_pa0000x-stat2
                                                  BINARY SEARCH.
          wa_saida2-status = wa_pa0000x-stat2 && ' - ' && wa_t529u-text1.
        ENDIF.

        IF wa_saida2-bukrs IS NOT INITIAL.
          CALL FUNCTION 'HRWPC_RFC_BUKRS_TEXT_GET'
            EXPORTING
              bukrs      = wa_saida2-bukrs
            IMPORTING
              bukrs_text = wa_saida2-bukrs_text.
        ENDIF.

        IF <fs_pa0001>-werks IS NOT INITIAL.
          CALL FUNCTION 'HRWPC_RFC_WERKS_TEXT_GET'
            EXPORTING
              werks      = <fs_pa0001>-werks
            IMPORTING
              werks_text = wa_saida2-nome_filial.
        ENDIF.

        READ TABLE it_hrp1000 INTO wa_hrp1000 WITH KEY objid = <fs_pa0001>-stell.
        IF sy-subrc IS INITIAL.
          wa_saida2-cargo      = <fs_pa0001>-stell.
          wa_saida2-desc_cargo = wa_hrp1000-stext.
        ENDIF.

        READ TABLE it_hrp1000x INTO wa_hrp1000x WITH KEY objid = <fs_pa0001>-plans.
        IF sy-subrc IS INITIAL.
          wa_saida2-plans          = <fs_pa0001>-plans.
          wa_saida2-des_pos        = wa_hrp1000x-stext.
        ENDIF.

        READ TABLE it_pa0008 INTO wa_pa0008 WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          wa_saida2-bet01  =  wa_pa0008-bet01.
          wa_saida2-edates =  wa_pa0008-aedtm+6(2) && '/' && wa_pa0008-aedtm+4(2) && '/' && wa_pa0008-aedtm(4). "entrydate
        ENDIF.

        READ TABLE it_pa0002 INTO wa_pa0002 WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          wa_saida2-cname  =  wa_pa0002-cname.
        ENDIF.

        READ TABLE it_pa0016 INTO wa_pa0016 WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          wa_saida2-cttyp  =  wa_pa0016-cttyp.
          READ TABLE it_t547s INTO wa_t547s WITH KEY cttyp = wa_pa0016-cttyp.
          IF sy-subrc IS INITIAL.
            wa_saida2-cttyp  =  wa_pa0016-cttyp && ' - ' && wa_t547s-cttxt.
          ENDIF.
          wa_saida2-ctedt  =  wa_pa0016-ctedt+6(2) && '/' && wa_pa0016-ctedt+4(2) && '/' && wa_pa0016-ctedt(4).
        ENDIF.

*ADVERT  "*ADVERTENCIA/SUSPENSAO  Contador da pa0030 quantidade de linhas
        CLEAR lv_cont.
        LOOP AT it_pa0030 INTO wa_pa0030 WHERE pernr EQ <fs_pa0001>-pernr.
          lv_cont = lv_cont + 1.
        ENDLOOP.
        wa_saida2-advert  =  lv_cont.

        READ TABLE it_hrp9666 INTO wa_hrp9666 WITH KEY objid = <fs_pa0001>-plans.
        IF sy-subrc IS INITIAL.

          READ TABLE it_pa0001x INTO wa_pa0001x WITH KEY plans = <fs_pa0001>-plans
                                                               bukrs = <fs_pa0001>-bukrs
                                                               werks = <fs_pa0001>-werks
                                                               kostl = <fs_pa0001>-kostl
                                                               endda = v_ultimo_dia.
          IF sy-subrc IS INITIAL.
*            wa_saida2-movimenta = abap_true.
          ENDIF.

          wa_saida2-tp_premio   =  wa_hrp9666-ztpbenef.
          READ TABLE it_dd07t INTO wa_dd07t WITH KEY domvalue_l = wa_hrp9666-ztpbenef.
          IF sy-subrc IS INITIAL AND wa_dd07t-domvalue_l NE 'GR'.
            wa_saida2-tp_premio   = wa_dd07t-domvalue_l && ' - ' && wa_dd07t-ddtext.
          ELSEIF wa_dd07t-domvalue_l EQ 'GR'.
            CONTINUE.
          ENDIF.

          IF wa_hrp9666-zvalor IS NOT INITIAL.
            wa_saida2-vlr_p_atual  = wa_hrp9666-zvalor.
          ENDIF.
        ENDIF.

        READ TABLE it_pa0015 INTO wa_pa0015 WITH KEY pernr = <fs_pa0001>-pernr.
        IF sy-subrc IS INITIAL.
          wa_saida2-vlr_p_z  = abap_true.
        ENDIF.

* -------------------------------------

        READ TABLE p_datad INTO DATA(lv_datad) INDEX 1.
        IF sy-subrc IS INITIAL.
          DATA(lv_compd) = lv_datad-low+6(2).
          lv_compm = lv_datad-low+4(2).
          lv_compa = lv_datad-low(4).
        ENDIF.

        BREAK rfreitas.

        DATA: lv_continue TYPE c.
        lv_continue = abap_true.
        WHILE lv_continue IS NOT INITIAL.
          CLEAR: lv_continue.

          lv_datac = lv_compm && '/' && lv_compa.
          wa_saida2-competencia = lv_datac.
          wa_saida2-compi = lv_compa && lv_compm && '01'.

          CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
            EXPORTING
              day_in            = wa_saida2-compi
            IMPORTING
              last_day_of_month = v_ultimo_diac.

          wa_saida2-compf = v_ultimo_diac.

          IF wa_pa0008-aedtm GT wa_saida2-compf.
            CLEAR lv_continue.
            EXIT.
          ENDIF.

          IF lv_compm EQ '01'.
            lv_compm = '12'.
            lv_compa = lv_compa - 1.
          ELSE.
            lv_compm = lv_compm - 1.
          ENDIF.
* -------------------------------------
* Objeto
          CLEAR: cl_read_payroll.
          CREATE OBJECT cl_read_payroll
            EXPORTING
              iv_pernr = <fs_pa0001>-pernr.

* Lista de Clusters
          REFRESH: tl_rgdir.
          CALL METHOD cl_read_payroll->get_rgdir
            EXPORTING
*             iv_begda = lv_dat
*             iv_endda = v_ultimo_diae
              iv_begda = wa_saida2-compi
              iv_endda = v_ultimo_diac
            IMPORTING
              et_rgdir = tl_rgdir.

          CLEAR: wl_rgdir.

          DELETE tl_rgdir WHERE ocrsn NOT IN rg_ocrsn.

          REFRESH tl_payroll.
          CALL METHOD cl_read_payroll->get_pay_result_table
            EXPORTING
              it_rgdir        = tl_rgdir
            IMPORTING
              et_paybr_result = tl_payroll.

          REFRESH: tl_rt.

          CLEAR: wl_payroll.
          LOOP AT tl_payroll INTO wl_payroll.
            APPEND LINES OF wl_payroll-inter-rt TO tl_rt.
          ENDLOOP.

          IF tl_rt[] IS INITIAL.
            lv_continue = abap_true.
            APPEND wa_saida2 TO it_saida2.
          ENDIF.

          CLEAR: lv_falta.

          LOOP AT tl_rt INTO wl_rt.

            IF wl_rt-lgart =  '1031'. "selecionar anzhl e betrg
*            wa_saida2-avos_p_folha = ( <fs_saida1>-avos_p_folha ) + wl_rt-anzhl.
*            wa_saida2-vlr_p_folha =  ( <fs_saida1>-vlr_p_folha ) + wl_rt-betrg.
            ENDIF.
            IF wl_rt-lgart =  '3032'. "faltas
              wa_saida2-falta = abap_true.
              lv_falta = lv_falta + 1.
            ENDIF.

            IF wl_rt-lgart = '1231'.
              wa_saida2-acumul_avos = ( wa_saida2-acumul_avos ) + wl_rt-betpe.
              wa_saida2-acumul_vlr =  ( wa_saida2-acumul_vlr ) + ( ( wl_rt-betpe / 100000 ) * wl_rt-anzhl ).

* data retirada
              lv_continue = abap_true.

              IF wa_pa0008-aedtm GT wa_saida2-compf.
                CLEAR lv_continue.
                EXIT.
              ENDIF.

              APPEND wa_saida2 TO it_saida2.
            ENDIF.
          ENDLOOP.

          CLEAR: wa_saida2-falta, lv_falta, wa_saida2-acumul_avos, wa_saida2-acumul_vlr.

          IF wa_pa0008-aedtm GT wa_saida2-compf.
            CLEAR lv_continue.
            EXIT.
          ENDIF.

        ENDWHILE.

        IF it_saida2[] IS NOT INITIAL.

          DATA(lv_qtdl) = lines( it_saida2 ).

          IF lv_qtdl GE 2.

            READ TABLE it_saida2 INTO DATA(wa_saida2_1) INDEX 1.
            IF sy-subrc IS INITIAL.

              READ TABLE it_saida2 INTO DATA(wa_saida2_2) INDEX 2.
              IF sy-subrc IS INITIAL.
* ----
                wa_saida2_1-acumul_avos = ( lv_compd / 30 ) * 100000 + wa_saida2_2-acumul_avos.
                wa_saida2_1-acumul_vlr =  ( wa_saida2_1-acumul_avos / 100000 ) * wa_saida2_1-vlr_p_atual.

                MODIFY it_saida2 FROM wa_saida2_1 INDEX 1.
              ENDIF.
            ENDIF.

          ELSEIF lv_qtdl EQ 1.

            READ TABLE it_saida2 INTO wa_saida2_1 INDEX 1.
            IF sy-subrc IS INITIAL.

              wa_saida2_1-acumul_avos = ( lv_compd / 30 ) * 100000 + 0.
              wa_saida2_1-acumul_vlr =  ( wa_saida2_1-acumul_avos / 100000 ) * wa_saida2_1-vlr_p_atual.

              MODIFY it_saida2 FROM wa_saida2_1 INDEX 1.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lv_falta GE 1
         OR wa_saida2-vlr_p_z IS NOT INITIAL
         OR wa_saida2-advert GE 1
         OR wa_saida2-ctedt LT lv_p_dia. "lv_u_dia
*          wa_saida2-avos_p_m = wa_saida2-ctedt+6(2).
*          wa_saida2-vlr_p_m = ( ( wa_saida2-vlr_p_atual / 30 ) * ( wa_saida2-ctedt+6(2) ) ).
          lv_c = abap_true.
        ENDIF.
      ENDLOOP. " 2
    WHEN OTHERS.
  ENDCASE.


  IF it_saida2[] IS NOT INITIAL.
    DATA: lva_data_inicio TYPE sy-datum,
          lva_data_fim    TYPE sy-datum.
*competencia
    CLEAR: lv_cont, wa_pa0030.

    rg_ano = VALUE #( FOR i IN it_saida2[] ( sign = 'I' option = 'EQ' low = i-competencia+3(4) ) ).
    rg_mes = VALUE #( FOR j IN it_saida2[] ( sign = 'I' option = 'EQ' low = j-competencia+0(2) ) ).

*Buscar advertência/suspensão
    IF rg_ano IS NOT INITIAL AND rg_mes IS NOT INITIAL.
      SELECT * FROM zhcmt0004 "selecionar initm e fimtm
        INTO TABLE @DATA(gt_zhcmt0004)
       WHERE anopr IN @rg_ano    "parâmetro
         AND mespr IN @rg_mes.   "parâmetro

*Ir na tabela pa0030 com

      IF it_zhcmt0004 IS NOT INITIAL.
        SELECT * FROM pa0030
          INTO TABLE @DATA(gt_pa0030)
          FOR ALL ENTRIES IN @gt_zhcmt0004
          WHERE pernr IN @p_pernr
            AND ( subty EQ '01'
            OR   subty EQ '10' )
            AND begda GE @gt_zhcmt0004-initm
            AND endda LE @gt_zhcmt0004-fimtm.
        IF sy-subrc EQ 0.
          LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<ws_saida>).
*  - CSB - Inicio
*            read table gt_zhcmt0004 into data(ws_zhcmt0004) with key anopr = <ws_saida>-competencia+0(4)
*                                                                     mespr = <ws_saida>-competencia+4(2).
            READ TABLE gt_zhcmt0004 INTO DATA(ws_zhcmt0004) WITH KEY anopr = <ws_saida>-competencia+3(4)
                                                                    mespr = <ws_saida>-competencia+0(2).
            IF sy-subrc EQ 0.
              CLEAR: lva_data_inicio,  lva_data_fim.
              CONCATENATE <ws_saida>-competencia+3(4) <ws_saida>-competencia+0(2) '01' INTO lva_data_inicio.

              CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
                EXPORTING
                  day_in            = lva_data_inicio
                IMPORTING
                  last_day_of_month = lva_data_fim.


* ADVERT  "*ADVERTENCIA/SUSPENSAO  Contador da pa0030 quantidade de linhas
              CLEAR lv_cont.
              LOOP AT gt_pa0030 INTO wa_pa0030 WHERE pernr EQ <ws_saida>-matricula.
                IF wa_pa0030-begda >= lva_data_inicio AND  wa_pa0030-begda <= lva_data_fim.
                  lv_cont = lv_cont + 1.
                ENDIF.
              ENDLOOP.
              <ws_saida>-advert  =  lv_cont.


              READ TABLE it_pa0015 INTO DATA(lwa_pa0015) WITH KEY pernr =  <ws_saida>-matricula.
              IF sy-subrc IS INITIAL.
                IF lwa_pa0015-begda >= lva_data_inicio AND  lwa_pa0015-begda <= lva_data_fim.
                  <ws_saida>-vlr_p_z  = abap_true.
                ELSE.
                  <ws_saida>-vlr_p_z = ''.
                ENDIF.
              ENDIF.

            ENDIF.
            CLEAR: lv_cont.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.

*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Início de Alteração
*  ELSE. "BUG - 158063 - CBRAND
*    MESSAGE 'Dados não encontrados.' TYPE 'I' DISPLAY LIKE 'E'.
*    CALL SELECTION-SCREEN 1000.
*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Fim de Alteração
  ENDIF.

*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Início de Alteração
  CLEAR vg_n_encontrou_dados.

  IF  it_saida1 IS INITIAL
  AND it_saida2 IS INITIAL.

    vg_n_encontrou_dados = abap_true.
    MESSAGE 'Dados não encontrados.' TYPE 'I' DISPLAY LIKE 'E'.

  ENDIF.
*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Fim de Alteração

ENDFORM.


MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0. "LEAVE PROGRAM.
    WHEN 'SAIR'.
      LEAVE PROGRAM.
    WHEN 'VOLTAR'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM fm_cria_fieldcat .
  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  CASE p_check.

    WHEN abap_false.

**** BUG - 149504 - Inicio - CBRAND
*      git_fcat_pend1 = VALUE lit_fieldcat_aux(
*    ( tabname = 'IT_SAIDA1'  fieldname = 'BUKRS            '            coltext = 'COD. EMPRESA                 'col_opt = 'X' no_zero = '' ref_table = 'PA0001   ' ref_field = 'BUKRS        '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'BUKRS_TEXT       '            coltext = 'NOME EMPRESA                 'col_opt = 'X' no_zero = '' ref_table = 'T001_BF  ' ref_field = 'BUTXT        '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'COD_FILIAL       '            coltext = 'COD. FILIAL                  'col_opt = 'X' no_zero = '' ref_table = 'PA0001   ' ref_field = 'WERKS         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'NOME_FILIAL      '            coltext = 'NOME_FILIAL                  'col_opt = 'X' no_zero = '' ref_table = 'T500P    ' ref_field = 'NAME1         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'MATRICULA        '            coltext = 'Matricula                    'col_opt = 'X' no_zero = '' ref_table = 'PA0001   ' ref_field = 'PERNR'         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'CNAME           '            coltext = 'Nome                          'col_opt = 'X' no_zero = '' ref_table = 'PA0002   ' ref_field = 'CNAME         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'EDATES          '            coltext = 'DATA ADMISSÃO                 'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'EDATES        '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'STATUS          '            coltext = 'Descrição                     'col_opt = 'X' no_zero = '' ref_table = 'HRP1000  ' ref_field = 'STEXT         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'CTTYP           '            coltext = 'TIPO DE CONTRATO              'col_opt = 'X' no_zero = '' ref_table = 'HRP1000  ' ref_field = 'STEXT          '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'CTEDT           '            coltext = 'TÉRMINO CONTRATO              'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'CTEDT         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'BET01           '            coltext = 'SALÁRIO BASE                  'col_opt = 'X' no_zero = '' ref_table = 'PA0008   ' ref_field = 'BET01         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'PERSK           '            coltext = 'SUBGRUPO EMPREGADOS           'col_opt = 'X' no_zero = '' ref_table = 'HRP1000  ' ref_field = 'STEXT          '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'CARGO           '            coltext = 'CARGO                         'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'CARGO         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'DESC_CARGO      '            coltext = 'DESC. CARGO                   'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'DESC_CARGO    '         )
**    ( tabname = 'T_SAIDA1'  fieldname = 'POS_ANT         '            coltext = 'POSIÇÃO ANTERIOR              'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'POS_ANT       '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'PLANS           '            coltext = 'POSIÇÃO ANTERIOR              'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'PLANS           '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'DES_POS_ANT     '            coltext = 'DESC. POSIÇÃO ANTERIOR        'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'DES_POS_ANT     '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'POS_ATUAL       '            coltext = 'POSIÇÃO ATUAL                 'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'POS_ATUAL       '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'DESC_POS_ATUAL  '            coltext = 'DESC. POSIÇÃO ATUAL           'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'DESC_POS_ATUAL  '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'TP_P_ANT        '            coltext = 'TP PRÊMIO ANTERIOR            'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'TP_P_ANT        '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'VLR_P_ANT       '            coltext = 'VLR PRÊMIO ANTERIOR           'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'VLR_P_ANT       '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'TP_P_ATUAL      '            coltext = 'TP PRÊMIO ATUAL               'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'TP_P_ATUAL      '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'VLR_P_ATUAL     '            coltext = 'VLR PRÊMIO ATUAL              'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'VLR_P_ATUAL     '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'GRATIF          '            coltext = '% GRATIFICAÇÃO                'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'GRATIF          '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'ADVERT          '            coltext = 'ADVERTENCIA/SUSPENSAO         'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'ADVERT          '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'FALTA           '            coltext = 'FALTA                         'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'FALTA           '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'VLR_P_Z         '            coltext = 'VALOR PREMIO ZERADO?          'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'VLR_P_Z         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'MOVIMENTA       '            coltext = 'MOVIMENTAÇÃO?                 'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'MOVIMENTA       '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'AVOS_P_M        '            coltext = 'CALC - AVOS PRÊMIO MENSAL     'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'AVOS_P_M        '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'VLR_P_M         '            coltext = 'CALC - VLR PRÊMIO MENSAL      'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'VLR_P_M         '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'ACUMUL_AVOS     '            coltext = 'ACUMULADO - AVOS PRÊMIO RESC  'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'ACUMUL_AVOS     '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'ACUMUL_VLR      '            coltext = 'ACUMULADO - VLR PRÊMIO RESC   'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'ACUMUL_VLR      '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'AVOS_P_FOLHA    '            coltext = 'AVOS PREMIO FOLHA             'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'AVOS_P_FOLHA    '         )
*    ( tabname = 'IT_SAIDA1'  fieldname = 'VLR_P_FOLHA     '            coltext = 'VLR PREMIO FOLHA              'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA1' ref_field = 'VLR_P_FOLHA     '         )
*    ).

      git_fcat_pend1 = VALUE lit_fieldcat_aux(
    (  fieldname = 'BUKRS'            coltext = 'COD. EMPRESA                  '   outputlen                = '15'    )
    (  fieldname = 'BUKRS_TEXT'       coltext = 'NOME EMPRESA                  '   outputlen                = '30'    )
    (  fieldname = 'COD_FILIAL'       coltext = 'COD. FILIAL                   '   outputlen                = '15'    )
    (  fieldname = 'NOME_FILIAL'      coltext = 'NOME_FILIAL                   '   outputlen                = '15'    )
    (  fieldname = 'MATRICULA'        coltext = 'Matricula                     '   outputlen                = '12'    )
    (  fieldname = 'CNAME'            coltext = 'Nome                          '   outputlen                = '30'    )
    (  fieldname = 'EDATES'           coltext = 'DATA ADMISSÃO                 '   outputlen                = '20'    )
    (  fieldname = 'STATUS'           coltext = 'Descrição                     '   outputlen                = '20'    )
    (  fieldname = 'CTTYP'            coltext = 'TIPO DE CONTRATO              '   outputlen                = '30'    )
    (  fieldname = 'CTEDT'            coltext = 'TÉRMINO CONTRATO              '   outputlen                = '30'    )
    (  fieldname = 'BET01'            coltext = 'SALÁRIO BASE                  '   outputlen                = '20'    )
    (  fieldname = 'PERSK'            coltext = 'SUBGRUPO EMPREGADOS           '   outputlen                = '30'    )
    (  fieldname = 'CARGO'            coltext = 'CARGO                         '   outputlen                = '15'    )
    (  fieldname = 'DESC_CARGO'       coltext = 'DESC. CARGO                   '   outputlen                = '30'    )
    (  fieldname = 'PLANS'            coltext = 'POSIÇÃO ANTERIOR              '   outputlen                = '20'    )
    (  fieldname = 'DES_POS_ANT'      coltext = 'DESC. POSIÇÃO ANTERIOR        '   outputlen                = '30'    )
    (  fieldname = 'POS_ATUAL'        coltext = 'POSIÇÃO ATUAL                 '   outputlen                = '15'    )
    (  fieldname = 'DESC_POS_ATUAL'   coltext = 'DESC. POSIÇÃO ATUAL           '   outputlen                = '30'    )
    (  fieldname = 'TP_P_ANT'         coltext = 'TP PRÊMIO ANTERIOR            '   outputlen                = '20'    )
    (  fieldname = 'VLR_P_ANT'        coltext = 'VLR PRÊMIO ANTERIOR           '   outputlen                = '25'    )
    (  fieldname = 'TP_P_ATUAL'       coltext = 'TP PRÊMIO ATUAL               '   outputlen                = '20'    )
    (  fieldname = 'VLR_P_ATUAL'      coltext = 'VLR PRÊMIO ATUAL              '   outputlen                = '20'    )
    (  fieldname = 'GRATIF'           coltext = '% GRATIFICAÇÃO                '   outputlen                = '20'    )
    (  fieldname = 'ADVERT'           coltext = 'ADVERTENCIA/SUSPENSAO         '   outputlen                = '25'    )
    (  fieldname = 'FALTA'            coltext = 'FALTA                         '   outputlen                = '10'    )
    (  fieldname = 'VLR_P_Z'          coltext = 'VALOR PREMIO ZERADO?          '   outputlen                = '30'    )
    (  fieldname = 'MOVIMENTA'        coltext = 'MOVIMENTAÇÃO?                 '   outputlen                = '15'    )
    (  fieldname = 'AVOS_P_M'         coltext = 'CALC - AVOS PRÊMIO MENSAL     '   outputlen                = '30'    )
    (  fieldname = 'VLR_P_M'          coltext = 'CALC - VLR PRÊMIO MENSAL      '   outputlen                = '30'    )
    (  fieldname = 'ACUMUL_AVOS'      coltext = 'ACUMULADO - AVOS PRÊMIO RESC  '   outputlen                = '30'    )
    (  fieldname = 'ACUMUL_VLR'       coltext = 'ACUMULADO - VLR PRÊMIO RESC   '   outputlen                = '30'    )
    (  fieldname = 'AVOS_P_FOLHA'     coltext = 'AVOS PREMIO FOLHA             '   outputlen                = '25'    )
    (  fieldname = 'VLR_P_FOLHA'      coltext = 'VLR PREMIO FOLHA              '   outputlen                = '25'    )
    ).
**** BUG - 149504 - Fim - CBRAND
    WHEN abap_true.
**** BUG - 149504 - Inicio - CBRAND
*      git_fcat_pend2 = VALUE lit_fieldcat_aux(
*    ( tabname = 'IT_SAIDA2'  fieldname = 'BUKRS          '            coltext = 'COD. EMPRESA                   'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'BUKRS         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'BUKRS_TEXT     '            coltext = 'NOME EMPRESA                   'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'BUKRS_TEXT    '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'COD_FILIAL     '            coltext = 'COD. FILIAL                    'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'COD_FILIAL    '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'NOME_FILIAL    '            coltext = 'NOME_FILIAL                    'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'NOME_FILIAL   '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'MATRICULA      '            coltext = 'MATRICULA                      'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'MATRICULA     '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'CNAME          '            coltext = 'NOME                           'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'CNAME         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'EDATES         '            coltext = 'DATA ADMISSÃO                  'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'EDATES        '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'STATUS         '            coltext = 'STATUS                         'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'STATUS        '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'CTTYP          '            coltext = 'TIPO DE CONTRATO               'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'CTTYP         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'CTEDT          '            coltext = 'TÉRMINO CONTRATO DET.          'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'CTEDT         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'BET01          '            coltext = 'SALÁRIO BASE                   'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'BET01         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'PERSK          '            coltext = 'SUBGRUPO EMPREGADOS            'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'PERSK         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'CARGO          '            coltext = 'CARGO                          'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'CARGO         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'DESC_CARGO     '            coltext = 'DESC. CARGO                    'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'DESC_CARGO    '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'PLANS          '            coltext = 'POSIÇÃO                        'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'PLANS         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'DES_POS        '            coltext = 'DESC. POSIÇÃO                  'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'DES_POS       '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'TP_PREMIO      '            coltext = 'TP PRÊMIO                      'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'TP_PREMIO     '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'VLR_P_ATUAL    '            coltext = 'VLR PRÊMIO                     'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'VLR_P_ATUAL   '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'COMPETENCIA    '            coltext = 'COMPETENCIA                    'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'COMPETENCIA   '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'ADVERT         '            coltext = 'ADVERTENCIA/SUSPENSAO          'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'ADVERT        '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'FALTA          '            coltext = 'FALTA                          'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'FALTA         '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'VLR_P_Z        '            coltext = 'VALOR PREMIO ZERADO ?          'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'VLR_P_Z       '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'ACUMUL_AVOS    '            coltext = 'ACUMULADO - AVOS PRÊMIO RESC   'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'ACUMUL_AVOS   '         )
*    ( tabname = 'IT_SAIDA2'  fieldname = 'ACUMUL_VLR     '            coltext = 'ACUMULADO - VLR PRÊMIO RESC    'col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA2' ref_field = 'ACUMUL_VLR      '         )
*    ).
      git_fcat_pend2 = VALUE lit_fieldcat_aux(
    (   fieldname = 'BUKRS'            coltext = 'COD. EMPRESA                   ' outputlen                = '15' )
    (   fieldname = 'BUKRS_TEXT'       coltext = 'NOME EMPRESA                   ' outputlen                = '30' )
    (   fieldname = 'COD_FILIAL'       coltext = 'COD. FILIAL                    ' outputlen                = '15' )
    (   fieldname = 'NOME_FILIAL'      coltext = 'NOME_FILIAL                    ' outputlen                = '15' )
    (   fieldname = 'MATRICULA'        coltext = 'MATRICULA                      ' outputlen                = '12' )
    (   fieldname = 'CNAME'            coltext = 'NOME                           ' outputlen                = '30' )
    (   fieldname = 'EDATES'           coltext = 'DATA ADMISSÃO                  ' outputlen                = '20' )
    (   fieldname = 'STATUS'           coltext = 'STATUS                         ' outputlen                = '20' )
    (   fieldname = 'CTTYP'            coltext = 'TIPO DE CONTRATO               ' outputlen                = '30' )
    (   fieldname = 'CTEDT'            coltext = 'TÉRMINO CONTRATO DET.          ' outputlen                = '30' )
    (   fieldname = 'BET01'            coltext = 'SALÁRIO BASE                   ' outputlen                = '20' )
    (   fieldname = 'PERSK'            coltext = 'SUBGRUPO EMPREGADOS            ' outputlen                = '30' )
    (   fieldname = 'CARGO'            coltext = 'CARGO                          ' outputlen                = '15' )
    (   fieldname = 'DESC_CARGO'       coltext = 'DESC. CARGO                    ' outputlen                = '30' )
    (   fieldname = 'PLANS'            coltext = 'POSIÇÃO                        ' outputlen                = '20' )
    (   fieldname = 'DES_POS'          coltext = 'DESC. POSIÇÃO                  ' outputlen                = '30' )
    (   fieldname = 'TP_PREMIO'        coltext = 'TP PRÊMIO                      ' outputlen                = '15' )
    (   fieldname = 'VLR_P_ATUAL'      coltext = 'VLR PRÊMIO                     ' outputlen                = '30' )
    (   fieldname = 'COMPETENCIA'      coltext = 'COMPETENCIA                    ' outputlen                = '20' )
    (   fieldname = 'ADVERT'           coltext = 'ADVERTENCIA/SUSPENSAO          ' outputlen                = '25' )
    (   fieldname = 'FALTA'            coltext = 'FALTA                          ' outputlen                = '20' )
    (   fieldname = 'VLR_P_Z'          coltext = 'VALOR PREMIO ZERADO ?          ' outputlen                = '20' )
    (   fieldname = 'ACUMUL_AVOS'      coltext = 'ACUMULADO - AVOS PRÊMIO RESC   ' outputlen                = '30' )
    (   fieldname = 'ACUMUL_VLR'       coltext = 'ACUMULADO - VLR PRÊMIO RESC    ' outputlen                = '30' )
    ).

**** BUG - 149504 - Fim - CBRAND
  ENDCASE.
ENDFORM.

FORM fm_alv.
  gs_variant-report = sy-repid.

  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(

  EXPORTING
    i_titulo  = 'Relatório de Conferência Prêmio/Gratificação - ' &&  lva_data
     i_filtros = VALUE zif_screen_linha_filtro_t(
   )

  CHANGING
    alv = gob_gui_alv_grid
    )
    EQ abap_true.

    wa_layout-sel_mode   = 'A'.
    wa_layout-zebra      = 'X'.

    CASE p_check.

      WHEN abap_false.

        CALL METHOD gob_gui_alv_grid->set_table_for_first_display
          EXPORTING
            is_variant                    = gs_variant
            i_save                        = 'A'
            is_layout                     = wa_layout
            i_default                     = 'X'
          CHANGING
            it_outtab                     = it_saida1
            it_fieldcatalog               = git_fcat_pend1
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN abap_true.

        CALL METHOD gob_gui_alv_grid->set_table_for_first_display
          EXPORTING
            is_variant                    = gs_variant
            i_save                        = 'A'
            is_layout                     = wa_layout
            i_default                     = 'X'
          CHANGING
            it_outtab                     = it_saida2
            it_fieldcatalog               = git_fcat_pend2
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

    ENDCASE.
  ENDIF.
ENDFORM.

FORM fm_exibirdados .

*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Início de Alteração
*  CALL SCREEN '0100'.
  IF vg_n_encontrou_dados IS INITIAL.

    CALL SCREEN '0100'.

  ENDIF.
*** Stefanini - IR218788 - 05/02/2025 - LAZAROSR - Fim de Alteração

ENDFORM.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  PERFORM fm_alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form check_authorization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> S_WERKS_LOW
*&      --> P_
*&      <-- V_RETURN
*&---------------------------------------------------------------------*
FORM check_authorization USING
                         p_object
                         p_field
                         p_id
                         CHANGING
                         return.
  AUTHORITY-CHECK OBJECT p_object ID p_id
  FIELD p_field.
  return = sy-subrc.
ENDFORM.
