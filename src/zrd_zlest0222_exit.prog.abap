*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0222_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_zlest0222_exit.

TYPES: BEGIN OF ty_rbkp_sel,
         lifnr TYPE rbkp-lifnr,
         xblnr TYPE rbkp-xblnr,
         stblg TYPE rbkp-stblg.
TYPES: END   OF ty_rbkp_sel.

TYPES: BEGIN OF ty_bkpf_sel,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey.
TYPES: END   OF ty_bkpf_sel.

TYPES: BEGIN OF ty_select,
         line TYPE fieldname,
       END OF ty_select.

*TYPES: BEGIN OF ty_ZLEST0222,
*         WG_DESC_EMPRESA TYPE t001-butxt.
*         include STRUCTURE  ZLEST0222.
*TYPES: END   OF ty_ZLEST0222.
*
*data: it_ZLEST0222 TYPE TABLE OF ty_ZLEST0222,
*wa_zlest0222 TYPE TY_ZLEST0222.

DATA: desc_empresa    TYPE butxt.

DATA: t_rbkp_sel TYPE TABLE OF ty_rbkp_sel,
      w_rbkp_sel TYPE ty_rbkp_sel,
      t_bkpf_sel TYPE TABLE OF ty_bkpf_sel,
      w_bkpf_sel TYPE ty_bkpf_sel.

DATA t_where  TYPE TABLE OF ty_select WITH HEADER LINE.

FORM f_exit_zlest0222_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_zlest0222 TYPE zlest0222.
  CLEAR:  wa_zlest0222.

  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0222.

  CLEAR: p_error.
  IF wa_zlest0222-bukrs IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Empresa é obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-cnpj_filial IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo CNPJ Filial é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-cnpj_transbordo IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo CNPJ Prestador de Serviço é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.

    SELECT SINGLE *
  FROM lfa1 INTO @DATA(lwa_lfa1)
  WHERE stcd1 EQ @wa_zlest0222-cnpj_transbordo.

    IF sy-subrc IS NOT INITIAL.

      p_error = abap_true.
      MESSAGE i000(z01) WITH 'CNPJ Prestador de Serviço não encontrado'.
      EXIT.

    ENDIF.
  ENDIF.

  IF wa_zlest0222-matnr IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Produto é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-datatransb_de IS INITIAL AND wa_zlest0222-datatransb_ate IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Obrigatório Informar de Período Competência' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-pesochegada  IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Volume Armazenado (Ton) é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-vlr_tot_est  IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Valor To. Armazenagem Estática é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-nfps  IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo NF Serviço é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-data  IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Data Emissão NF Serviço é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-data  IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Campo Valor Total do Serviço é obrigatório ' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF wa_zlest0222-tarifa_unitaria IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Tarifa de frete não cadastrada, favor verificar!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  IF wa_zlest0222-vlr_tot_est IS INITIAL.
    p_error = abap_true.
    MESSAGE 'Valor Total Armazenagem Estática não informada, favor verificar' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_exit_zlest0222_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_zlest0222 TYPE zlest0222.
  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0222.
*  wa_ZLEST0222-usnam_cad = sy-uname.
*  wa_ZLEST0222-dt_cad = sy-datum.
*  wa_ZLEST0222-hr_cad = sy-uzeit.
  MOVE-CORRESPONDING wa_zlest0222 TO p_registro_manter.

ENDFORM.

FORM f_exit_zlest0222_0004 CHANGING p_saida TYPE any.

  DATA: wl_zlest0222_out TYPE zlest0222_out,
        l_awkey          TYPE bkpf-awkey.



  CLEAR: wl_zlest0222_out.


  MOVE-CORRESPONDING p_saida TO wl_zlest0222_out.
  CLEAR p_saida.

  IF sy-subrc IS INITIAL.

    SELECT lifnr, stcd1
    FROM lfa1
    INTO TABLE @DATA(t_lfa1)
   WHERE stcd1 = @wl_zlest0222_out-cnpj_transbordo.


    SORT t_lfa1 BY stcd1.
    DELETE ADJACENT DUPLICATES FROM t_lfa1
                          COMPARING stcd1.

    READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY stcd1 = wl_zlest0222_out-cnpj_transbordo
                       BINARY SEARCH.

    IF sy-subrc = 0.
      w_rbkp_sel-lifnr     = w_lfa1-lifnr.
      "w_rbkp_sel-xblnr     = wl_zlest0222_out-nfps.
      w_rbkp_sel-stblg     = abap_off.
      APPEND w_rbkp_sel   TO t_rbkp_sel.


        CONCATENATE  'LIFNR  = ''' w_lfa1-lifnr '''' INTO t_where-line .
        APPEND t_where.

        t_where-line = 'and'.
        APPEND t_where.

      IF wl_zlest0222_out-nfps IS NOT INITIAL.


        SEARCH wl_zlest0222_out-nfps FOR '-' AND MARK.
        IF sy-subrc = 0.
          CONCATENATE  'XBLNR  = ''' wl_zlest0222_out-nfps '''' INTO t_where-line .
          APPEND t_where.
        ELSE.
          CONCATENATE  'XBLNR LIKE ''' wl_zlest0222_out-nfps '-%''' INTO t_where-line .
          APPEND t_where.
        ENDIF.
      ENDIF.

       t_where-line = 'and'.
        APPEND t_where.

       CONCATENATE    'STBLG = ''' abap_off '''' INTO t_where-line .
          APPEND t_where.
    ENDIF.

    IF  t_rbkp_sel[] IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, lifnr, xblnr, stblg
        FROM rbkp
        INTO TABLE @DATA(t_rbkp)
        " FOR ALL ENTRIES IN @t_rbkp_sel
       WHERE (t_where).
         "lifnr = @t_rbkp_sel-lifnr
         "AND xblnr = @t_rbkp_sel-xblnr
         "AND stblg = @t_rbkp_sel-stblg
         "AND (t_where).
    ENDIF.

    IF t_rbkp[] IS NOT INITIAL.
      SELECT belnr, gjahr, ebeln
        FROM ekbe
        INTO TABLE @DATA(t_ekbe)
         FOR ALL ENTRIES IN @t_rbkp
       WHERE belnr = @t_rbkp-belnr
         AND gjahr = @t_rbkp-gjahr.
    ENDIF.

    LOOP AT t_rbkp     INTO DATA(w_rbkp).
      w_bkpf_sel-bukrs    = w_rbkp-bukrs.
      w_bkpf_sel-gjahr    = w_rbkp-gjahr.
      w_bkpf_sel-awkey    = w_rbkp-belnr && w_rbkp-gjahr.
      APPEND w_bkpf_sel  TO t_bkpf_sel.
    ENDLOOP.

    IF t_bkpf_sel[] IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr, awkey
        FROM bkpf
        INTO TABLE @DATA(t_bkpf)
         FOR ALL ENTRIES IN @t_bkpf_sel
       WHERE bukrs = @t_bkpf_sel-bukrs
         AND gjahr = @t_bkpf_sel-gjahr
         AND awkey = @t_bkpf_sel-awkey.
    ENDIF.

    IF t_bkpf[] IS NOT INITIAL.
      SELECT bukrs, belnr, gjahr
        FROM bsik
        INTO TABLE @DATA(t_bsik)
         FOR ALL ENTRIES IN @t_bkpf
       WHERE bukrs = @t_bkpf-bukrs
         AND belnr = @t_bkpf-belnr
         AND gjahr = @t_bkpf-gjahr.

      SELECT bukrs, belnr, gjahr, augbl, augdt
        FROM bsak
        INTO TABLE @DATA(t_bsak)
         FOR ALL ENTRIES IN @t_bkpf
       WHERE bukrs = @t_bkpf-bukrs
         AND belnr = @t_bkpf-belnr
         AND gjahr = @t_bkpf-gjahr.
    ENDIF.


    READ TABLE t_lfa1 INTO w_lfa1 WITH KEY stcd1 = wl_zlest0222_out-cnpj_transbordo
                                    BINARY SEARCH.

    READ TABLE t_rbkp INTO w_rbkp WITH KEY lifnr = w_lfa1-lifnr
                                           xblnr = wl_zlest0222_out-nfps
                                           stblg = abap_off.

    READ TABLE t_ekbe INTO DATA(w_ekbe) WITH KEY belnr = w_rbkp-belnr
                                                 gjahr = w_rbkp-gjahr.

    l_awkey = w_rbkp-belnr && w_rbkp-gjahr.

    READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY bukrs = w_rbkp-bukrs
                                                 gjahr = w_rbkp-gjahr
                                                 awkey = l_awkey.

    LOOP AT t_bsik INTO DATA(w_bsik) WHERE bukrs = w_bkpf-bukrs
                                       AND belnr = w_bkpf-belnr
                                       AND gjahr = w_bkpf-gjahr.
    ENDLOOP.



    IF sy-subrc <> 0.
      LOOP AT t_bsak INTO DATA(w_bsak) WHERE bukrs = w_bkpf-bukrs
                                         AND belnr = w_bkpf-belnr
                                         AND gjahr = w_bkpf-gjahr.
        wl_zlest0222_out-augbl        = w_bsak-augbl.
        wl_zlest0222_out-augdt        = w_bsak-augdt.
        EXIT.
      ENDLOOP.
    ENDIF.


    wl_zlest0222_out-ebeln            = w_ekbe-ebeln.
    wl_zlest0222_out-belnr            = w_rbkp-belnr.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = wl_zlest0222_out-matnr
    IMPORTING
      output = wl_zlest0222_out-matnr.

  MOVE-CORRESPONDING wl_zlest0222_out TO p_saida.
  CLEAR: w_lfa1, w_rbkp, l_awkey, t_where[], t_bkpf_sel[], t_rbkp_sel[].
ENDFORM.

FORM f_exit_zlest0222_0005 CHANGING p_registro_manter TYPE any.

*DATA: wa_zlest0222 TYPE  ZLEST0222.
*
*  DATA: v_qtd_dt    TYPE zlest0222-dias_transito,
*        v_diferenca TYPE i.
*  MOVE-CORRESPONDING p_registro_manter TO wa_zlest0222.
*  if wa_zlest0222-bukrs IS NOT INITIAL.
*
*    select single butxt from T001 into DESC_EMPRESA where bukrs = wa_zlest0222-bukrs.
*
*wa_zlest0222-desc_empresa = DESC_EMPRESA.
*    ENDIF.
*
*  IF wa_zlest0222-bukrs IS NOT INITIAL AND wa_zlest0222-cnpj_transbordo IS NOT INITIAL.
*    SELECT SINGLE *
*     FROM lfa1 INTO @DATA(lwa_lfa1)
*     WHERE stcd1 EQ @wa_zlest0222-cnpj_transbordo.
*
*    IF sy-subrc IS INITIAL.
*      SELECT SINGLE *
*      FROM a917
*      INTO @DATA(lwa_a917)
*      WHERE kappl EQ 'F'
*        AND kschl EQ 'ZTRA'
*        AND matnr EQ @wa_zlest0222-matnr
*        AND tdlnr EQ @lwa_lfa1-lifnr
*        AND kfrst EQ ''
*    AND  datab LE @wa_zlest0222-datatransb_de
*    AND  datbi GE @wa_zlest0222-datatransb_ate
*    AND  EXISTS ( SELECT *
*                  FROM  konp
*                  WHERE knumh = a917~knumh
*                  AND   loevm_ko EQ '' ).
*
*      IF sy-subrc EQ 0.
*        SELECT SINGLE *
*          FROM konp
*          INTO @DATA(lwa_konp)
*          WHERE knumh EQ @lwa_a917-knumh
*           AND loevm_ko EQ @space.
*      ENDIF.
*      wa_zlest0222-tarifa_unitaria = lwa_konp-kbetr.
*
*    ENDIF.
*  ENDIF.
*
*  IF wa_zlest0222-datatransb_de IS NOT INITIAL AND wa_zlest0222-datatransb_ate IS NOT INITIAL.
*
*    CALL FUNCTION 'DAYS_BETWEEN_TWO_DATES'
*      EXPORTING
*        i_datum_bis = wa_zlest0222-datatransb_de
*        i_datum_von = wa_zlest0222-datatransb_ate
*      IMPORTING
*        e_tage      = v_qtd_dt.
*    IF sy-subrc <> 0.
*
*    ELSE.
*      wa_zlest0222-dias_transito = v_qtd_dt + 1.
*    ENDIF.
*  ELSE.
*    wa_zlest0222-dias_transito = 0.
*  ENDIF.
*
*  IF wa_zlest0222-dias_transito IS NOT INITIAL AND  wa_zlest0222-tarifa_unitaria IS NOT INITIAL.
*
*    wa_zlest0222-valor_dia = wa_zlest0222-tarifa_unitaria / wa_zlest0222-dias_transito.
*  ELSE.
*    wa_zlest0222-valor_dia = 0.
*  ENDIF.
*
*
*  IF wa_zlest0222-pesochegada IS NOT INITIAL AND wa_zlest0222-dias_transito IS NOT INITIAL AND wa_zlest0222-valor_dia IS NOT INITIAL.
*    wa_zlest0222-vlr_tot_est = wa_zlest0222-pesochegada * wa_zlest0222-tarifa_unitaria.
*  ELSE.
*    wa_zlest0222-vlr_tot_est = 0.
*  ENDIF.
*
*  IF wa_zlest0222-valor_servico IS NOT INITIAL.
*
*    v_diferenca = abs( wa_zlest0222-valor_servico - wa_zlest0222-vlr_tot_est ).

*    IF v_diferenca > 2.
*      MESSAGE 'A diferança entre Valor toral do Serviço e Volume armazenado período não pode ser maior que R$2,00' TYPE 'E'.
*    ENDIF.

*  ENDIF.
*
*  MOVE-CORRESPONDING wa_zlest0222 TO p_registro_manter.



ENDFORM.

FORM  f_exit_zlest0222_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZLEST0222'.
    APPEND 'Novo'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
