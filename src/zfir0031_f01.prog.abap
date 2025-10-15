*----------------------------------------------------------------------*
***INCLUDE ZFIR0031_F01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_trata_campos  USING p_field p_group1 p_value p_invisible.
  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  APPEND tg_fields.
ENDFORM.                    " F_TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_verifica_erros .
  TYPES: BEGIN OF ty_tcurr.
           INCLUDE STRUCTURE tcurr.
  TYPES:   data TYPE sy-datum,
         END OF ty_tcurr.

  TYPES: BEGIN OF ty_tcurr_a,
           kurst TYPE tcurr-kurst,
           fcurr TYPE tcurr-fcurr,
           tcurr TYPE tcurr-tcurr,
           gdatu TYPE tcurr-gdatu,
           ukurs TYPE tcurr-ukurs,
         END OF ty_tcurr_a.

  DATA: wl_zimp_cad_depto TYPE zimp_cad_depto,
        wl_lfbk           TYPE lfbk,
        wl_zfit00146      TYPE zfit0146, "Limite de credito do OPUS
        wl_t042z          TYPE t042z,
        wl_ekko           TYPE ekko,
        wl_t012           TYPE t012,
        wl_lfa1           TYPE lfa1,
        wl_tcurc          TYPE tcurc,
        wl_tcurr          TYPE ty_tcurr,
        tl_tcurr          TYPE TABLE OF ty_tcurr,
        wl_usr_name       TYPE v_usr_name,
        wl_data           TYPE sy-datum,
        wl_gdatu(10)                    ,
        xvlrad            TYPE zfit0046-vlr_adiantamento,
        v_total           TYPE zfit0046-vlr_adiantamento,
        vsdo_disponivel   TYPE zfit0046-sdo_disponivel,
        v_saldo           TYPE vbak-netwr,
        c_saldo(20),
        v_limite          TYPE klimg,
        v_mtorg           TYPE mbew-mtorg,
        v_werks           TYPE ekpo-werks,
        wl_linha(6),
        t_libadto         TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
        vl_check_iban     TYPE boolean.

  data:  IT_DATAS               TYPE TABLE OF ISCAL_DAY. "IR155393 Não deixar programar em dias que não são uteis - BG

  RANGES: r_gdatu FOR tcurr-gdatu,
          r_fcurr FOR tcurr-fcurr.

  DATA: wl_date_aux  TYPE datum,
        wl_input(10),
        wa_tcurr     TYPE ty_tcurr_a,
        t_tcurr_a    TYPE TABLE OF ty_tcurr_a.


  REFRESH t_tcurr_a.
  MOVE 'IBT' TO r_gdatu.

  wl_date_aux = sy-datum - 30.

  WRITE wl_date_aux TO wl_input.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = wl_input
    IMPORTING
      output = r_gdatu-high.

  wl_date_aux = sy-datum.

  WRITE wl_date_aux TO wl_input.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = wl_input
    IMPORTING
      output = r_gdatu-low.

  APPEND r_gdatu.


  SELECT kurst fcurr tcurr gdatu ukurs
     FROM tcurr
     INTO TABLE t_tcurr_a
    WHERE kurst EQ 'B'
      AND fcurr EQ 'USD'
      AND tcurr EQ 'BRL'
      AND gdatu IN r_gdatu.

  SORT t_tcurr_a BY gdatu ASCENDING fcurr ASCENDING.

  CLEAR:    tg_msg_ret.
  REFRESH:  tg_msg_ret.

  DATA(_dt_liq) = sy-datum + 360.

  " Usuários
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class         = '0000'
      setnr         = 'MAGGI_ZFI0025_LIBADTO'
    TABLES
      set_values    = t_libadto
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT t_libadto BY from.

  IF wg_cadlan-lifnr_p IS INITIAL.
    MOVE: 'WG_CADLAN-LIFNR_P'  TO tg_msg_ret-field.
    CONCATENATE 'Forn. Pagamento -' 'Obrigatório' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
      WHERE lifnr EQ wg_cadlan-lifnr_p .
    IF sy-subrc NE 0.
      MOVE: 'WG_CADLAN-LIFNR_P'  TO tg_msg_ret-field,
            'Forn. Pagamento não cadastrado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
    "
    SELECT SINGLE *  "Se é fornecedor do pedido
      FROM ekko
      INTO @DATA(_ekko)
      WHERE ebeln = @wg_cadlan-ebeln
      AND   lifnr = @wg_cadlan-lifnr_p.
    IF sy-subrc NE 0.
      "função parceiro
      SELECT SINGLE * "Se é Emissor fatura do pedido
        FROM ekpa
        INTO @DATA(wl_ekpa)
        WHERE ebeln = @wg_cadlan-ebeln
        AND   parvw = 'RS'
        AND   lifn2 = @wg_cadlan-lifnr_p.
      IF sy-subrc NE 0.
        MOVE: 'WG_CADLAN-LIFNR_P'  TO tg_msg_ret-field,
             'Forn. Pagamento inválido para pedido' TO tg_msg_ret-msg.
        APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
    "
  ENDIF.



  IF wg_cadlan-resp_neg  IS INITIAL.
    MOVE: 'WG_CADLAN-RESP_NEG'  TO tg_msg_ret-field.
    CONCATENATE 'Responsável pela Negociação -' 'Obrigatório' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM  v_usr_name INTO wl_usr_name
      WHERE bname = wg_cadlan-resp_neg.
    IF sy-subrc NE 0.
      MOVE: 'WG_CADLAN-RESP_NEG'  TO tg_msg_ret-field,
            'Responsável pela Negociação não cadastrado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadlan-dep_resp IS INITIAL OR wg_cadlan-descricao IS INITIAL.
    MOVE: 'WG_CADLAN-DEP_RESP'  TO tg_msg_ret-field.
    CONCATENATE 'Dpto. Responsável -' 'Obrigatório' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM zimp_cad_depto
      INTO wl_zimp_cad_depto
      WHERE dep_resp EQ wg_cadlan-dep_resp .
    IF sy-subrc NE 0.
      MOVE: 'WG_CADLAN-DEP_RESP'  TO tg_msg_ret-field,
            'Dpto não cadastrado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ELSEIF wl_zimp_cad_depto-inativo = 'S'.
      MOVE: 'WG_CADLAN-DEP_RESP'  TO tg_msg_ret-field,
            'Dpto inativado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadlan-dt_pgto IS  INITIAL .
    MOVE: 'Informe a data de Pagamento'  TO tg_msg_ret-msg,
          'WG_CADLAN-DT_PGTO'            TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    CALL FUNCTION 'ADD_TIME_TO_DATE'
      EXPORTING
        i_idate               = sy-datum
        i_time                = 3
        i_iprkz               = ''
      IMPORTING
        o_idate               = wl_data
      EXCEPTIONS
        invalid_period        = 1
        invalid_round_up_rule = 2
        internal_error        = 3
        OTHERS                = 4.
    IF wg_cadlan-dt_pgto LT wl_data.
      MOVE: 'Data de Pagamento, menor que 72 horas'  TO tg_msg_ret-msg,
         'WG_CADLAN-DT_PGTO'            TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    "USER STORY 158527 - MMSILVA - 17.01.2025 - Inicio
    DATA: lv_hol_cal       TYPE scal-hcalid,
          lv_fac_cal       TYPE scal-fcalid,
          lv_nome_processo TYPE ze_nomep.

    lv_nome_processo = sy-tcode.

    zcl_calendario=>get_calendario(
      EXPORTING
        I_BUKRS            = wg_cadlan-bukrs
*        I_TIPO_PROCESSO    = 'T'
*        I_NOME_PROCESSO    = lv_nome_processo
      IMPORTING
        E_HOLIDAY_CALENDAR = lv_hol_cal
        E_FACTORY_CALENDAR = lv_fac_cal ).

    IF lv_hol_cal IS INITIAL.
      lv_hol_cal = 'BR'.
    ENDIF.
    IF lv_fac_cal IS INITIAL.
      lv_fac_cal = 'ZF'.
    ENDIF.
    "USER STORY 158527 - MMSILVA - 17.01.2025 - Fim


*** BUG #183642 - MMSILVA - 27.06.2025 - Ini ***

     " ------> - Inicio Comentário - BUG #183642 - <------
     "IR155393 Não deixar programar em dias que não são uteis - BG
*    if  wg_cadlan-orig_pgt = 'E' AND wg_cadlan-form_pgt = 'T' AND wg_cadlan-hbkid_e IS not INITIAL.
     " ------> - Fim Comentário - BUG #183642 - <------

     if wg_cadlan-dt_pgto is not initial.
*** BUG #183642 - MMSILVA - 27.06.2025 - Fim ***

      CALL FUNCTION 'HOLIDAY_GET'
        EXPORTING
          HOLIDAY_CALENDAR = lv_hol_cal "USER STORY 158527 - MMSILVA - 17.01.2025
          FACTORY_CALENDAR = lv_fac_cal "USER STORY 158527 - MMSILVA - 17.01.2025
          DATE_FROM        = wg_cadlan-dt_pgto
          DATE_TO          = wg_cadlan-dt_pgto
        TABLES
          HOLIDAYS         = IT_DATAS
        EXCEPTIONS
          OTHERS           = 1.

      IF IT_DATAS[] IS NOT INITIAL.
        MOVE: 'Data de Pgto informada não é um dia útil'  TO tg_msg_ret-msg,
              'WG_CADLAN-DT_PGTO'            TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      endif.

    ENDIF.
    "IR155393 Não deixar programar em dias que não são uteis - BG

  ENDIF.

  IF wg_cadlan-zlsch IS  INITIAL .
    MOVE: 'Informe a Forma de Pagamento'  TO tg_msg_ret-msg,
          'WG_CADLAN-ZLSCH'            TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
     FROM  t042z
      INTO  wl_t042z
    WHERE land1 = 'BR'
    AND   zlsch = wg_cadlan-zlsch.
    IF sy-subrc NE 0.
      MOVE: 'WG_CADLAN-ZLSCH'  TO tg_msg_ret-field,
            'Forma de Pagto não cadastrado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF  wg_cadlan-zlsch NE 'C' AND
      wg_cadlan-zlsch NE 'D' AND
      wg_cadlan-zlsch NE 'E'.
    IF wg_cadlan-bvtyp IS  INITIAL .
      MOVE: 'Informe a C/C Fornecedor'  TO tg_msg_ret-msg,
            'WG_CADLAN-BVTYP'            TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
        FROM lfbk
        INTO wl_lfbk
        WHERE lifnr = wg_cadlan-lifnr
        AND   bvtyp = wg_cadlan-bvtyp.

      IF sy-subrc NE 0.
        MOVE: 'WG_CADLAN-BVTYP'  TO tg_msg_ret-field,
              'C/C Forn. não cadastrado' TO tg_msg_ret-msg.
        APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

    IF wg_cadlan-orig_pgt NE 'E'.
      IF wg_cadlan-hbkid  IS  INITIAL .
        MOVE: 'Informe Banco Empresa'  TO tg_msg_ret-msg,
              'WG_CADLAN-HBKID  '      TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        SELECT SINGLE *
          FROM t012
          INTO wl_t012
          WHERE bukrs = wg_cadlan-bukrs
          AND   hbkid = wg_cadlan-hbkid.
        IF sy-subrc NE 0.
          MOVE: 'WG_CADLAN-HBKID'  TO tg_msg_ret-field,
                'Banco Empresa não cadastrado' TO tg_msg_ret-msg.
          APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wg_cadlan-orig_pgt  IS  INITIAL .
    MOVE: 'Informe Liquidação de pagamento'  TO tg_msg_ret-msg,
          'WG_CADLAN-ORIG_PGT'      TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF NOT 'B_E' CS wg_cadlan-orig_pgt.
    MOVE: 'Liquidação de pagamento inválida'  TO tg_msg_ret-msg,
      'WG_CADLAN-ORIG_PGT'      TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_cadlan-orig_pgt = 'E' AND wg_cadlan-form_pgt IS INITIAL.
    MOVE: 'Informe a forma de pagamento no exterior'  TO tg_msg_ret-msg,
          'WG_CADLAN-FORM_PGT'      TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_cadlan-orig_pgt = 'E' AND wg_cadlan-form_pgt = 'T' AND wg_cadlan-hbkid_e IS INITIAL.
    MOVE: 'Informe o banco empresa no exterior'  TO tg_msg_ret-msg,
          'WG_CADLAN-FORM_PGT'      TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_cadlan-orig_pgt = 'E' AND wg_cadlan-form_pgt = 'T' AND wg_cadlan-tp_oper  EQ '01' AND vg_check_banco NE 'X'.
    MOVE: 'Confirme a C/C fornecedor'  TO tg_msg_ret-msg,
         'WG_CADLAN-BVTYP'      TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_cadlan-hbkid_e IS NOT INITIAL.
    SELECT SINGLE *
       FROM t012k
       INTO @DATA(wl_t012k)
       WHERE bukrs = @wg_cadlan-bukrs
       AND   hbkid = @wg_cadlan-hbkid_e
       AND   waers NE 'BRL'.
    IF sy-subrc NE 0.
      MOVE: 'WG_CADLAN-HBKID_E'  TO tg_msg_ret-field,
            'Banco Empresa no exterior não cadastrado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadlan-dt_prev_liq IS  INITIAL .
    MOVE: 'Informe a data Dt.Prevista p/liquidação '  TO tg_msg_ret-msg,
          'WG_CADLAN-DT_PREV_LIQ'                     TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_cadlan-dt_prev_liq LE wg_cadlan-dt_pgto.
    MOVE: 'Dt.Prevista p/liquidação deve ser maior que Data de Pagamento'  TO tg_msg_ret-msg,
          'WG_CADLAN-DT_PREV_LIQ'                     TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF wg_cadlan-dt_prev_liq GT _dt_liq.
    MOVE: 'Dt.Prevista p/liquidação deve ser menor/igual que 360 dias' TO tg_msg_ret-msg,
          'WG_CADLAN-DT_PREV_LIQ'  TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.


  CLEAR wl_tcurr.
  IF wg_cadlan-moeda_pgto IS  INITIAL .
    MOVE: 'Informe a Moeda de Pagamento '  TO tg_msg_ret-msg,
          'WG_CADLAN-MOEDA_PGTO'           TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
      FROM tcurc
      INTO wl_tcurc
      WHERE waers = wg_cadlan-moeda_pgto.
    IF sy-subrc NE 0.
      MOVE: 'WG_CADLAN-MOEDA_PGTO '  TO tg_msg_ret-field,
            'Moeda de Pagamento não cadastrado' TO tg_msg_ret-msg.
      APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
    ELSEIF wg_cadlan-waers = 'USD' OR wg_cadlan-waers = 'EUR'.
      SELECT *
        FROM tcurr
        INTO TABLE tl_tcurr
        WHERE kurst = 'B'
        AND fcurr   = wg_cadlan-waers
        AND tcurr   = 'BRL'
        ORDER BY gdatu ASCENDING.
      LOOP AT tl_tcurr INTO wl_tcurr.
        CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
          EXPORTING
            input  = wl_tcurr-gdatu
          IMPORTING
            output = wl_gdatu.
        CONCATENATE wl_gdatu+6(4) wl_gdatu+3(2) wl_gdatu+0(2) INTO   wl_tcurr-data.
        MODIFY tl_tcurr FROM wl_tcurr INDEX sy-tabix TRANSPORTING data.

      ENDLOOP.

      IF tl_tcurr[] IS NOT INITIAL.
        SORT tl_tcurr BY data DESCENDING.
        READ TABLE tl_tcurr INTO wl_tcurr INDEX 1.
      ELSE.
        CLEAR wl_tcurr.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA: p_forma_pagamento TYPE  dzlsch,
        p_princ_bnc_emp   TYPE  hbkid.

  "Verificar Banco Empresa e Forma de Pagamento
  IF  wg_cadlan-orig_pgt = 'E' AND wg_cadlan-form_pgt = 'T' AND wg_cadlan-hbkid_e IS NOT INITIAL.
















  ELSE.
    CALL FUNCTION 'Z_RET_FORMA_PAGAMENTO'
      EXPORTING
        p_bukrs           = wg_cadlan-bukrs
        p_lifnr           = wg_cadlan-lifnr
        p_zlsch           = wg_cadlan-zlsch
        p_bvtyp           = wg_cadlan-bvtyp
      IMPORTING
        p_forma_pagamento = p_forma_pagamento
        p_princ_bnc_emp   = p_princ_bnc_emp
      EXCEPTIONS
        nao_fornecedor    = 1
        fornecedor_conta  = 2
        fornecedor_banco  = 3
        faixa_valor       = 4
        banco_empresa     = 5
        OTHERS            = 6.


    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO tg_msg_ret-msg WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      MOVE: 'WG_CADLAN-ZLSCH' TO tg_msg_ret-field.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadlan-orig_pgt NE 'E'.
    IF p_princ_bnc_emp NE wg_cadlan-hbkid.
      CONCATENATE 'Para Forma de Pagamento' p_forma_pagamento 'utilizar banco empresa' p_princ_bnc_emp INTO tg_msg_ret-msg SEPARATED BY space.
      tg_msg_ret-field = 'WG_CADLAN-HBKID'.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  SELECT SINGLE *
  FROM ekko
  INTO wl_ekko
  WHERE ebeln = wg_cadlan-ebeln.


  vsdo_disponivel = 0.
  v_total = 0.
  CLEAR v_mtorg.
  SELECT SINGLE werks
   INTO v_werks
   FROM ekpo
   WHERE ebeln = wl_ekko-ebeln.

  LOOP AT tg_itens INTO wg_itens.
    wl_linha = sy-tabix.
    IF wl_linha = 1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wg_itens-matnr
        IMPORTING
          output = wg_itens-matnr.

      SELECT SINGLE mtorg
        INTO v_mtorg
        FROM mbew
        WHERE matnr = wg_itens-matnr
        AND   bwkey = v_werks.
    ENDIF.
    ADD wg_itens-sdo_disponivel TO vsdo_disponivel.
    ADD wg_itens-vlr_adiantamento TO v_total.
    xvlrad = wg_itens-vlr_adiantamento.

    IF wg_cadlan-waers NE wg_cadlan-moeda_pgto AND wg_cadlan-waers NE 'BRL'.
      IF wl_tcurr-ukurs IS INITIAL.
        CONCATENATE TEXT-e03 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        xvlrad = wg_itens-vlr_adiantamento / wl_tcurr-ukurs.
      ENDIF.
    ENDIF.
    IF xvlrad GT wg_itens-sdo_disponivel AND xvlrad GT 0.
      READ TABLE t_libped WITH KEY from =  wl_ekko-bsart.
      IF sy-subrc NE 0.
        CONCATENATE TEXT-e02 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF v_total LE 0.
    MOVE: 'Informe um VALOR DE ADIANTAMENTO '  TO tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF v_total GT vsdo_disponivel.
    READ TABLE t_libped WITH KEY from =  wl_ekko-bsart.
    IF sy-subrc NE 0.
      MOVE: 'Total adiantamento maior que Saldo Disponível total '  TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ELSEIF 'ZFTE_ZDEF_ZSEM' CS wl_ekko-bsart
         AND ( v_mtorg NE 1 AND v_mtorg NE 2 AND v_mtorg NE 6 AND v_mtorg NE 7 )
         AND ( wg_cadlan-bukrs = '0001' OR wg_cadlan-bukrs = '0015' ).
    c_saldo  = wg_cadlan-saldo.
    REPLACE ALL OCCURRENCES OF '.' IN c_saldo WITH ' '.
    REPLACE ALL OCCURRENCES OF ',' IN c_saldo WITH '.'.
    CONDENSE c_saldo NO-GAPS.
    v_saldo =  c_saldo.
    READ TABLE  t_tcurr_a INTO wa_tcurr INDEX 1.
    IF wg_cadlan-moeda_pgto NE 'USD'.
      v_total = v_total / wa_tcurr-ukurs.
    ENDIF.
    wg_cadlan-adto_ins = ' '.
    IF v_saldo LT v_total.
      wg_cadlan-adto_ins = 'X'.
      wg_cadlan-status   = ' '.
*        MOVE: 'Total adiantamento maior que SALDO ADTO  Disponível  '  TO TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
    ENDIF.

  ENDIF.

  " Início - Valida Pgto Internacional/Importação - DEVK9A1SW7 - #128364 RSA
  IF wg_cadlan-orig_pgt EQ 'E' AND
     wg_cadlan-form_pgt EQ 'T' AND
*     wg_cadlan-zlsch    EQ 'V' AND
     wg_cadlan-tp_oper  EQ '01'.

    SELECT SINGLE *
           FROM lfbk
           INTO @DATA(wl_data_lfbk)
           WHERE lifnr = @wg_cadlan-lifnr.

    IF wg_cadlan-bvtyp IS NOT INITIAL.
      SELECT SINGLE *
             FROM lfbk
             INTO wl_data_lfbk
             WHERE lifnr = wg_cadlan-lifnr
             AND   bvtyp = wg_cadlan-bvtyp.
      IF sy-subrc NE 0.
        SELECT SINGLE *
               FROM lfbk
               INTO wl_data_lfbk
               WHERE lifnr = wg_cadlan-lifnr.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
           FROM bnka
           INTO @DATA(wl_bnka)
           WHERE banks  = @wl_data_lfbk-banks
           AND   bankl  = @wl_data_lfbk-bankl.

    SELECT  *
            FROM tiban
            INTO TABLE @DATA(tl_tiban)
            WHERE banks   = @wl_data_lfbk-banks
            AND   bankl   = @wl_data_lfbk-bankl
            AND   bankn   = @wl_data_lfbk-bankn
            AND   tabname IN ('LFBK', 'BUT0BK')
            ORDER BY erdat DESCENDING.

    LOOP AT tl_tiban INTO DATA(wa_tiban) WHERE NOT iban IS INITIAL.
      vl_check_iban = abap_true.
    ENDLOOP.

    IF wl_bnka-swift IS INITIAL AND
       wl_bnka-brnch IS INITIAL AND
      vl_check_iban EQ abap_false.
      MOVE: 'Não existe SWIFT ou ABA, verificar pgto Internacional/Nacional, se for internacional, cadastrar SWIFT ou ABA no canal bancário'  TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
  " Fim - Valida Pgto Internacional/Importação - DEVK9A1SW7 - #128364 RSA


ENDFORM.                    " F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_obtem_proximo .

  IF wg_cadlan-nro_sol IS INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZFI_31'
      IMPORTING
        number                  = vl_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      wg_cadlan-nro_sol = vl_number.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_dados .
  CHECK wg_cadlan-nro_sol IS NOT INITIAL.

  DATA: wl_input_zfit0045 TYPE zfit0045,
        tl_input_zfit0046 TYPE TABLE OF zfit0046 WITH HEADER LINE,
        tl_zmmt0147       TYPE TABLE OF zmmt0147 WITH HEADER LINE.


  MOVE: sy-mandt                    TO wl_input_zfit0045-mandt,
        wg_cadlan-nro_sol           TO wl_input_zfit0045-nro_sol,
        wg_cadlan-ebeln             TO wl_input_zfit0045-ebeln,
        wg_cadlan-bukrs             TO wl_input_zfit0045-bukrs,
        wg_cadlan-lifnr_p           TO wl_input_zfit0045-lifnr,
        wg_cadlan-dt_pgto           TO wl_input_zfit0045-dt_pgto,
        wg_cadlan-zlsch             TO wl_input_zfit0045-zlsch  ,
        wg_cadlan-bvtyp             TO wl_input_zfit0045-bvtyp  ,
        wg_cadlan-hbkid             TO wl_input_zfit0045-hbkid  ,
        wg_cadlan-identificador     TO wl_input_zfit0045-identificador,
        wg_cadlan-dt_prev_liq       TO wl_input_zfit0045-dt_prev_liq,
        wg_cadlan-moeda_pgto        TO wl_input_zfit0045-moeda_pgto,
        wg_cadlan-motivo            TO wl_input_zfit0045-motivo,
        wg_cadlan-sgtxt             TO wl_input_zfit0045-sgtxt,
        wg_cadlan-resp_neg          TO wl_input_zfit0045-resp_neg,
        wg_cadlan-dep_resp          TO wl_input_zfit0045-dep_resp,
        wg_cadlan-status            TO wl_input_zfit0045-status,
        wg_cadlan-adto_ins          TO wl_input_zfit0045-adto_ins,
        wg_cadlan-orig_pgt          TO wl_input_zfit0045-orig_pgt,
        wg_cadlan-form_pgt          TO wl_input_zfit0045-form_pgt,
        wg_cadlan-hbkid_e           TO wl_input_zfit0045-hbkid_e,
        wg_cadlan-taxa              TO wl_input_zfit0045-taxa,
        wg_cadlan-saldo             TO wl_input_zfit0045-saldo,
        wg_cadlan-limite            TO wl_input_zfit0045-limite,
        sy-uname                    TO wl_input_zfit0045-usnam,
        wg_cadlan-tp_oper           TO wl_input_zfit0045-tp_oper,
        wg_cadlan-nro_sm_se          TO wl_input_zfit0045-nro_sm_se,
        sy-datum                    TO wl_input_zfit0045-dt_atual,
        sy-uzeit                    TO wl_input_zfit0045-hr_atual.

  IF v_orig_pgm =  'ZMMR149'.
    wl_input_zfit0045-nro_sol_cp = wg_cadlan-nro_sol_cp. "BUG 71354* - Anderson Oenning / 12/01/2021
  ELSE.
    wl_input_zfit0045-nro_sol_cp = ''.
  ENDIF.


*** PBI - 60949 - Inicio
  IMPORT v_orig_pgm  FROM MEMORY ID 'ORIG_PROG'.
  DELETE FROM MEMORY ID 'ORIG_PROG'.
*** PBI - 60949 - Fim




  LOOP AT tg_itens INTO wg_itens.
    IF wg_itens-vlr_adiantamento GT 0.
      tl_input_zfit0046-nro_sol           = wl_input_zfit0045-nro_sol.
      tl_input_zfit0046-ebeln             = wl_input_zfit0045-ebeln.
      tl_input_zfit0046-ebelp             = wg_itens-ebelp.
      tl_input_zfit0046-knttp             = wg_itens-knttp.
      tl_input_zfit0046-pstyp             = wg_itens-pstyp.
      tl_input_zfit0046-matnr             = wg_itens-matnr.
      tl_input_zfit0046-aufnr             = wg_itens-aufnr.
      tl_input_zfit0046-anln1             = wg_itens-anln1.
      tl_input_zfit0046-anln2             = wg_itens-anln2.
      tl_input_zfit0046-saldo_item        = wg_itens-saldo_item.
      tl_input_zfit0046-pgtos_real        = wg_itens-pgtos_real.
      tl_input_zfit0046-sdo_disponivel    = wg_itens-sdo_disponivel .
      tl_input_zfit0046-vlr_adiantamento  = wg_itens-vlr_adiantamento.

      IF v_orig_pgm =  'ZMMR149'.
*        tl_input_zfit0046-nro_sol_cp = wl_input_zfit0045-nro_sol.
        tl_input_zfit0046-nro_sol_cp = wg_cadlan-nro_sol_cp. "BUG 71354* - Anderson Oenning / 12/01/2021

*        tl_zmmt0147-nro_sol_cp = wg_cadlan-nro_sol_cp.
*        tl_zmmt0147-ebeln      = wg_cadlan-ebeln.
*        tl_zmmt0147-ebelp      = wg_itens-ebelp.
*        tl_zmmt0147-vlr_adto   = wg_itens-vlr_adiantamento.
*        tl_zmmt0147-dt_vcto    = wg_cadlan-dt_pgto.
*        tl_zmmt0147-dt_atual   = sy-datum.
*        tl_zmmt0147-hr_atual   = sy-uzeit.
*        tl_zmmt0147-usnam      = sy-uname.

      ELSE.
        tl_input_zfit0046-nro_sol_cp = ''.
      ENDIF.

      APPEND tl_input_zfit0046.
      APPEND tl_zmmt0147.




    ENDIF.
    CLEAR: tl_input_zfit0046.

  ENDLOOP.

  "DELETE FROM ZFIT0046 WHERE NRO_SOL = WG_CADLAN-NRO_SOL.
  MODIFY zfit0045 FROM       wl_input_zfit0045.
  MODIFY zfit0046 FROM TABLE tl_input_zfit0046.
  MODIFY zmmt0147 FROM TABLE tl_zmmt0147.
  "128411 - CS2023000924 Modificação tela ZFI-0025 Fornecedor pagamento PSA
  IF wg_cadlan-belnr = abap_false AND wg_cadlan-orig_pgt = 'E' AND wg_cadlan-form_pgt = 'T'.

    SELECT SINGLE * FROM zfit0045 WHERE bukrs = @wg_cadlan-bukrs AND nro_sol = @wg_cadlan-nro_sol INTO @DATA(aux_zfit0045).

    IF wg_cadlan-lifnr_p <> v_forn_paga. " ZFI - Modificação tela ZFI-0025 Forn pagamento - BG #134554
      DELETE FROM zadt_sol_aprov WHERE bukrs = wg_cadlan-bukrs AND nro_sol = wg_cadlan-nro_sol.
      UPDATE zfit0045 SET status = 'L' WHERE bukrs = wg_cadlan-bukrs AND nro_sol = wg_cadlan-nro_sol.
      COMMIT WORK.
    ENDIF.

  ENDIF.

  MESSAGE s836(sd) WITH 'Lançamento' wg_cadlan-nro_sol ', criado/modificado com sucesso!'.

  wg_acao = c_displa.
  PERFORM f_busca_dados.

ENDFORM.                    " F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_limpa_campos .
  CLEAR: wg_cadlan, tg_editor, x_field.
  wg_cadlan-hbkid  = 'BBRA'.
  REFRESH: tg_itens, tg_editor.
ENDFORM.                    " F_LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM f_montar_layout  USING    p_edit.
  REFRESH tg_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ' '          ' '               'TG_ITENS'  'EBELP'            'Item'                '10'  ' '     ' ' ' ' ' ' ' ',
        2   ' '          ' '               'TG_ITENS'  'KNTTP'            'Ctg.contabil'        '10'  ' '     ' ' ' ' ' ' ' ',
        3   ' '          ' '               'TG_ITENS'  'PSTYP'            'Ctg.Item'            '10'  ' '     ' ' ' ' ' ' ' ',
        4   ' '          ' '               'TG_ITENS'  'MATNR'            'Material'            '10'  ' '     ' ' ' ' ' ' ' ',
        5   ' '          ' '               'TG_ITENS'  'TXZ01'            'Descrição'           '30'  ' '     ' ' ' ' ' ' ' ',
        5   ' '          ' '               'TG_ITENS'  'AUFNR'            'Ordem Interna'       '15'  ' '     ' ' ' ' ' ' ' ',
        6   ' '          ' '               'TG_ITENS'  'ANLN1'            'Imobilizado'         '15'  ' '     ' ' ' ' ' ' ' ',
        7   'BSID'       'DMBE2'           'TG_ITENS'  'SALDO_ITEM'       'Saldo Item'          '15'  ' '     'X' ' ' ' ' ' ',
        8   'BSID'       'DMBE2'           'TG_ITENS'  'PGTOS_REAL'       'Pgtos. Moeda PED.'   '15'  ' '     'X' ' ' ' ' ' ',
*        8   'BSID'       'DMBE2'           'TG_ITENS'  'PGTOS_FORT'       'Pgtos.em Dolar'      '15'  ' '     ' ' ' ' ' ' ' ',
        9   'BSID'       'DMBE2'           'TG_ITENS'  'SDO_DISPONIVEL'   'Sdo.Disponivel'      '15'  ' '     'X' ' ' ' ' ' ',
       10   'ZFIT0046'   'VLR_ADIANTAMENTO' 'TG_ITENS'  'VLR_ADIANTAMENTO' 'Vlr.Adiantamento'   '15'  p_edit  'X' ' ' ' ' ' '.


ENDFORM.                    " F_MONTAR_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_montar_estrutura  USING  p_col_pos     p_ref_tabname   p_ref_fieldname
                                p_tabname     p_field         p_scrtext_l
                                p_outputlen   p_edit          p_sum
                                p_emphasize   p_f4            p_ico.

  CLEAR wg_fieldcatalog.
  wg_fieldcatalog-fieldname    = p_field.
  wg_fieldcatalog-tabname      = p_tabname.
  wg_fieldcatalog-ref_table    = p_ref_tabname.
  wg_fieldcatalog-ref_field    = p_ref_fieldname.
  wg_fieldcatalog-key          = ' '.
  wg_fieldcatalog-edit         = p_edit.
  wg_fieldcatalog-do_sum       = p_sum.

  wg_fieldcatalog-col_pos      = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    wg_fieldcatalog-outputlen  = p_outputlen.
  ENDIF.

  wg_fieldcatalog-no_out       = ' '.
  wg_fieldcatalog-reptext      = p_scrtext_l.
  wg_fieldcatalog-scrtext_s    = p_scrtext_l.
  wg_fieldcatalog-scrtext_m    = p_scrtext_l.
  wg_fieldcatalog-scrtext_l    = p_scrtext_l.
  wg_fieldcatalog-emphasize    = p_emphasize.

  APPEND wg_fieldcatalog TO tg_fieldcatalog.
ENDFORM.                    " F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_busca_dados .
  DATA: wl_cont           TYPE sy-tabix,
        wl_cont_aux       TYPE sy-tabix,
        wl_cont_aux2      TYPE sy-tabix,
        w_valor           TYPE komp-netwr,
        w_wmwst           TYPE komp-netwr,
        w_pgtos_real      TYPE zfit0046-pgtos_real,
        w_flag53(1),

        xa_dmbtr          TYPE bsak-dmbtr,
        xa_dmbe2          TYPE bsak-dmbe2,
        xm_dmbtr          TYPE bsak-dmbtr,
        xm_dmbe2          TYPE bsak-dmbe2,
        xt_pr_dmbtr       TYPE bsak-dmbtr,
        xt_pr_dmbe2       TYPE bsak-dmbe2,
        wl_zimp_cad_depto TYPE zimp_cad_depto,
        wl_lfa1           TYPE lfa1,
        v_saldo           TYPE vbak-netwr,
        v_limite          TYPE klimg,

        wa_usr            LIKE v_usr_name,
        wl_ekpa           TYPE ekpa,
        wl_t001           TYPE t001,
        so_item           TYPE RANGE OF ekpo-ebeln,
        wa_item           LIKE LINE OF so_item.

  REFRESH: it_ekkn,
           it_cobrb,
           it_ekbe,
           it_bsak,
           it_bsik,
           it_ekbe_miro,
           it_bsak_miro,
           it_bkpf,
           it_ekpo,
           it_zfit0046.


  IF wg_cadlan-dep_resp IS NOT INITIAL.
    SELECT SINGLE *
        FROM zimp_cad_depto
        INTO wl_zimp_cad_depto
        WHERE dep_resp EQ wg_cadlan-dep_resp .
    IF sy-subrc = 0.
      CLEAR: wa_zfit0053, w_flag53.
      SELECT SINGLE *
       FROM zfit0053
       INTO wa_zfit0053
       WHERE dep_resp = wg_cadlan-dep_resp
       AND   usnam    = sy-uname.
      IF sy-subrc EQ 0.
        w_flag53 = 'X'.
      ENDIF.
      IF wg_cadlan-descricao  NE wl_zimp_cad_depto-dep_resp_desc AND vg_check_banco IS INITIAL.
        wg_acao = c_add.
      ENDIF.
      wg_cadlan-descricao  = wl_zimp_cad_depto-dep_resp_desc.
    ELSE.
      CLEAR wg_cadlan-descricao .
    ENDIF.
  ENDIF.

  IF wg_cadlan-resp_neg IS NOT INITIAL.
    SELECT SINGLE *
      FROM  v_usr_name INTO wa_usr
              WHERE bname = wg_cadlan-resp_neg.

    IF sy-subrc = 0.
      wg_cadlan-resp_neg_n       = wa_usr-name_text.
    ELSE.
      CLEAR wg_cadlan-resp_neg_n.
    ENDIF.

  ENDIF.

  IF wg_cadlan-lifnr_p IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-lifnr_p
      IMPORTING
        output = wg_cadlan-lifnr_p.
    SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
        WHERE lifnr EQ wg_cadlan-lifnr_p.
    IF sy-subrc = 0.
      wg_cadlan-name1_p  = wl_lfa1-name1.
    ELSE.
      CLEAR wg_cadlan-name1_p .
    ENDIF.
  ELSE.
    CLEAR wg_cadlan-name1_p.
  ENDIF.

  PERFORM f_verifica_erros.

  IF wg_acao = c_add. "Novo Lançamento
    CHECK  wg_cadlan-ebeln IS NOT INITIAL.
    CLEAR vg_check_banco.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wg_cadlan-ebeln
      IMPORTING
        output = wg_cadlan-ebeln.

    " Usuários que podem mudar o Status
    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        class         = '0000'
        setnr         = 'MAGGI_ZFI0025_LIBPED'
      TABLES
        set_values    = t_libped
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    SORT t_libped BY from.



    SELECT  *
      FROM  zfit0045
      INTO TABLE it_zfit0045
      WHERE ebeln = wg_cadlan-ebeln
      AND   belnr EQ ''
      AND   loekz EQ ''.

    " se existir documento não pago entao pega tambem para debitar do saldo
    SELECT  *
      FROM  zfit0045
      APPENDING TABLE it_zfit0045
      WHERE ebeln = wg_cadlan-ebeln
      AND   belnr NE ''
      AND   loekz EQ ''
      AND   EXISTS ( SELECT * FROM bsik WHERE bukrs = zfit0045~bukrs AND belnr = zfit0045~belnr  ).

    IF it_zfit0045[] IS NOT INITIAL.
      SELECT *
        FROM zfit0046
        INTO TABLE it_zfit0046
        FOR ALL ENTRIES IN it_zfit0045
        WHERE nro_sol EQ it_zfit0045-nro_sol.
    ENDIF.

    "Solicitação de compras (Saldo Unico)
    SELECT SINGLE *
      FROM zmmt0035
      INTO @DATA(w0035)
      WHERE ebeln = @wg_cadlan-ebeln. " Pedido original

    IF sy-subrc = 0.
      SELECT DISTINCT ebeln
        FROM zmmt0037
        INTO TABLE @DATA(it0037)
      WHERE nro_sol_cp EQ @w0035-nro_sol_cp
      AND   ebeln      NE ''.
      LOOP AT it0037 INTO DATA(w0037).
        wa_item-sign = 'I'.
        wa_item-option = 'EQ'.
        wa_item-low = w0037-ebeln.
        APPEND wa_item  TO so_item.
      ENDLOOP.
    ELSE.
      SELECT SINGLE *
       FROM zmmt0037
       INTO @DATA(wa0037)
       WHERE ebeln = @wg_cadlan-ebeln. " Pedido original

      SELECT DISTINCT ebeln
        FROM zmmt0037
        INTO TABLE @DATA(it0037_)
      WHERE nro_sol_cp EQ @wa0037-nro_sol_cp
      AND   ebeln      NE ''.
      LOOP AT it0037_ INTO DATA(w0037_).
        wa_item-sign = 'I'.
        wa_item-option = 'EQ'.
        wa_item-low = w0037_-ebeln.
        APPEND wa_item  TO so_item.
      ENDLOOP.

    ENDIF.

    wa_item-sign = 'I'.
    wa_item-option = 'EQ'.
    wa_item-low = wg_cadlan-ebeln.
    APPEND wa_item  TO so_item.

    SELECT   ebeln loekz ebelp knttp pstyp  matnr txz01 bukrs werks menge peinh j_1bnbm
      FROM ekpo
      INTO TABLE it_ekpo
      WHERE ebeln  IN so_item
      AND   loekz	 EQ ''.

    CHECK it_ekpo[] IS NOT INITIAL.

    SELECT SINGLE ebeln bsart lifnr waers frgke bukrs
    FROM ekko
    INTO wa_ekko
    WHERE ebeln  EQ wg_cadlan-ebeln.

    IF wa_ekko-frgke NE '2' AND wa_ekko-bsart NE 'NB' AND wa_ekko-bsart+0(1) NE 'Y'.
      MESSAGE 'Pedido não está Liberado' TYPE 'I'.
      PERFORM f_limpa_campos.
      EXIT.
    ENDIF.

    SELECT SINGLE *
      FROM t001
      INTO wl_t001
      WHERE bukrs = wa_ekko-bukrs.

    wg_cadlan-status        = 'L'.
    wg_cadlan-bukrs         = wa_ekko-bukrs.
    wg_cadlan-butxt         = wl_t001-butxt.
    SELECT SINGLE *
        FROM lfa1
        INTO wl_lfa1
        WHERE lifnr = wa_ekko-lifnr.

    wg_cadlan-lifnr         = wa_ekko-lifnr.
    wg_cadlan-name1         = wl_lfa1-name1.

    wg_cadlan-dt_prev_liq   = sy-datum + 90. "Padrão

    IF 'ZFTE_ZSEM_ZDEF_ZEFI' CS wa_ekko-bsart OR wl_lfa1-ktokk = 'ZFEX'. " Insumos ou fornecedores exterior
      wg_cadlan-dt_prev_liq   = sy-datum + 300. "
    ENDIF.

    IF wg_cadlan-lifnr_p IS INITIAL.
      "função parceiro
      SELECT SINGLE *
        FROM ekpa
        INTO wl_ekpa
        WHERE ebeln = wg_cadlan-ebeln
        AND   parvw = 'RS'.

      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM lfa1
          INTO wl_lfa1
          WHERE lifnr = wl_ekpa-lifn2.
        wg_cadlan-lifnr_p         = wl_ekpa-lifn2.
      ELSE.
        SELECT SINGLE *
         FROM lfa1
         INTO wl_lfa1
         WHERE lifnr = wa_ekko-lifnr.
        wg_cadlan-lifnr_p         = wa_ekko-lifnr.
      ENDIF.

      wg_cadlan-name1_p       = wl_lfa1-name1.

      wg_cadlan-moeda_pgto    = wa_ekko-waers.
      wg_cadlan-waers         = wa_ekko-waers.

      SELECT SINGLE *
        FROM  v_usr_name INTO wa_usr
                WHERE bname = sy-uname.

      wg_cadlan-usnam         = sy-uname.
      wg_cadlan-usnam_n       = wa_usr-name_text.
      IF wg_cadlan-icon NE icon_message_critical_small.
        wg_cadlan-icon          = icon_message_warning.
      ENDIF.

      IF 'ZFTE_ZDEF_ZSEM_YFTE' CS wa_ekko-bsart. "USER STORY 164020 - MMSILVA - 03.02.2025 - Incluído tipo "YFTE"
        CALL FUNCTION 'Z_SALDO_ADTOFOR'
          EXPORTING
            v_lifnr  = wa_ekko-lifnr
            i_waers  = wg_cadlan-moeda_pgto
            i_bukrs  = wg_cadlan-bukrs "USER STORY 164020 - MMSILVA - 03.02.2025
          IMPORTING
            v_saldo  = v_saldo
            v_limite = v_limite.
        WRITE v_saldo TO wg_cadlan-saldo.
        WRITE v_limite TO wg_cadlan-limite.
      ENDIF.
    ENDIF.

    REFRESH tg_itens.
    SELECT ebeln ebelp anln1 anln2 aufnr
      FROM ekkn
      INTO TABLE it_ekkn
      FOR ALL ENTRIES IN it_ekpo
      WHERE ebeln EQ  it_ekpo-ebeln
      AND   ebelp EQ  it_ekpo-ebelp.

    IF it_ekkn[] IS NOT INITIAL.
      LOOP AT it_ekkn INTO wa_ekkn.
        CONCATENATE 'OR' wa_ekkn-aufnr  INTO wa_ekkn-objnr.
        MODIFY it_ekkn FROM wa_ekkn INDEX sy-tabix TRANSPORTING objnr.
      ENDLOOP.

      SELECT objnr anln1 anln2
        FROM cobrb
        INTO TABLE it_cobrb
        FOR ALL ENTRIES IN it_ekkn
        WHERE objnr	EQ it_ekkn-objnr.
    ENDIF.

    " Adiantamento
    SELECT ebeln ebelp belnr buzei gjahr wrbtr shkzg reewr
      FROM ekbe
      INTO TABLE it_ekbe
      FOR ALL ENTRIES IN it_ekpo
       WHERE ebeln EQ  it_ekpo-ebeln
       AND   ebelp EQ  it_ekpo-ebelp
       AND   vgabe IN ( '4' , 'C' ).

    SORT  it_ekpo BY ebeln ebelp.
    LOOP AT it_ekbe INTO wa_ekbe.
      tabix = sy-tabix.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln
                                               ebelp = wa_ekbe-ebelp BINARY SEARCH.
      CHECK sy-subrc = 0.
      wa_ekbe-bukrs = wa_ekpo-bukrs.
      MODIFY it_ekbe FROM wa_ekbe INDEX tabix TRANSPORTING bukrs.
    ENDLOOP.

    IF it_ekbe[] IS NOT INITIAL.
      SELECT   bukrs belnr gjahr dmbtr dmbe2 waers
        FROM  bsak
        INTO TABLE it_bsak
        FOR ALL ENTRIES IN it_ekbe
        WHERE bukrs EQ  it_ekbe-bukrs
        AND   belnr EQ  it_ekbe-belnr
        AND   gjahr EQ  it_ekbe-gjahr.

      SELECT   bukrs belnr gjahr dmbtr dmbe2
         FROM  bsik
         APPENDING TABLE it_bsak
         FOR ALL ENTRIES IN it_ekbe
         WHERE bukrs EQ  it_ekbe-bukrs
         AND   belnr EQ  it_ekbe-belnr
         AND   gjahr EQ  it_ekbe-gjahr.
    ENDIF.

    " MIRO
    SELECT ebeln ebelp belnr buzei gjahr wrbtr shkzg reewr
      FROM ekbe
      INTO TABLE it_ekbe_miro
      FOR ALL ENTRIES IN it_ekpo
       WHERE ebeln EQ  it_ekpo-ebeln
       AND   ebelp EQ  it_ekpo-ebelp
       AND   vgabe  = 2.

    SORT it_ekpo       BY ebeln ebelp.
    LOOP AT it_ekbe_miro INTO wa_ekbe.
      tabix = sy-tabix.
      READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln
                                               ebelp = wa_ekbe-ebelp BINARY SEARCH.
      CHECK sy-subrc = 0.
      wa_ekbe-bukrs = wa_ekpo-bukrs.
      CONCATENATE wa_ekbe-belnr wa_ekbe-gjahr INTO wa_ekbe-awkey .
*      IF wa_ekbe-wrbtr NE wa_ekbe-reewr AND wa_ekko-waers = 'BRL'.
*        READ TABLE it_ekpo INTO wa_ekpo WITH KEY ebeln = wa_ekbe-ebeln
*                                                 ebelp = wa_ekbe-ebelp BINARY SEARCH.
*        CLEAR w_wmwst.
*        PERFORM r_imposto_item USING wa_ekko-lifnr
*                                     wa_ekpo-werks
*                                     wa_ekpo-ebelp
*                                     wa_ekpo-ebeln
*                           CHANGING  w_valor
*                                     w_wmwst.
**        IF w_wmwst GT 0.
**          wa_ekbe-wrbtr = wa_ekbe-reewr + w_wmwst.
**          MODIFY it_ekbe_miro FROM wa_ekbe INDEX tabix TRANSPORTING wrbtr.
**        ENDIF.
*
*      ENDIF.
      MODIFY it_ekbe_miro FROM wa_ekbe INDEX tabix TRANSPORTING bukrs awkey.
    ENDLOOP.

    IF it_ekbe_miro[] IS NOT INITIAL.
      SELECT bukrs gjahr awkey belnr
        FROM bkpf
        INTO TABLE it_bkpf
        FOR ALL ENTRIES IN it_ekbe_miro
        WHERE bukrs EQ  it_ekbe_miro-bukrs
        AND   gjahr EQ  it_ekbe_miro-gjahr
        AND   awkey EQ  it_ekbe_miro-awkey.

      IF it_bkpf[] IS NOT INITIAL.
        SELECT   bukrs belnr gjahr dmbtr dmbe2 waers
          FROM  bsak
          INTO TABLE it_bsak_miro
          FOR ALL ENTRIES IN it_bkpf
          WHERE bukrs EQ  it_bkpf-bukrs
          AND   belnr EQ  it_bkpf-belnr
          AND   gjahr EQ  it_bkpf-gjahr.

        SELECT   bukrs belnr gjahr dmbtr dmbe2 zlspr
           FROM  bsik
           INTO TABLE it_bsik
           FOR ALL ENTRIES IN it_bkpf
           WHERE bukrs EQ  it_bkpf-bukrs
           AND   belnr EQ  it_bkpf-belnr
           AND   gjahr EQ  it_bkpf-gjahr.
      ENDIF.
    ENDIF.

    SORT : it_ekkn       BY ebeln ebelp,
           it_cobrb      BY objnr,
           it_ekbe       BY ebeln ebelp,
           it_bsak       BY bukrs belnr gjahr,
           it_bsik       BY bukrs belnr gjahr,
           it_ekbe_miro  BY ebeln ebelp,
           it_bkpf       BY bukrs gjahr awkey,
           it_ekpo       BY ebeln ebelp.


    CLEAR: xt_pr_dmbtr, xt_pr_dmbe2.


*** PBI - 60949 - Inicio
    IMPORT v_orig_pgm  FROM MEMORY ID 'ORIG_PROG'.
    DELETE FROM MEMORY ID 'ORIG_PROG'.

    IMPORT v_vlr_adto  FROM MEMORY ID 'VLR_ADTO'.
    DELETE FROM MEMORY ID 'VLR_ADTO'.

    CLEAR: v_vlr_adto_residual.
    v_vlr_adto_residual = v_vlr_adto.

    LOOP AT it_ekpo INTO wa_ekpo.
      CLEAR wg_itens.
      wg_itens-ebelp             = wa_ekpo-ebelp.
      wg_itens-knttp             = wa_ekpo-knttp.
      wg_itens-pstyp             = wa_ekpo-pstyp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_ekpo-matnr
        IMPORTING
          output = wg_itens-matnr.

      wg_itens-txz01             = wa_ekpo-txz01.
      READ TABLE it_ekkn INTO wa_ekkn WITH KEY ebeln = wa_ekpo-ebeln
                                               ebelp = wa_ekpo-ebelp BINARY SEARCH.
      wg_itens-aufnr             = wa_ekkn-aufnr.
      IF wa_ekpo-knttp EQ 'A'.
        wg_itens-anln1             = wa_ekkn-anln1.
        wg_itens-anln2             = wa_ekkn-anln2.
      ELSEIF wa_ekpo-knttp EQ 'F'.
        READ TABLE it_cobrb INTO wa_cobrb WITH KEY objnr  = wa_ekkn-objnr BINARY SEARCH.
        wg_itens-anln1             = wa_cobrb-anln1.
        wg_itens-anln2             = wa_cobrb-anln2.
      ENDIF.

      PERFORM r_imposto_item USING      wa_ekko-lifnr
                                        wa_ekpo-werks
                                        wa_ekpo-ebelp
                                        wa_ekpo-ebeln
                              CHANGING 	 w_valor
                                         w_wmwst.

      wg_itens-saldo_item        =  w_valor .

      " Adiantamento
      xa_dmbtr = 0.
      xa_dmbe2 = 0.
      LOOP AT it_ekbe INTO wa_ekbe WHERE ebeln = wa_ekpo-ebeln
                                   AND   ebelp = wa_ekpo-ebelp.

        IF wa_ekbe-shkzg = 'S'.
          ADD wa_ekbe-wrbtr TO xa_dmbtr.
          ADD wa_ekbe-wrbtr TO xa_dmbe2.
        ELSE.
          SUBTRACT wa_ekbe-wrbtr FROM xa_dmbtr.
          SUBTRACT wa_ekbe-wrbtr FROM xa_dmbe2.
        ENDIF.
      ENDLOOP.

      " MIROS
      xm_dmbtr = 0.
      xm_dmbe2 = 0.
      CLEAR wl_erro.
      LOOP AT it_ekbe_miro INTO wa_ekbe WHERE ebeln = wa_ekpo-ebeln
                                        AND   ebelp = wa_ekpo-ebelp.

        READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs = wa_ekbe-bukrs
                                                 gjahr = wa_ekbe-gjahr
                                                 awkey = wa_ekbe-awkey.
*        SELECT  SINGLE BUKRS BELNR GJAHR DMBTR DMBE2 ZLSPR
*               FROM BSIK
*               INTO WA_BSIK
*               WHERE BUKRS  = WA_BKPF-BUKRS
*               AND   BELNR  = WA_BKPF-BELNR
*               AND   GJAHR  = WA_BKPF-GJAHR.
*
*        IF SY-SUBRC NE 0.
*          SELECT SINGLE  BUKRS BELNR GJAHR DMBTR DMBE2 WAERS
*            FROM  BSAK
*            INTO WA_BSAK
*              WHERE BUKRS  = WA_BKPF-BUKRS
*               AND   BELNR = WA_BKPF-BELNR
*               AND   GJAHR = WA_BKPF-GJAHR.
*          WA_BSIK-DMBE2 = WA_BSAK-DMBE2.
*        ENDIF.

*         IF WA_EKBE-SHKZG = 'S'.
*          ADD WA_EKBE-WRBTR TO XM_DMBTR.
*          IF SY-SUBRC = 0.
*            ADD WA_BSIK-DMBE2 TO XM_DMBE2.
*          ELSE.
*            ADD WA_EKBE-WRBTR TO XM_DMBE2.
*          ENDIF.
*        ELSE.
*          SUBTRACT WA_EKBE-WRBTR FROM XM_DMBTR.
*          IF SY-SUBRC = 0.
*            SUBTRACT WA_BSIK-DMBE2 FROM XM_DMBE2.
*          ELSE.
*            SUBTRACT WA_EKBE-WRBTR FROM XM_DMBE2.
*          ENDIF.
*        ENDIF.


        IF wa_ekbe-shkzg = 'S'.
          ADD wa_ekbe-wrbtr TO xm_dmbtr.
          ADD wa_ekbe-wrbtr TO xm_dmbe2.
        ELSE.
          SUBTRACT wa_ekbe-wrbtr FROM xm_dmbtr.
          SUBTRACT wa_ekbe-wrbtr FROM xm_dmbe2.
        ENDIF.

        IF w_flag53 = 'X'.
          LOOP AT it_bkpf INTO wa_bkpf WHERE bukrs = wa_ekbe-bukrs
                                       AND   gjahr = wa_ekbe-gjahr
                                       AND   awkey = wa_ekbe-awkey.
            LOOP AT it_bsik    INTO wa_bsik WHERE bukrs  = wa_bkpf-bukrs
                                              AND   belnr  = wa_bkpf-belnr
                                              AND   gjahr  = wa_bkpf-gjahr.
              IF wa_bsik-zlspr = ''.
                wl_erro = 'X'.
                MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Verificar Fatura em aberto sem bloquear.'.
              ELSE.
                SUBTRACT  wa_ekbe-wrbtr FROM xm_dmbtr.
                SUBTRACT  wa_ekbe-wrbtr FROM xm_dmbe2.
*                SUBTRACT WA_BSIK-DMBE2  FROM XM_DMBE2.
              ENDIF.
            ENDLOOP.
          ENDLOOP.
          IF xm_dmbtr LT 0.
            xm_dmbtr = 0.
          ENDIF.
          IF xm_dmbe2 LT 0.
            xm_dmbe2 = 0.
          ENDIF.

        ENDIF.

      ENDLOOP.
      IF wl_erro = 'X'.
        REFRESH  tg_itens.
        EXIT.
      ENDIF.
      xt_pr_dmbtr = xa_dmbtr + xm_dmbtr.
      xt_pr_dmbe2 = xa_dmbe2 + xm_dmbe2.
*      IF WA_EKKO-WAERS = 'BRL'.
      IF wa_ekko-waers NE 'USD'.
*---> 08/06/2023 - Migração S4 - JS
*             WG_ITENS-PGTOS_REAL        = XT_PR_DMBTR.
        wg_itens-pgtos_real = CONV #( xt_pr_dmbtr ).
*<--- 08/06/2023 - Migração S4 - JS

      ELSE.
*---> 08/06/2023 - Migração S4 - JS
*            WG_ITENS-PGTOS_REAL        = XT_PR_DMBE2.
        wg_itens-pgtos_real = CONV #( xt_pr_dmbe2 ).
*<--- 08/06/2023 - Migração S4 - JS


      ENDIF.
      wg_itens-sdo_disponivel    = wg_itens-saldo_item - wg_itens-pgtos_real  .
      wg_itens-vlr_adiantamento  = 0.

*
*      IF v_orig_pgm =  'ZMMR149'.
*
*        IF v_vlr_adto_residual >= wg_itens-sdo_disponivel.
*          wg_itens-vlr_adiantamento = wg_itens-sdo_disponivel.
*          v_vlr_adto_residual = v_vlr_adto_residual - wg_itens-sdo_disponivel.
*        ELSE.
*          IF v_vlr_adto_residual > 0.
*            wg_itens-vlr_adiantamento = v_vlr_adto_residual.
*            v_vlr_adto_residual = v_vlr_adto_residual - wg_itens-sdo_disponivel.
*          ENDIF.
*        ENDIF.
*      ENDIF.

*** PBI - 60949 - Fim

      APPEND wg_itens TO tg_itens.
    ENDLOOP.

    "totaliza pagamentos nao contabilizados
    SORT it_zfit0046 BY ebelp.
    LOOP AT tg_itens INTO wg_itens.
      tabix = sy-tabix.
      w_pgtos_real = 0.
      LOOP AT it_zfit0046 INTO wa_zfit0046 WHERE ebelp = wg_itens-ebelp.
        ADD    wa_zfit0046-vlr_adiantamento TO w_pgtos_real.
      ENDLOOP.
      ADD w_pgtos_real TO wg_itens-pgtos_real .
      wg_itens-sdo_disponivel    = wg_itens-saldo_item - wg_itens-pgtos_real  .

      IF v_orig_pgm =  'ZMMR149'.
        IF v_vlr_adto_residual >= wg_itens-sdo_disponivel.
          wg_itens-vlr_adiantamento = wg_itens-sdo_disponivel.
          v_vlr_adto_residual = v_vlr_adto_residual - wg_itens-sdo_disponivel.
        ELSE.
          IF v_vlr_adto_residual > 0.
            wg_itens-vlr_adiantamento = v_vlr_adto_residual.
            v_vlr_adto_residual = v_vlr_adto_residual - wg_itens-sdo_disponivel.
          ENDIF.
        ENDIF.
      ENDIF.

      MODIFY tg_itens FROM wg_itens INDEX tabix TRANSPORTING pgtos_real sdo_disponivel vlr_adiantamento.
    ENDLOOP.
    IF wg_cadlan-dep_resp IS NOT INITIAL.
      wg_acao = c_modif.
    ENDIF.
  ELSEIF wg_acao = c_displa.
    FREE: tg_itens.
    CHECK wg_cadlan-nro_sol  IS NOT INITIAL.

    SELECT SINGLE *
      FROM  zfit0045
      INTO wa_zfit0045
      WHERE nro_sol EQ wg_cadlan-nro_sol.

    "if wl_zfit0045-LIFNR is NOT INITIAL.
    v_forn_paga = wa_zfit0045-lifnr.
    "  endif.

    SELECT *
      FROM  zfit0046
      INTO TABLE it_zfit0046
      WHERE nro_sol EQ wg_cadlan-nro_sol.

    CHECK sy-subrc = 0.

    SELECT SINGLE ebeln bsart lifnr waers frgke bukrs
    FROM ekko
    INTO wa_ekko
    WHERE ebeln  EQ wa_zfit0045-ebeln.


    IF wa_zfit0045-belnr IS INITIAL.
      wg_cadlan-icon = icon_message_warning.
    ELSE.
      wg_cadlan-icon = icon_checked.
    ENDIF.

    IF wa_zfit0045-status = 'B' OR wa_zfit0045-adto_ins = 'X'.
      wg_cadlan-icon = icon_locked.
    ENDIF.

    IF wa_zfit0045-status = 'L'.
      wg_cadlan-icon = icon_release.
    ENDIF.

    IF wa_zfit0045-loekz = 'X'.
      wg_cadlan-icon = icon_delete.
    ENDIF.

    wg_cadlan-status        = wa_zfit0045-status.
    wg_cadlan-belnr         = wa_zfit0045-belnr.
    wg_cadlan-ebeln         = wa_zfit0045-ebeln.
    wg_cadlan-zlsch         = wa_zfit0045-zlsch.
    wg_cadlan-hbkid         = wa_zfit0045-hbkid.
    wg_cadlan-identificador = wa_zfit0045-identificador.
    wg_cadlan-bvtyp         = wa_zfit0045-bvtyp.
    wg_cadlan-dt_pgto       = wa_zfit0045-dt_pgto.
    wg_cadlan-dt_prev_liq   = wa_zfit0045-dt_prev_liq.
    wg_cadlan-motivo        = wa_zfit0045-motivo.
    wg_cadlan-sgtxt         = wa_zfit0045-sgtxt.
    wg_cadlan-resp_neg      = wa_zfit0045-resp_neg.
    wg_cadlan-dep_resp      = wa_zfit0045-dep_resp.
    wg_cadlan-orig_pgt      = wa_zfit0045-orig_pgt.
    wg_cadlan-form_pgt      = wa_zfit0045-form_pgt.
    wg_cadlan-hbkid_e       = wa_zfit0045-hbkid_e.
    wg_cadlan-tp_oper       = wa_zfit0045-tp_oper.
    wg_cadlan-nro_sm_se     = wa_zfit0045-nro_sm_se.
    wg_cadlan-saldo         = wa_zfit0045-saldo.
    wg_cadlan-limite        = wa_zfit0045-limite.
    wg_cadlan-nro_sol_cp    = wa_zfit0045-nro_sol_cp.


    SELECT SINGLE *
      FROM  v_usr_name INTO wa_usr
              WHERE bname = wg_cadlan-resp_neg.

    IF sy-subrc = 0.
      wg_cadlan-resp_neg_n       = wa_usr-name_text.
    ELSE.
      CLEAR wg_cadlan-resp_neg_n.
    ENDIF.

    wg_cadlan-dep_resp      = wa_zfit0045-dep_resp.
    SELECT SINGLE *
       FROM zimp_cad_depto
       INTO wl_zimp_cad_depto
       WHERE dep_resp EQ wg_cadlan-dep_resp .
    IF sy-subrc = 0.
      wg_cadlan-descricao  = wl_zimp_cad_depto-dep_resp_desc.
    ELSE.
      CLEAR wg_cadlan-descricao .
    ENDIF.

    SELECT SINGLE *
      FROM t001
      INTO wl_t001
      WHERE bukrs = wa_zfit0045-bukrs.

    wg_cadlan-bukrs         = wa_zfit0045-bukrs.
    wg_cadlan-butxt         = wl_t001-butxt.

    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
      WHERE lifnr = wa_ekko-lifnr.

    wg_cadlan-lifnr         = wa_ekko-lifnr.
    wg_cadlan-name1         = wl_lfa1-name1.

    SELECT SINGLE *
      FROM lfa1
      INTO wl_lfa1
      WHERE lifnr = wa_zfit0045-lifnr.

    wg_cadlan-lifnr_p       = wa_zfit0045-lifnr.
    wg_cadlan-name1_p       = wl_lfa1-name1.


    wg_cadlan-moeda_pgto    = wa_zfit0045-moeda_pgto.
    wg_cadlan-waers         = wa_ekko-waers.
    wg_cadlan-usnam         = wa_zfit0045-usnam.

    LOOP AT it_zfit0046 INTO wa_zfit0046.
      SELECT  SINGLE ebeln loekz ebelp knttp pstyp  matnr txz01 bukrs werks menge peinh j_1bnbm
        FROM ekpo
        INTO wa_ekpo
        WHERE ebeln  EQ wa_zfit0045-ebeln
        AND   ebelp  EQ wa_zfit0046-ebelp.
      wg_itens-ebelp             = wa_zfit0046-ebelp.
      wg_itens-knttp             = wa_zfit0046-knttp.
      wg_itens-pstyp             = wa_zfit0046-pstyp.
      wg_itens-matnr             = wa_zfit0046-matnr.
      wg_itens-txz01             = wa_ekpo-txz01.
      wg_itens-aufnr             = wa_zfit0046-aufnr.
      wg_itens-anln1             = wa_zfit0046-anln1.
      wg_itens-anln2             = wa_zfit0046-anln2.
      wg_itens-saldo_item        = wa_zfit0046-saldo_item.
      wg_itens-pgtos_real        = wa_zfit0046-pgtos_real.
      wg_itens-sdo_disponivel    = wg_itens-saldo_item - wg_itens-pgtos_real  .
      wg_itens-vlr_adiantamento  = wa_zfit0046-vlr_adiantamento.
      wg_itens-nro_sol_cp        = wa_zfit0046-nro_sol_cp.
      APPEND wg_itens TO tg_itens.
    ENDLOOP.
    wg_acao = c_modif.
  ENDIF.
  PERFORM f_verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
      i_show        = space   "c_x
      i_repid       = sy-repid
      i_pressed_tab = ' '
      i_set_field   = 'X_FIELD'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.
ENDFORM.                    " F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  R_IMPOSTO_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM r_imposto_item USING     w_lifnr
                              w_werks
                              w_ebelp
                              w_ebeln
                    CHANGING  w_valor
                              w_wmwst.

  DATA: wa_ite  LIKE mepoitem.
  CLEAR wa_ite.
  CALL FUNCTION 'MEPO_DOC_ITEM_GET'
    EXPORTING
      im_ebelp = w_ebelp                                    "'00010'
    IMPORTING
      ex_item  = wa_ite
    EXCEPTIONS
      failure  = 1
      OTHERS   = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA: BEGIN OF t_konv OCCURS 0.
          INCLUDE STRUCTURE konv.
  DATA: END OF t_konv.

  TYPES: ty_konv TYPE TABLE OF komv.

  FIELD-SYMBOLS: <wmwst> TYPE any,
                 <lfa1>  TYPE lfa1,
                 <ekpo>  TYPE ekpo,
                 <ek2>   TYPE ekpo,
                 <ekko>  TYPE ekko,
                 <vorga> TYPE any,
                 <konv>  TYPE ty_konv,
                 <cva>   TYPE any.

  ASSIGN ('(SAPLMEPO)ekpo') TO <ekpo>.
  ASSIGN ('(SAPLMEPO)ekko') TO <ekko>.
  ASSIGN ('(SAPLMEPO)lfa1') TO <lfa1>.


  SELECT SINGLE * FROM ekpo INTO <ekpo>
    WHERE ebeln = w_ebeln AND
          ebelp = w_ebelp AND
          loekz = space.


  SELECT SINGLE * FROM ekko INTO <ekko>
    WHERE ebeln = w_ebeln.

  SELECT SINGLE * FROM lfa1 INTO <lfa1>
    WHERE lifnr = <ekko>-lifnr.


  TRY.

      cl_prc_result_factory=>get_instance( )->get_prc_result( )->get_price_element_db(
        EXPORTING it_selection_attribute = VALUE #(
       ( fieldname = 'KNUMV' value = <ekko>-knumv )
       )
        IMPORTING et_prc_element_classic_format = DATA(etl1459c2r7367) ).
      t_konv[] = etl1459c2r7367.
    CATCH cx_prc_result .
      sy-subrc = 4.
  ENDTRY.

  ASSIGN ('(SAPLMEPO)fc_vorga') TO <vorga>.
  ASSIGN ('(SAPLMEPO)cva_en') TO <cva>.
  ASSIGN ('(SAPLMEPO)tkomv[]') TO <konv>.

  <vorga> = <cva>.

  PERFORM kond_taxes(saplmepo) USING 'D' 'X'.

  CHECK <ekpo>-loekz = space.
  ASSIGN ('(SAPLMEPO)taxcom-WMWST') TO <wmwst>.


  DATA: w_netwr  TYPE komp-netwr.

  w_netwr = <ekpo>-netwr.
  w_wmwst  =  <wmwst>.
  w_valor  = ( w_netwr + <wmwst> ).

ENDFORM.                    " R_IMPOSTO_ITEM
*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_eliminar_lancamento .
  DATA: wl_zfit0045 TYPE zfit0045.

  SELECT  SINGLE *
    FROM zfit0045
    INTO wl_zfit0045
     WHERE nro_sol EQ wg_cadlan-nro_sol.

  IF sy-subrc IS INITIAL.
    IF  wl_zfit0045-belnr IS NOT INITIAL.
      SELECT SINGLE *
        FROM bkpf
        INTO @DATA(w_bkpf)
        WHERE bukrs EQ @wl_zfit0045-bukrs
        AND   belnr EQ @wl_zfit0045-belnr
        AND   stblg NE ''.

      IF sy-subrc NE 0.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                                                'contabil não esta estornado!'.
        EXIT.
      ENDIF.

    ENDIF.
    IF wl_zfit0045-loekz IS INITIAL.
      MOVE: c_x TO wl_zfit0045-loekz.
      MODIFY zfit0045 FROM wl_zfit0045.
      wg_cadlan-icon = icon_delete.
      MESSAGE s836(sd) WITH 'O documento foi eliminado!'.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                            'já foi marcado para eliminação!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " F_ELIMINAR_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  F_BLOQUEIO_LANCAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_bloqueio_lancamento .
  DATA: wl_zfit0045 TYPE zfit0045.

  SELECT  SINGLE *
    FROM zfit0045
    INTO wl_zfit0045
     WHERE nro_sol EQ wg_cadlan-nro_sol.

  IF sy-subrc IS INITIAL.
    IF wl_zfit0045-status IS INITIAL.
      MOVE: 'B' TO wl_zfit0045-status.
      MODIFY zfit0045 FROM wl_zfit0045.
      MESSAGE s836(sd) WITH 'O documento foi Bloqueado'.
      wg_cadlan-icon = icon_locked.
    ELSE.
      MOVE: '' TO wl_zfit0045-status.
      MODIFY zfit0045 FROM wl_zfit0045.
      MESSAGE s836(sd) WITH 'O documento foi Desbloqueado'.
      IF wl_zfit0045-belnr IS INITIAL.
        wg_cadlan-icon = icon_message_warning.
      ELSE.
        wg_cadlan-icon = icon_checked.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_BLOQUEIO_LANCAMENTO
*&---------------------------------------------------------------------*
*&      Form  HABILITAR_WORKFLOW_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM habilitar_workflow_documentos .
  REFRESH: it_seq_lcto.
  CLEAR: it_seq_lcto.

  "Somente validar acesso para modificar
  IF wg_cadlan-nro_sol IS NOT INITIAL.
    it_seq_lcto-seq_lcto = wg_cadlan-nro_sol .
    APPEND it_seq_lcto.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_LIBERA_INS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_libera_ins .
  DATA: object  TYPE sibflporb,
        lt_stat TYPE sgs_t_acnt.

  SELECT  SINGLE *
    FROM zfit0045
    INTO @DATA(w_zfit0045)
     WHERE nro_sol EQ @wg_cadlan-nro_sol.

  IF sy-subrc IS INITIAL.
    IF w_zfit0045-status = 'A'.
      MESSAGE 'Documento já Aprovado!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF w_zfit0045-status = 'L'.
      MESSAGE 'Documento já Liberado!' TYPE 'I'.
      EXIT.
    ENDIF.
    IF w_zfit0045-adto_ins = ' '.
      MESSAGE 'Documento não ultrapassou limite de insumos!' TYPE 'I'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE 'Documento não gravado!' TYPE 'I'.
    EXIT.
  ENDIF.

  object-typeid = 'ZFIR0031'.
  CONCATENATE sy-mandt wg_cadlan-nro_sol INTO object-instid.
  MOVE 'BO' TO object-catid.
  REFRESH  lt_stat.
  CALL METHOD cl_gos_attachment_query=>count_for_object
    EXPORTING
      is_object = object
      ip_arl    = space
    RECEIVING
      rt_stat   = lt_stat.
  IF lt_stat[] IS INITIAL.
    MESSAGE 'Por favor, incluir ANEXO' TYPE 'I'.
    EXIT.
  ENDIF.

  UPDATE zfit0045 SET status   = 'L',
                      adto_ins = ' '
  WHERE nro_sol EQ @wg_cadlan-nro_sol.

  COMMIT WORK.

  MESSAGE s836(sd) WITH 'Lançamento' wg_cadlan-nro_sol ', liberado com sucesso!'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form init_container
*&---------------------------------------------------------------------*
FORM init_container .
  IF go_container IS NOT BOUND.
    CREATE OBJECT go_container
      EXPORTING
        container_name              = 'CC_CONTAS'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    PERFORM init_alv_popup.
    PERFORM build_fcat.
    PERFORM show_popup_data.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form init_alv_popup
*&---------------------------------------------------------------------*
FORM init_alv_popup .
  IF go_alv_popup IS NOT BOUND.
    CREATE OBJECT go_alv_popup
      EXPORTING
*       i_shellstyle      = 0
*       i_lifetime        =
        i_parent          = go_container
*       i_appl_events     = space
*       i_parentdbg       =
*       i_applogparent    =
*       i_graphicsparent  =
*       i_name            =
*       i_fcat_complete   = SPACE
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_fcat
*&---------------------------------------------------------------------*
FORM build_fcat .
  REFRESH tg_fieldcatalog.
  PERFORM montar_estrutura USING:
         1 ' '           ' '        'TG_CONTA' 'BVTYP'           'Seq.'(314)         '08' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKS'           'País'(315)         '10' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKL'           'Banco'(135)        '10' 'X' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKN'           'Cta.Bancária'(316) '18' 'X' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BKONT'           'CC'(317)           '02' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'IBAN'            'IBAN'(318)         '30' 'X' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'SWIFT'           'SWIFT'(319)        '10' ' ' ' ' ' ',
         1 ' '           ' '        'TG_CONTA' 'BANKA'           'Nome Banco'(320)   '20' ' ' ' ' ' '.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
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

  CLEAR wg_fieldcatalog.
  wg_fieldcatalog-fieldname     = p_field.
  wg_fieldcatalog-tabname       = p_tabname.
  wg_fieldcatalog-ref_table     = p_ref_tabname.
  wg_fieldcatalog-ref_field     = p_ref_fieldname.
  wg_fieldcatalog-key           = ' '.
  wg_fieldcatalog-edit          = p_edit.
  wg_fieldcatalog-do_sum        = p_sum.

  wg_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    wg_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.


  wg_fieldcatalog-no_out        = ' '.
  wg_fieldcatalog-reptext       = p_scrtext_l.
  wg_fieldcatalog-scrtext_s     = p_scrtext_l.
  wg_fieldcatalog-scrtext_m     = p_scrtext_l.
  wg_fieldcatalog-scrtext_l     = p_scrtext_l.
  wg_fieldcatalog-emphasize     = p_emphasize.

  APPEND wg_fieldcatalog TO tg_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*& Form show_popup_data
*&---------------------------------------------------------------------*
FORM show_popup_data .
  DATA: tl_function TYPE ui_functions,
        wl_function LIKE LINE OF tl_function.

  CLEAR wa_layout.
  CREATE OBJECT obg_toolbar
    EXPORTING
      io_alv_grid = go_alv_popup.
*      * Register event handler
  SET HANDLER obg_toolbar->on_toolbar FOR go_alv_popup.
  SET HANDLER obg_toolbar->handle_user_command_cta FOR go_alv_popup.

  wa_layout-no_toolbar = space.
  wa_layout-zebra      = c_x.
  wa_stable-row        = c_x.
  wa_layout-grid_title = ''.
  wa_layout-no_toolbar = ''.

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

  CALL METHOD go_alv_popup->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      it_toolbar_excluding          = tl_function
    CHANGING
      it_outtab                     = tg_conta
      it_fieldcatalog               = tg_fieldcatalog
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD go_alv_popup->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
         lcl_event_handler=>on_data_changed_cta          FOR go_alv_popup.

ENDFORM.
