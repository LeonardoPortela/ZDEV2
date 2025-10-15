FUNCTION z_fi_compensar.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_KURSF) TYPE  BKPF-KURSF OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_BLNTAB) TYPE  BLNTAB
*"     REFERENCE(MSG_TEXT) TYPE  STRING
*"  TABLES
*"      IT_COMPENSAR STRUCTURE  ZDE_DOC_VALOR
*"      IT_ZGLT036 STRUCTURE  ZGLT036
*"----------------------------------------------------------------------

  DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
        l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
        l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately


  DATA: lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE,
        lt_ftclear TYPE STANDARD TABLE OF ftclear WITH HEADER LINE,
        lt_ftpost  TYPE STANDARD TABLE OF ftpost  WITH HEADER LINE,
        lt_fttax   TYPE STANDARD TABLE OF fttax   WITH HEADER LINE,
        v_sgtxt    TYPE bseg-sgtxt,
        lds_return TYPE bapiret2.

  DATA: obj_zcl_util_sd  TYPE REF TO zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  DATA: vdata(10),
        vdata_venc(10),
        i_data         TYPE gdatu_inv,
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        w_erro(1),
        wl_vlrn        TYPE p DECIMALS 2,
        vcampo(15),
        v_wrbtr        TYPE bseg-wrbtr,
        v_dmbtr        TYPE bseg-dmbtr,

        v_wrbtr_t      TYPE bseg-wrbtr,
        v_dmbtr_t      TYPE bseg-dmbtr,
        v_dmbe2_t      TYPE bseg-dmbe2,

        v_reskx        TYPE bseg-dmbtr,
        v_resdx        TYPE bseg-dmbtr,
        v_wrbtr_var_b  TYPE bseg-wrbtr,
        v_wrbtr_vark   TYPE bseg-wrbtr,
        v_wrbtr_vard   TYPE bseg-wrbtr,
        v_wrbtr_resk   TYPE bseg-wrbtr,
        v_wrbtr_resd   TYPE bseg-wrbtr,
        v_vlr_moeda    TYPE bseg-wrbtr,
        v_bukrs        TYPE bseg-bukrs,
        v_waers_bco    TYPE bkpf-waers,
        v_waers_doc    TYPE bkpf-waers,
        v_hkont        TYPE bseg-hkont,
        v_hkont_var    TYPE bseg-hkont,
        v_vbund_k      TYPE bseg-vbund,
        v_vbund_d      TYPE bseg-vbund,
        v_lifnr        TYPE lfa1-lifnr,
        v_kunnr        TYPE kna1-kunnr,
        v_zfbdt_resk   TYPE dzfbdt,
        v_zfbdt_resd   TYPE dzfbdt,
        v_dt_vc        TYPE dzfbdt,
        v_gsber        TYPE bseg-gsber,
        v_zuonr        TYPE bseg-zuonr,
        account_name   TYPE skat-txt20,
        parid_name     TYPE lfa1-name1,

        msg_no         TYPE t100-msgnr,
        p_mode         LIKE rfpdo-allgazmd,
        vl_dt_mov      TYPE sy-datum,
        vl_dt_vct      TYPE sy-datum,
        count_ft       TYPE ftpost-count,
        v_xsimu        TYPE char1.

  DATA: obj_cotacao TYPE REF TO zcl_util_sd,
        lc_ukurs    TYPE ukurs_curr,
        lc_ukurs2   TYPE ukurs_curr,
        lc_data     TYPE gdatu_inv.

  DATA: xsplit(1),
        wa_split1 TYPE bseg,
        wa_split2 TYPE bseg.

  DATA: v_considera_zbd1t TYPE c. "verifica se será necessário atribuir valor para o campo BSEG-ZBD1T

  CLEAR e_blntab.

  CLEAR: lt_blntab,   lt_blntab[],
         lt_ftclear,  lt_ftclear[],
         lt_ftpost,   lt_ftpost[],
         lt_fttax,    lt_fttax[],

         lds_return.

  CLEAR: v_wrbtr,v_dmbtr,v_wrbtr_t,v_dmbtr_t, v_dmbe2_t, v_wrbtr_resk, v_wrbtr_resd, v_zfbdt_resk, v_zfbdt_resd, v_gsber, v_waers_doc, lc_ukurs,lc_ukurs2,  v_wrbtr_vark, v_wrbtr_vard, v_reskx, v_resdx, v_sgtxt, v_wrbtr_var_b.
  LOOP AT it_compensar INTO DATA(wa_comp2).

    v_bukrs = wa_comp2-bukrs.
    v_hkont = wa_comp2-konto.
    vl_dt_mov = wa_comp2-budat.
    CONCATENATE  vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) INTO vdata SEPARATED BY '.'.

    v_hkont_var = wa_comp2-saknr_variacao_b.

    IF wa_comp2-waers_b IS INITIAL.
      v_waers_bco = wa_comp2-waers.
    ELSE.
      v_waers_bco = wa_comp2-waers_b.
    ENDIF.
    IF v_waers_doc IS INITIAL.
      SELECT SINGLE waers
       FROM bkpf
       INTO ( v_waers_doc )
       WHERE bukrs = wa_comp2-bukrs
       AND   belnr = wa_comp2-belnr
       AND   gjahr = wa_comp2-gjahr.
      " Taxa do documento NOVO
      DATA etl120c6r374 TYPE TABLE OF bseg.
      DATA lt_fields_l120c6r5899 TYPE fagl_t_field.
      lt_fields_l120c6r5899 = VALUE #( ( line = 'DMBTR' )
       ( line = 'DMBE2' )
       ( line = 'WRBTR' )
       ).
      DATA rldnr_l120c6r6443 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l120c6r6443
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr      = rldnr_l120c6r6443
            i_bukrs      = wa_comp2-bukrs
            i_belnr      = wa_comp2-belnr
            i_gjahr      = wa_comp2-gjahr
            i_buzei      = wa_comp2-buzei
            it_fieldlist = lt_fields_l120c6r5899
          IMPORTING
            et_bseg      = etl120c6r374
          EXCEPTIONS
            not_found    = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl120c6r374 ) = 1.
        v_dmbtr_t = etl120c6r374[ 1 ]-dmbtr.
        v_dmbe2_t = etl120c6r374[ 1 ]-dmbe2.
        v_wrbtr_t = etl120c6r374[ 1 ]-wrbtr.
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.


      lc_ukurs  = v_dmbtr_t / v_wrbtr_t.
      lc_ukurs2 = v_dmbe2_t / v_wrbtr_t.
    ENDIF.
    IF  wa_comp2-koart = 'K'.
      SELECT SINGLE vbund INTO v_vbund_k
        FROM lfa1
        WHERE lifnr = wa_comp2-parid.
    ELSE.
      SELECT SINGLE vbund INTO v_vbund_d
                FROM kna1
                WHERE kunnr = wa_comp2-parid.
    ENDIF.
    IF v_waers_bco = v_waers_doc."mesma moeda, residual fica no mesmo documento
      CLEAR xsplit.
      IF  wa_comp2-koart = 'K'.
        IF wa_comp2-shkzg = 'H'.
          ADD wa_comp2-dmbtr     TO v_wrbtr.
        ELSE.
          SUBTRACT wa_comp2-dmbtr FROM v_wrbtr.
        ENDIF.
        v_lifnr = wa_comp2-parid.

      ELSE.
        IF wa_comp2-shkzg = 'H'.
          ADD wa_comp2-dmbtr     TO v_wrbtr.
        ELSE.
          SUBTRACT wa_comp2-dmbtr     FROM v_wrbtr.
        ENDIF.
        v_kunnr = wa_comp2-parid.
      ENDIF.
    ELSE. " outra moeda, residual fica em outro documento
      xsplit = 'X'.
      IF  wa_comp2-koart = 'K'.
        IF wa_comp2-shkzg = 'H'.
          v_wrbtr = v_wrbtr + abs( wa_comp2-valor_payments_a ).
          v_dmbtr = v_dmbtr + abs( wa_comp2-valor_payments_b ).
        ELSE.
          v_wrbtr = v_wrbtr - abs( wa_comp2-valor_payments_a ).
          v_dmbtr = v_dmbtr - abs( wa_comp2-valor_payments_b ).
        ENDIF.

        IF wa_comp2-dmbtr_res = 0.
          ADD wa_comp2-valor_variacao_a  TO v_wrbtr_vark.
        ELSE.
          ADD wa_comp2-valor_variacao_c  TO v_wrbtr_vark.
        ENDIF.

        v_lifnr = wa_comp2-parid.
      ELSE.
        IF wa_comp2-shkzg = 'H'.
          v_wrbtr = v_wrbtr + abs( wa_comp2-valor_payments_a ).
          v_dmbtr = v_dmbtr + abs( wa_comp2-valor_payments_b ).
        ELSE.
          v_wrbtr = v_wrbtr - abs( wa_comp2-valor_payments_a ).
          v_dmbtr = v_dmbtr - abs( wa_comp2-valor_payments_b ).
        ENDIF.

        IF wa_comp2-dmbtr_res = 0.
          ADD wa_comp2-valor_variacao_a  TO v_wrbtr_vard.
        ELSE.
          ADD wa_comp2-valor_variacao_c  TO v_wrbtr_vard.
        ENDIF.

        v_kunnr = wa_comp2-parid.
      ENDIF.
    ENDIF.

    IF v_gsber IS INITIAL.
* ---> S4 Migration - 16/06/2023 - JV
*      SELECT SINGLE gsber
*        INTO v_gsber
*        FROM bseg
*        WHERE bukrs = wa_comp2-bukrs
*        AND   belnr = wa_comp2-belnr
*        AND   gjahr = wa_comp2-gjahr
*        AND   gsber NE ''.

      DATA:   lt_bseg TYPE fagl_t_bseg.

      CALL FUNCTION 'FAGL_GET_BSEG'
        EXPORTING
          i_bukrs = wa_comp2-bukrs
          i_belnr = wa_comp2-belnr
          i_gjahr = wa_comp2-gjahr
        IMPORTING
          et_bseg = lt_bseg
        EXCEPTIONS
          OTHERS  = 2.

      DELETE lt_bseg WHERE gsber EQ ''.

      READ TABLE lt_bseg INTO DATA(wa_bseg) INDEX 1.

      IF sy-subrc EQ 0.
        v_gsber = wa_bseg-gsber.
      ENDIF.
* <--- S4 Migration - 16/06/2023 - JV
    ENDIF.

  ENDLOOP.


  CLEAR wa_split2.
  IF xsplit = 'X'.
    LOOP AT it_compensar INTO wa_comp2.
      IF wa_comp2-dmbtr_res EQ 0.
        CONTINUE.
      ENDIF.
      MOVE-CORRESPONDING wa_comp2 TO wa_split1.
      wa_split1-dmbtr = wa_comp2-dmbtr_res.
      wa_split1-lifnr = wa_comp2-parid.

      PERFORM f_bapi_f51 USING   wa_split1
                                 vl_dt_mov
                                 wa_comp2-zfbdt_res
                        CHANGING wa_split2
                                 msg_text.
      IF wa_split2-belnr IS NOT INITIAL .
        READ TABLE it_compensar INTO wa_comp2 WITH KEY belnr = wa_split1-belnr.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING wa_split2 TO wa_comp2.
          MODIFY it_compensar FROM wa_comp2 INDEX sy-tabix TRANSPORTING bukrs belnr buzei gjahr.
        ENDIF.
      ELSE.
        w_erro = 'X'.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF w_erro = 'X'.
      EXIT.
    ENDIF.

  ENDIF.


  p_mode = 'N'.

  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = p_mode
      i_update           = 'S'
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc NE 0.
    ROLLBACK WORK.
    MESSAGE 'Houve ao efeturar a compensação' TYPE 'S'.
    RETURN.
  ENDIF.

  count_ft = 1.

  lt_ftpost-stype = 'K'."Header
  lt_ftpost-count = count_ft.  "number of Dynpro

  lt_ftpost-fnam = 'BKPF-BUKRS'.
  lt_ftpost-fval = v_bukrs.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-WAERS'.
  IF  xsplit IS INITIAL.
    lt_ftpost-fval = v_waers_doc.
  ELSE.
    lt_ftpost-fval = v_waers_bco.
  ENDIF.

  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLDAT'.
  lt_ftpost-fval =  vdata.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BUDAT'.
  lt_ftpost-fval = vdata.
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-MONAT'.
  lt_ftpost-fval =  vl_dt_mov+4(2).
  APPEND lt_ftpost.

  lt_ftpost-fnam = 'BKPF-BLART'.
  lt_ftpost-fval = 'ZZ'.
  APPEND lt_ftpost.

**
*
  "
  SELECT SINGLE ktopl
    FROM t001
    INTO @DATA(v_ktopl)
    WHERE bukrs = @v_bukrs.

  SELECT SINGLE waers
  FROM t001
  INTO @DATA(v_waers)
  WHERE bukrs = @v_bukrs.
  "
  LOOP AT it_zglt036 INTO DATA(wa_zglt036).
    IF  wa_zglt036-hkont = v_hkont. " partida banco agrupa valor
      SELECT SINGLE shkzg
        INTO @DATA(v_shkzg)
        FROM tbsl
        WHERE bschl = @wa_zglt036-bschl.
      IF v_shkzg = 'H'.
        ADD wa_zglt036-vlr_moeda_doc     TO v_wrbtr.
*        IF v_waers_bco NE v_waers_doc.
*          IF v_waers_bco = 'USD'.
*            ADD wa_zglt036-vlr_moeda_forte     TO v_dmbtr.
*          ELSE.
*            ADD wa_zglt036-vlr_moeda_int     TO v_dmbtr.
*          ENDIF.
*        ENDIF.
      ELSE.
        SUBTRACT wa_zglt036-vlr_moeda_doc    FROM v_wrbtr.
*        IF v_waers_bco NE v_waers_doc.
*          IF v_waers_bco = 'USD'.
*            SUBTRACT wa_zglt036-vlr_moeda_forte   FROM v_dmbtr.
*          ELSE.
*            SUBTRACT wa_zglt036-vlr_moeda_int     FROM v_dmbtr.
*          ENDIF.
*        ENDIF.
      ENDIF.

      CONTINUE.
    ENDIF.

    ADD 1 TO count_ft.
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft.

    IF wa_zglt036-sgtxt IS INITIAL.
      SELECT SINGLE *
             FROM t003t
             INTO @DATA(wa_t003t)
             WHERE spras = @sy-langu
             AND   blart = 'ZZ'.
      IF sy-subrc = 0.
        wa_zglt036-sgtxt     = wa_t003t-ltext.
        "
        SELECT SINGLE txt20
          FROM skat
          INTO account_name
         WHERE spras = sy-langu
           AND ktopl = v_ktopl
           AND saknr = wa_zglt036-hkont.
        CONCATENATE wa_zglt036-sgtxt  account_name INTO wa_zglt036-sgtxt SEPARATED BY ' '.
      ENDIF.
    ENDIF.
    lt_ftpost-fnam = 'BSEG-SGTXT'.
    lt_ftpost-fval = wa_zglt036-sgtxt.
    APPEND lt_ftpost.

    IF wa_zglt036-gsber IS NOT INITIAL.
      IF wa_zglt036-umskz IS NOT INITIAL.
        lt_ftpost-fnam = 'BSEG-GSBER'.
      ELSE.
        lt_ftpost-fnam = 'COBL-GSBER'.
      ENDIF.
      lt_ftpost-fval = wa_zglt036-gsber.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  wa_zglt036-bschl.
    APPEND lt_ftpost.

    IF wa_zglt036-umskz IS NOT INITIAL.
      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval = wa_zglt036-umskz.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval =  wa_zglt036-hkont.
    APPEND lt_ftpost.

    wl_vlrn = wa_zglt036-vlr_moeda_doc.

    WRITE: wl_vlrn TO wl_vlrc.
    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

*    IF v_waers_bco NE v_waers_doc.
*      IF wa_zglt036-vlr_moeda_int GT 0.
*        wl_vlrn = wa_zglt036-vlr_moeda_int.
*        WRITE: wl_vlrn TO wl_vlrc.
*        lt_ftpost-fnam = 'BSEG-DMBTR'.
*        lt_ftpost-fval =  wl_vlrc.
*        APPEND lt_ftpost.
*
*        IF wa_zglt036-vlr_moeda_forte GT 0.
*          wl_vlrn = wa_zglt036-vlr_moeda_forte.
*          WRITE: wl_vlrn TO wl_vlrc.
*          lt_ftpost-fnam = 'BSEG-DMBE2'.
*          lt_ftpost-fval =  wl_vlrc.
*          APPEND lt_ftpost.
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.

    IF wa_zglt036-valut IS NOT INITIAL.
      CONCATENATE  wa_zglt036-valut+6(2) wa_zglt036-valut+4(2) wa_zglt036-valut(4) INTO vdata_venc SEPARATED BY '.'.
      lt_ftpost-fnam = 'BSEG-VALUT'.
      lt_ftpost-fval = vdata_venc.
      APPEND lt_ftpost.
    ENDIF.

    IF wa_zglt036-hbkid IS NOT INITIAL.
      lt_ftpost-fnam = 'BSEG-HBKID'.
      lt_ftpost-fval = wa_zglt036-hbkid.
      APPEND lt_ftpost.
    ENDIF.

    IF wa_zglt036-bvtyp IS NOT INITIAL.
      lt_ftpost-fnam = 'BSEG-BVTYP'.
      lt_ftpost-fval = wa_zglt036-bvtyp.
      APPEND lt_ftpost.
    ENDIF.

*    IF v_vbund_k IS NOT INITIAL.
*      lt_ftpost-fnam = 'BSEG-VBUND'.
*      lt_ftpost-fval =  v_vbund_k.
*      APPEND lt_ftpost.
*    ELSEIF v_vbund_d IS NOT INITIAL.
*      lt_ftpost-fnam = 'BSEG-VBUND'.
*      lt_ftpost-fval =  v_vbund_d.
*      APPEND lt_ftpost.
*    ENDIF.

    IF wa_zglt036-kostl IS NOT INITIAL.
      lt_ftpost-fnam = 'COBL-KOSTL'.
      lt_ftpost-fval = wa_zglt036-kostl.
      APPEND lt_ftpost.
    ENDIF.

    IF wa_zglt036-aufnr IS NOT INITIAL.
      lt_ftpost-fnam = 'COBL-AUFNR'.
      lt_ftpost-fval = wa_zglt036-aufnr.
      APPEND lt_ftpost.
    ENDIF.

    IF wa_zglt036-prctr IS NOT INITIAL.
      lt_ftpost-fnam = 'COBL-PRCTR'.
      lt_ftpost-fval = wa_zglt036-prctr.
      APPEND lt_ftpost.
    ENDIF.

    IF wa_zglt036-bewar IS NOT INITIAL.
      lt_ftpost-fnam = 'COBL-RMVCT'.
      lt_ftpost-fval = wa_zglt036-bewar.
      APPEND lt_ftpost.
    ENDIF.

  ENDLOOP.


  "Pagamento ou crédito no BANCO
  IF v_wrbtr NE 0.
    ADD 1 TO count_ft.
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    IF v_lifnr IS NOT INITIAL.
      SELECT SINGLE name1
       FROM lfa1
       INTO parid_name
      WHERE lifnr = v_lifnr.
    ELSE.
      SELECT SINGLE name1
          FROM kna1
          INTO parid_name
         WHERE kunnr = v_kunnr.
    ENDIF.

    IF v_wrbtr GT 0.
      lt_ftpost-fval = account_name.
      CONCATENATE TEXT-002 parid_name INTO lt_ftpost-fval SEPARATED BY space.
    ELSE.
      CONCATENATE TEXT-001 parid_name INTO lt_ftpost-fval SEPARATED BY space.
    ENDIF.

    APPEND lt_ftpost.

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    IF v_wrbtr GT 0.
      lt_ftpost-fval =  '50'.
    ELSE.
      lt_ftpost-fval =  '40'.
      MULTIPLY v_wrbtr BY -1.
      MULTIPLY v_dmbtr BY -1.
    ENDIF.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'COBL-GSBER'.
    lt_ftpost-fval = v_gsber.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval =  v_hkont.
    APPEND lt_ftpost.

*---> 15/06/2023 - Migração S4 - JS
*        wl_vlrn = v_wrbtr.
    wl_vlrn = CONV #( v_wrbtr ).
*<--- 15/06/2023 - Migração S4 - JS
    WRITE: wl_vlrn TO wl_vlrc.
    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

    IF v_waers_bco = 'GBP'.
      i_data  = vl_dt_mov.
      obj_zcl_util_sd->set_data(  EXPORTING i_data  = i_data   ).
      obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'B' ).
      obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'GBP' ).
      obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'USD' ).
      obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = lc_ukurs2 ).

      wl_vlrn =  v_wrbtr * abs( lc_ukurs2 ).
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.
    "
    IF v_waers_bco NE v_waers_doc.
      IF v_dmbtr GT 0.
*---> 15/06/2023 - Migração S4 - JS
*        wl_vlrn = v_dmbtr.
        wl_vlrn = CONV #( v_dmbtr ).
*<--- 15/06/2023 - Migração S4 - JS
        WRITE: wl_vlrn TO wl_vlrc.
        IF v_waers_bco = 'GBP'.
        ELSEIF v_waers_bco = 'USD'.
          lt_ftpost-fnam = 'BSEG-DMBE2'.
          lt_ftpost-fval =  wl_vlrc.
          APPEND lt_ftpost.
        ELSEIF v_waers EQ v_waers_bco.
          lt_ftpost-fnam = 'BSEG-DMBTR'.
          lt_ftpost-fval =  wl_vlrc.
          APPEND lt_ftpost.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.

  " Variação K
  IF v_wrbtr_vark NE 0.
    ADD 1 TO count_ft.
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    SELECT SINGLE txt20
          FROM skat
          INTO account_name
         WHERE spras = sy-langu
           AND ktopl = v_ktopl
           AND saknr = v_hkont_var.


    CONCATENATE '' account_name INTO lt_ftpost-fval SEPARATED BY space.

    APPEND lt_ftpost.


    lt_ftpost-fnam = 'RF05A-NEWBS'.
    IF v_wrbtr_vark GT 0.
      lt_ftpost-fval =  '50'.
    ELSE.
      lt_ftpost-fval =  '40'.
      MULTIPLY v_wrbtr_vark BY -1.
    ENDIF.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval =  v_hkont_var.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'COBL-GSBER'.
    lt_ftpost-fval = v_gsber.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-VBUND'.
    lt_ftpost-fval =  v_vbund_k.
    APPEND lt_ftpost.

*---> 15/06/2023 - Migração S4 - JS
*     wl_vlrn = v_wrbtr_vark.
    wl_vlrn = CONV #( v_wrbtr_vark ).
*<--- 15/06/2023 - Migração S4 - JS
    WRITE: wl_vlrn TO wl_vlrc.
    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.
  ENDIF.

  " Variação D
  IF v_wrbtr_vard NE 0.
    ADD 1 TO count_ft.
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'BSEG-SGTXT'.
    SELECT SINGLE txt20
          FROM skat
          INTO account_name
         WHERE spras = sy-langu
           AND ktopl = v_ktopl
           AND saknr = v_hkont_var.


    CONCATENATE '' account_name INTO lt_ftpost-fval SEPARATED BY space.
    APPEND lt_ftpost.


    lt_ftpost-fnam = 'RF05A-NEWBS'.
    IF v_wrbtr_vard GT 0.
      lt_ftpost-fval =  '50'.
    ELSE.
      lt_ftpost-fval =  '40'.
      MULTIPLY v_wrbtr_vard BY -1.
    ENDIF.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval =  v_hkont_var.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'COBL-GSBER'.
    lt_ftpost-fval = v_gsber.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-VBUND'.
    lt_ftpost-fval =  v_vbund_d.
    APPEND lt_ftpost.

*---> 15/06/2023 - Migração S4 - JS
*     wl_vlrn = v_wrbtr_vard.
    wl_vlrn = CONV #( v_wrbtr_vard ).
*<--- 15/06/2023 - Migração S4 - JS
    WRITE: wl_vlrn TO wl_vlrc.
    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.
  ENDIF.

  "Split do fornecedor
  IF  xsplit IS INITIAL.
    LOOP AT it_compensar INTO wa_comp2.
      IF wa_comp2-dmbtr_res EQ 0 OR wa_comp2-koart NE 'K'.
        CONTINUE.
      ENDIF.
      v_wrbtr_resk  = wa_comp2-dmbtr_res.
      v_sgtxt = wa_comp2-sgtxt.
      v_lifnr = wa_comp2-parid.
      IF wa_comp2-zfbdt_res IS NOT INITIAL.
        v_zfbdt_resk = wa_comp2-zfbdt_res.
      ENDIF.

      wl_vlrn = abs( v_wrbtr_resk ).
      WRITE: wl_vlrn TO wl_vlrc.

      ADD 1 TO count_ft.
      lt_ftpost-stype = 'P'.
      lt_ftpost-count = count_ft .

      lt_ftpost-fnam = 'RF05A-NEWBS'.

      "Quando o oducmento processado possuir um código de razão especial
      "A chave de lançamento precisa ser alterada de 24 para 29 ou de 34 para 39
      IF v_wrbtr_resk GT 0.
        IF wa_comp2-umskz NE space.
          lt_ftpost-fval =  '29'.
          APPEND lt_ftpost.
        ELSE.
          lt_ftpost-fval =  '24'.
          APPEND lt_ftpost.
        ENDIF.
      ELSE.
        IF wa_comp2-umskz NE space.
          lt_ftpost-fval =  '39'.
          APPEND lt_ftpost.
        ELSE.
          lt_ftpost-fval =  '34'.
          APPEND lt_ftpost.
        ENDIF.
      ENDIF.

      v_considera_zbd1t = 'X'.
      IF wa_comp2-umskz NE space.
        "Caso for utilizar a chave de lançamento 29 ou 39,
        "o código de razão especial é obrigatório ser informado
        lt_ftpost-fnam = 'RF05A-NEWUM'.
        lt_ftpost-fval =  wa_comp2-umskz.
        APPEND lt_ftpost.

        "Desconciderar preenchimento do campo BSEG-ZBD1T
        CLEAR v_considera_zbd1t.
      ENDIF.
*    ENDIF.

      lt_ftpost-fnam = 'BSEG-HKONT'.
      lt_ftpost-fval =  v_lifnr.
      APPEND lt_ftpost.

      SELECT SINGLE name1
           FROM lfa1
           INTO parid_name
          WHERE lifnr = v_lifnr.

      lt_ftpost-fnam = 'BSEG-SGTXT'.
      CONCATENATE TEXT-003 parid_name INTO lt_ftpost-fval SEPARATED BY space.
      lt_ftpost-fval = v_sgtxt.
      APPEND lt_ftpost.


      lt_ftpost-fnam = 'BSEG-GSBER'.
      lt_ftpost-fval = v_gsber.
      APPEND lt_ftpost.

      CONCATENATE  v_zfbdt_resk+6(2) v_zfbdt_resk+4(2) v_zfbdt_resk(4) INTO vdata_venc SEPARATED BY '.'.
      lt_ftpost-fnam = 'BSEG-ZFBDT'.
      lt_ftpost-fval = vdata_venc.
      APPEND lt_ftpost.

      "Só preenche o campo ZBD1T se for necessário
      IF v_considera_zbd1t EQ 'X'.
        lt_ftpost-fnam = 'BSEG-ZBD1T'.
        lt_ftpost-fval = '0'.
        CONDENSE lt_ftpost-fval NO-GAPS.
        APPEND lt_ftpost.
      ENDIF.
*---> 15/06/2023 - Migração S4 - JS
*     wl_vlrn = v_wrbtr.
      wl_vlrn = CONV #( v_wrbtr ).
*<--- 15/06/2023 - Migração S4 - JS
      lt_ftpost-fnam = 'BSEG-WRBTR'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.

      "
      " Taxa do documento NOVO
      DATA etl748c6r5377 TYPE TABLE OF bseg.
      DATA lt_fields_l748c6r2320 TYPE fagl_t_field.
      lt_fields_l748c6r2320 = VALUE #( ( line = 'DMBTR' )
       ( line = 'DMBE2' )
       ( line = 'WRBTR' )
       ( line = 'ZUONR' )
       ).
      DATA rldnr_l748c6r6650 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l748c6r6650
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr      = rldnr_l748c6r6650
            i_bukrs      = wa_comp2-bukrs
            i_belnr      = wa_comp2-belnr
            i_gjahr      = wa_comp2-gjahr
            i_buzei      = wa_comp2-buzei
            it_fieldlist = lt_fields_l748c6r2320
          IMPORTING
            et_bseg      = etl748c6r5377
          EXCEPTIONS
            not_found    = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl748c6r5377 ) = 1.
        v_dmbtr_t = etl748c6r5377[ 1 ]-dmbtr.
        v_dmbe2_t = etl748c6r5377[ 1 ]-dmbe2.
        v_wrbtr_t = etl748c6r5377[ 1 ]-wrbtr.
        v_zuonr = etl748c6r5377[ 1 ]-zuonr.
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.


      lt_ftpost-fnam = 'BSEG-ZUONR'.
      lt_ftpost-fval = v_zuonr.
      APPEND lt_ftpost.

      lc_ukurs  = v_dmbtr_t / v_wrbtr_t.
      lc_ukurs2 = v_dmbe2_t / v_wrbtr_t.

      IF lc_ukurs NE 1.
        v_dmbtr_t = lc_ukurs * abs( v_wrbtr_resk ).
*---> 15/06/2023 - Migração S4 - JS
*            wl_vlrn =  v_dmbtr_t .
        wl_vlrn = CONV #( v_dmbtr_t ).
*<--- 15/06/2023 - Migração S4 - JS
        WRITE: wl_vlrn TO wl_vlrc.
        lt_ftpost-fnam = 'BSEG-DMBTR'.
        lt_ftpost-fval =  wl_vlrc.
        APPEND lt_ftpost.
      ENDIF.
      "
      IF lc_ukurs2 NE 1.
        v_dmbe2_t = lc_ukurs2 * abs( v_wrbtr_resk ).
*---> 15/06/2023 - Migração S4 - JS
*           wl_vlrn =  v_dmbe2_t.
        wl_vlrn = CONV #( v_dmbe2_t ).
*<--- 15/06/2023 - Migração S4 - JS
        WRITE: wl_vlrn TO wl_vlrc.
        lt_ftpost-fnam = 'BSEG-DMBE2'.
        lt_ftpost-fval =  wl_vlrc.
        APPEND lt_ftpost.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Split do cliente
  IF xsplit IS INITIAL.
    LOOP AT it_compensar INTO wa_comp2.
      IF wa_comp2-dmbtr_res EQ 0 OR wa_comp2-koart NE 'D'.
        CONTINUE.
      ENDIF.
      v_wrbtr_resd = wa_comp2-dmbtr_res.
      v_sgtxt      = wa_comp2-sgtxt.
      v_kunnr = wa_comp2-parid.
      IF wa_comp2-zfbdt_res IS NOT INITIAL.
        v_zfbdt_resd = wa_comp2-zfbdt_res.
      ENDIF.
      wl_vlrn = abs( v_wrbtr_resd ).
      WRITE: wl_vlrn TO wl_vlrc.

      ADD 1 TO count_ft.
      lt_ftpost-stype = 'P'.
      lt_ftpost-count = count_ft .

      lt_ftpost-fnam = 'RF05A-NEWBS'.

      "Quando o documento processado possuir um código de razão especial
      "A chave de lançamento precisa ser alterada de 04 para 09 ou de 14 para 19
      IF v_wrbtr_resd GT 0.
        IF wa_comp2-umskz NE space.
          lt_ftpost-fval =  '09'.
          APPEND lt_ftpost.
        ELSE.
          lt_ftpost-fval =  '04'.
          APPEND lt_ftpost.
        ENDIF.
      ELSE.
        IF wa_comp2-umskz NE space.
          lt_ftpost-fval =  '19'.
          APPEND lt_ftpost.
        ELSE.
          lt_ftpost-fval =  '14'.
          APPEND lt_ftpost.
        ENDIF.
      ENDIF.

      v_considera_zbd1t = 'X'.
      IF wa_comp2-umskz NE space.
        "Caso for utilizar a chave de lançamento 29 ou 39,
        "o código de razão especial é obrigatório ser informado
        lt_ftpost-fnam = 'RF05A-NEWUM'.
        lt_ftpost-fval =  wa_comp2-umskz.
        APPEND lt_ftpost.

        "Desconciderar preenchimento do campo BSEG-ZBD1T
        CLEAR v_considera_zbd1t.
      ENDIF.


      lt_ftpost-fnam = 'BSEG-HKONT'.
      lt_ftpost-fval =  v_kunnr.
      APPEND lt_ftpost.

      SELECT SINGLE name1
         FROM kna1
         INTO parid_name
        WHERE kunnr = v_kunnr.

      lt_ftpost-fnam = 'BSEG-SGTXT'.
      CONCATENATE TEXT-003 parid_name INTO lt_ftpost-fval SEPARATED BY space.
      lt_ftpost-fval = v_sgtxt.
      APPEND lt_ftpost.

      lt_ftpost-fnam = 'BSEG-GSBER'.
      lt_ftpost-fval = v_gsber.
      APPEND lt_ftpost.

      CONCATENATE  v_zfbdt_resd+6(2) v_zfbdt_resd+4(2) v_zfbdt_resd(4) INTO vdata_venc SEPARATED BY '.'.
      lt_ftpost-fnam = 'BSEG-ZFBDT'.
      lt_ftpost-fval = vdata_venc.
      APPEND lt_ftpost.

      "Só preenche o campo ZBD1T se for necessário
      IF v_considera_zbd1t EQ 'X'.
        lt_ftpost-fnam = 'BSEG-ZBD1T'.
        lt_ftpost-fval = '0'.
        CONDENSE lt_ftpost-fval NO-GAPS.
        APPEND lt_ftpost.
      ENDIF.
*---> 15/06/2023 - Migração S4 - JS
*         wl_vlrn = v_wrbtr.
      wl_vlrn = CONV #( v_wrbtr ).
*<--- 15/06/2023 - Migração S4 - JS
      lt_ftpost-fnam = 'BSEG-WRBTR'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.

      " Taxa do documento NOVO
      DATA etl874c6r1817 TYPE TABLE OF bseg.
      DATA lt_fields_l874c6r2461 TYPE fagl_t_field.
      lt_fields_l874c6r2461 = VALUE #( ( line = 'DMBTR' )
       ( line = 'DMBE2' )
       ( line = 'WRBTR' )
       ( line = 'ZUONR' )
       ).
      DATA rldnr_l874c6r4311 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l874c6r4311
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr      = rldnr_l874c6r4311
            i_bukrs      = wa_comp2-bukrs
            i_belnr      = wa_comp2-belnr
            i_gjahr      = wa_comp2-gjahr
            i_buzei      = wa_comp2-buzei
            it_fieldlist = lt_fields_l874c6r2461
          IMPORTING
            et_bseg      = etl874c6r1817
          EXCEPTIONS
            not_found    = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl874c6r1817 ) = 1.
        v_dmbtr_t = etl874c6r1817[ 1 ]-dmbtr.
        v_dmbe2_t = etl874c6r1817[ 1 ]-dmbe2.
        v_wrbtr_t = etl874c6r1817[ 1 ]-wrbtr.
        v_zuonr = etl874c6r1817[ 1 ]-zuonr.
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.


      lt_ftpost-fnam = 'BSEG-ZUONR'.
      lt_ftpost-fval = v_zuonr.
      APPEND lt_ftpost.


      lc_ukurs  = v_dmbtr_t / v_wrbtr_t.
      lc_ukurs2 = v_dmbe2_t / v_wrbtr_t.

      IF lc_ukurs NE 1.
        v_dmbtr_t = lc_ukurs * abs( v_wrbtr_resd ).
*---> 15/06/2023 - Migração S4 - JS
*            wl_vlrn =  v_dmbtr_t .
        wl_vlrn = CONV #( v_dmbtr_t ).
*<--- 15/06/2023 - Migração S4 - JS
        WRITE: wl_vlrn TO wl_vlrc.
        lt_ftpost-fnam = 'BSEG-DMBTR'.
        lt_ftpost-fval =  wl_vlrc.
        APPEND lt_ftpost.
      ENDIF.

      IF lc_ukurs2 NE 1.
        v_dmbe2_t = lc_ukurs2 * abs( v_wrbtr_resd ).
*---> 15/06/2023 - Migração S4 - JS
*         wl_vlrn =  v_dmbe2_t .
        wl_vlrn = CONV #( v_dmbe2_t ).
*<--- 15/06/2023 - Migração S4 - JS
        WRITE: wl_vlrn TO wl_vlrc.
        lt_ftpost-fnam = 'BSEG-DMBE2'.
        lt_ftpost-fval =  wl_vlrc.
        APPEND lt_ftpost.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "PAGAR/RECEBER
  LOOP AT it_compensar INTO DATA(wa_comp).
    lt_ftclear-agkoa  = wa_comp-koart.
    lt_ftclear-agkon  = wa_comp-parid.
    lt_ftclear-agums  = wa_comp-umskz.
    lt_ftclear-agbuk  = wa_comp-bukrs.
    lt_ftclear-xnops  = 'X'.
    lt_ftclear-selfd  = 'BELNR'.
    CONCATENATE wa_comp-belnr wa_comp-gjahr wa_comp-buzei INTO lt_ftclear-selvon.
    APPEND lt_ftclear.
  ENDLOOP.


  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = l_auglv
      i_tcode                    = l_tcode
      i_sgfunct                  = l_sgfunct
      i_no_auth                  = 'X'
      i_xsimu                    = v_xsimu
    IMPORTING
      e_msgid                    = lds_return-id
      e_msgno                    = lds_return-number
      e_msgty                    = lds_return-type
      e_msgv1                    = lds_return-message_v1
      e_msgv2                    = lds_return-message_v2
      e_msgv3                    = lds_return-message_v3
      e_msgv4                    = lds_return-message_v4
    TABLES
      t_blntab                   = lt_blntab
      t_ftclear                  = lt_ftclear
      t_ftpost                   = lt_ftpost
      t_fttax                    = lt_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.


  IF lt_blntab[] IS INITIAL.
    WRITE lds_return-number TO msg_no.
    IF lds_return-id = '00' AND lds_return-number = '298'.
      IF sy-langu = 'P'.
        msg_text = 'Faltou informar data vencimento para saldo residual'.
      ELSE.
        msg_text = 'Missing inform due date for residual balance'.
      ENDIF.
    ELSE.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = lds_return-id
          msg_no                 = msg_no
          msg_var1               = lds_return-message_v1
          msg_var2               = lds_return-message_v2
          msg_var3               = lds_return-message_v3
          msg_var4               = lds_return-message_v4
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
    ENDIF.

  ELSE.
    READ TABLE lt_blntab INDEX 1.
    e_blntab = e_blntab.


  ENDIF.

  "fim
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.




ENDFUNCTION.

FORM f_bapi_f51 USING    wa_split1 TYPE bseg
                         vl_dt_mov
                         vl_dt_vc
                CHANGING wa_split2 TYPE bseg
                         msg_text.



  DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
        l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
        l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately

  DATA: lt_blntab  TYPE STANDARD TABLE OF blntab  WITH HEADER LINE,
        lt_ftclear TYPE STANDARD TABLE OF ftclear WITH HEADER LINE,
        lt_ftpost  TYPE STANDARD TABLE OF ftpost  WITH HEADER LINE,
        lt_fttax   TYPE STANDARD TABLE OF fttax   WITH HEADER LINE,
        lds_return TYPE bapiret2.


  DATA: wa_ret_document TYPE zfie_ret_document,
        it_ret_document LIKE STANDARD TABLE OF wa_ret_document.

  DATA: vdata(10),
        vdata_venc(10),
        cnum_seq(2),
        wl_vlr(16),
        wl_taxa(16),
        wl_vlrc(16),
        wl_vlrn        TYPE p DECIMALS 2,
        lc_ukurs       TYPE ukurs_curr,
        lc_ukurs2      TYPE ukurs_curr,
        v_wrbtr_t      TYPE bseg-wrbtr,
        v_dmbtr_t      TYPE bseg-dmbtr,
        v_dmbe2_t      TYPE bseg-dmbe2,
        vcampo(15),
        v_kur          TYPE bkpf-kursf,
        vvalor_bax     TYPE bseg-dmbtr,
        msg_no         TYPE t100-msgnr,
        "MSG_TEXT       TYPE STRING,
        p_mode         LIKE rfpdo-allgazmd,
*        VL_DT_MOV      TYPE SY-DATUM,
        count_ft       TYPE ftpost-count,
        v_xsimu        TYPE char1,
        w_bkpf         TYPE bkpf.


* ---> S4 Migration - 19/06/2023 - JS
*  select *
*    from BSEG
*    into table  @data(T_BSEG)
*     where BUKRS = @WA_SPLIT1-BUKRS
*     and   BELNR = @WA_SPLIT1-BELNR
*     and   GJAHR = @WA_SPLIT1-GJAHR
*     and   BUZEI = @WA_SPLIT1-BUZEI.

  DATA: t_bseg TYPE fagl_t_bseg.

  CALL FUNCTION 'FAGL_GET_BSEG'
    EXPORTING
      i_bukrs = wa_split1-bukrs
      i_belnr = wa_split1-belnr
      i_gjahr = wa_split1-gjahr
      i_buzei = wa_split1-buzei
    IMPORTING
      et_bseg = t_bseg
    EXCEPTIONS
      OTHERS  = 2.
* <--- S4 Migration - 19/06/2023 - JS


  IF sy-subrc NE 0.
    REFRESH  t_bseg.
  ENDIF.

  LOOP AT t_bseg INTO DATA(w_bseg).

  ENDLOOP.
  IF t_bseg[] IS INITIAL.

  ELSE.
    IF wa_split1-dmbtr LT 0.
      MULTIPLY wa_split1-dmbtr BY -1.
    ENDIF.
    vvalor_bax = w_bseg-wrbtr - wa_split1-dmbtr.
    SELECT SINGLE *
      FROM bkpf
      INTO w_bkpf
      WHERE bukrs = w_bseg-bukrs
      AND   belnr = w_bseg-belnr
      AND   gjahr = w_bseg-gjahr.
    p_mode = 'N'.

    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_client           = sy-mandt
        i_function         = 'C'
        i_mode             = p_mode
        i_update           = 'S'
        i_user             = sy-uname
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        OTHERS             = 6.

    IF sy-subrc NE 0.
      ROLLBACK WORK.
      MESSAGE 'Houve ao efetuar a compensação' TYPE 'S'.
      RETURN.
    ENDIF.

    CONCATENATE  vl_dt_mov+6(2) vl_dt_mov+4(2) vl_dt_mov(4) INTO vdata SEPARATED BY '.'.


    CONDENSE wl_taxa NO-GAPS.

    CLEAR: lt_blntab,   lt_blntab[],
           lt_ftclear,  lt_ftclear[],
           lt_ftpost,   lt_ftpost[],
           lt_fttax,    lt_fttax[],

           lds_return.

    count_ft = 1.

    lt_ftpost-stype = 'K'."Header
    lt_ftpost-count = count_ft.  "number of Dynpro

    lt_ftpost-fnam = 'BKPF-BUKRS'.
    lt_ftpost-fval = w_bkpf-bukrs.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-WAERS'.
    lt_ftpost-fval = w_bkpf-waers.
    APPEND lt_ftpost.

    WRITE: w_bkpf-kursf TO wl_taxa.
    CONDENSE wl_taxa NO-GAPS.
    lt_ftpost-fnam = 'BKPF-KURSF'.
    lt_ftpost-fval = wl_taxa.
    APPEND lt_ftpost.


    lt_ftpost-fnam = 'BKPF-BLDAT'.
    lt_ftpost-fval =  vdata.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-BUDAT'.
    lt_ftpost-fval = vdata.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-MONAT'.
    lt_ftpost-fval =  vl_dt_mov+4(2).
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BKPF-BLART'.
    lt_ftpost-fval = w_bkpf-blart.
    APPEND lt_ftpost.

    "parte 1
*---> 15/06/2023 - Migração S4 - JS
*      wl_vlrn = wa_split1-dmbtr.
    wl_vlrn = CONV #( wa_split1-dmbtr ).
*<--- 15/06/2023 - Migração S4 - JS
    WRITE: wl_vlrn TO wl_vlrc.

    ADD 1 TO count_ft.
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  w_bseg-bschl.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval =  wa_split1-lifnr.
    APPEND lt_ftpost.

    IF w_bseg-umskz NE ' '.
      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval =  w_bseg-umskz.
      APPEND lt_ftpost.
    ENDIF.

    " RMI CASE CS1042045- Correção das datas na ZFIS36 - RSF - 15.12.2022 - IR118896
    " Considerar o campo de texto da popup 'Data do Residual"
    " Programa ZGLT067 - screen 0303
    IF wa_split1-sgtxt IS NOT INITIAL.
      lt_ftpost-fnam = 'BSEG-SGTXT'.
      lt_ftpost-fval = wa_split1-sgtxt.
      APPEND lt_ftpost.
    ELSE.
      lt_ftpost-fnam = 'BSEG-SGTXT'.
      lt_ftpost-fval = w_bseg-sgtxt.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'BSEG-ZUONR'.
    lt_ftpost-fval = w_bseg-zuonr.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-GSBER'.
    lt_ftpost-fval = w_bseg-gsber.
    APPEND lt_ftpost.

    CONCATENATE  vl_dt_vc+6(2) vl_dt_vc+4(2) vl_dt_vc(4) INTO vdata_venc SEPARATED BY '.'.
    lt_ftpost-fnam = 'BSEG-ZFBDT'.
    lt_ftpost-fval = vdata_venc.
    APPEND lt_ftpost.

    IF w_bseg-zbd1t > 0.
      lt_ftpost-fnam = 'BSEG-ZBD1T'.
      lt_ftpost-fval = w_bseg-zbd1t.
      CONDENSE lt_ftpost-fval NO-GAPS.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

    " Taxa do documento NOVO
    lc_ukurs  = w_bseg-dmbtr / w_bseg-wrbtr.
    lc_ukurs2 = w_bseg-dmbe2 / w_bseg-wrbtr.

    IF lc_ukurs NE 1.
      v_dmbtr_t = lc_ukurs * abs( wa_split1-dmbtr ).
*---> 15/06/2023 - Migração S4 - JS
*            wl_vlrn =  v_dmbtr_t .
      wl_vlrn = CONV #( v_dmbtr_t ).
*<--- 15/06/2023 - Migração S4 - JS
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBTR'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.
    "
    IF lc_ukurs2 NE 1.
      v_dmbe2_t = lc_ukurs2 * abs( wa_split1-dmbtr ).
*---> 15/06/2023 - Migração S4 - JS
*           wl_vlrn =  v_dmbe2_t.
      wl_vlrn = CONV #( v_dmbe2_t ).
*<--- 15/06/2023 - Migração S4 - JS
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.

    "parte 2
*---> 15/06/2023 - Migração S4 - JS
*     wl_vlrn = vvalor_bax.
    wl_vlrn = CONV #( vvalor_bax ).
*<--- 15/06/2023 - Migração S4 - JS
    WRITE: wl_vlrn TO wl_vlrc.

    ADD 1 TO count_ft.
    lt_ftpost-stype = 'P'.
    lt_ftpost-count = count_ft .

    lt_ftpost-fnam = 'RF05A-NEWBS'.
    lt_ftpost-fval =  w_bseg-bschl.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-HKONT'.
    lt_ftpost-fval =  wa_split1-lifnr.
    APPEND lt_ftpost.

    IF w_bseg-umskz NE ' '.
      lt_ftpost-fnam = 'RF05A-NEWUM'.
      lt_ftpost-fval =  w_bseg-umskz.
      APPEND lt_ftpost.
    ENDIF.

    " RMI CASE CS1042045- Correção das datas na ZFIS36 - RSF - 15.12.2022 - IR118896
    " Considerar o campo de texto da popup 'Data do Residual"
    " Programa ZGLT067 - screen 0303
    IF wa_split1-sgtxt IS NOT INITIAL.
      lt_ftpost-fnam = 'BSEG-SGTXT'.
      lt_ftpost-fval = wa_split1-sgtxt.
      APPEND lt_ftpost.
    ELSE.
      lt_ftpost-fnam = 'BSEG-SGTXT'.
      lt_ftpost-fval = w_bseg-sgtxt.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'BSEG-ZUONR'.
    lt_ftpost-fval = w_bseg-zuonr.
    APPEND lt_ftpost.

    lt_ftpost-fnam = 'BSEG-GSBER'.
    lt_ftpost-fval = w_bseg-gsber.
    APPEND lt_ftpost.

    CONCATENATE  w_bseg-zfbdt+6(2) w_bseg-zfbdt+4(2) w_bseg-zfbdt(4) INTO vdata_venc SEPARATED BY '.'.
    lt_ftpost-fnam = 'BSEG-ZFBDT'.
    lt_ftpost-fval = vdata_venc.
    APPEND lt_ftpost.

    IF w_bseg-zbd1t > 0.
      lt_ftpost-fnam = 'BSEG-ZBD1T'.
      lt_ftpost-fval = w_bseg-zbd1t.
      CONDENSE lt_ftpost-fval NO-GAPS.
      APPEND lt_ftpost.
    ENDIF.

    lt_ftpost-fnam = 'BSEG-WRBTR'.
    lt_ftpost-fval =  wl_vlrc.
    APPEND lt_ftpost.

    IF lc_ukurs NE 1.
      v_dmbtr_t = lc_ukurs * abs( vvalor_bax ).
*---> 15/06/2023 - Migração S4 - JS
*            wl_vlrn =  v_dmbtr_t .
      wl_vlrn = CONV #( v_dmbtr_t ).
*<--- 15/06/2023 - Migração S4 - JS
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBTR'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.
    "
    IF lc_ukurs2 NE 1.
      v_dmbe2_t = lc_ukurs2 * abs( vvalor_bax ).
*---> 15/06/2023 - Migração S4 - JS
*        wl_vlrn =  v_dmbe2_t.
      wl_vlrn = CONV #( v_dmbe2_t ).
*<--- 15/06/2023 - Migração S4 - JS
      WRITE: wl_vlrn TO wl_vlrc.
      lt_ftpost-fnam = 'BSEG-DMBE2'.
      lt_ftpost-fval =  wl_vlrc.
      APPEND lt_ftpost.
    ENDIF.

    "FATURA ORIGINAL
    lt_ftclear-agkoa  = w_bseg-koart.
    lt_ftclear-agkon  = w_bseg-lifnr.
    lt_ftclear-agums  = w_bseg-umskz.
    lt_ftclear-agbuk  = w_bseg-bukrs.
    lt_ftclear-xnops  = 'X'.
    lt_ftclear-selfd  = 'BELNR'.
    CONCATENATE w_bseg-belnr w_bseg-gjahr w_bseg-buzei INTO lt_ftclear-selvon.
    APPEND lt_ftclear.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = l_auglv
        i_tcode                    = l_tcode
        i_sgfunct                  = l_sgfunct
        i_no_auth                  = 'X'
        i_xsimu                    = v_xsimu
      IMPORTING
        e_msgid                    = lds_return-id
        e_msgno                    = lds_return-number
        e_msgty                    = lds_return-type
        e_msgv1                    = lds_return-message_v1
        e_msgv2                    = lds_return-message_v2
        e_msgv3                    = lds_return-message_v3
        e_msgv4                    = lds_return-message_v4
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
      EXCEPTIONS
        clearing_procedure_invalid = 1
        clearing_procedure_missing = 2
        table_t041a_empty          = 3
        transaction_code_invalid   = 4
        amount_format_error        = 5
        too_many_line_items        = 6
        company_code_invalid       = 7
        screen_not_found           = 8
        no_authorization           = 9
        OTHERS                     = 10.


    IF lt_blntab[] IS INITIAL.

    ELSE.
      READ TABLE lt_blntab INDEX 1.
      WAIT UP TO 2 SECONDS.
      IF wa_split1-koart = 'K'.
* ---> S4 Migration - 16/06/2023 - JS
*        SELECT SINGLE bukrs belnr buzei kunnr gjahr
*          FROM bseg
*          INTO CORRESPONDING FIELDS OF wa_split2
*           WHERE bukrs = lt_blntab-bukrs
*           AND   belnr = lt_blntab-belnr
*           AND   gjahr = lt_blntab-gjahr
*           AND   lifnr = wa_split1-lifnr
*           AND   wrbtr = vvalor_bax.
        DATA lt_bseg TYPE TABLE OF bseg.

        "ALRS US 122598
        CALL FUNCTION 'FAGL_GET_BSEG'
          EXPORTING
            i_bukrs = lt_blntab-bukrs
            i_belnr = lt_blntab-belnr
            i_gjahr = lt_blntab-gjahr
          IMPORTING
            et_bseg = lt_bseg
          EXCEPTIONS
            OTHERS  = 2.

        DELETE lt_bseg WHERE lifnr NE wa_split1-lifnr OR
                             wrbtr NE vvalor_bax.

        IF sy-subrc = 0 AND lines( lt_bseg ) > 0.

          sy-dbcnt = lines( lt_bseg ).
          LOOP AT lt_bseg INTO DATA(wa_bseg).
            MOVE-CORRESPONDING wa_bseg TO wa_split2.
          ENDLOOP.
        ELSE.

          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.

      ELSE.
*        select single BUKRS BELNR BUZEI KUNNR GJAHR
*          from BSEG
*         into corresponding fields of WA_SPLIT2
*           where BUKRS = LT_BLNTAB-BUKRS
*           and   BELNR = LT_BLNTAB-BELNR
*           and   GJAHR = LT_BLNTAB-GJAHR
*           and   KUNNR = WA_SPLIT1-LIFNR
*           and   WRBTR = VVALOR_BAX.

        "ALRS US 122598
        CALL FUNCTION 'FAGL_GET_BSEG'
          EXPORTING
            i_bukrs = lt_blntab-bukrs
            i_belnr = lt_blntab-belnr
            i_gjahr = lt_blntab-gjahr
          IMPORTING
            et_bseg = lt_bseg
          EXCEPTIONS
            OTHERS  = 2.

        DELETE lt_bseg WHERE kunnr NE wa_split1-lifnr OR
                             wrbtr NE vvalor_bax.

        IF sy-subrc = 0 AND lines( lt_bseg ) > 0.

          sy-dbcnt = lines( lt_bseg ).
          LOOP AT lt_bseg INTO wa_bseg.
            MOVE-CORRESPONDING wa_bseg TO wa_split2.
          ENDLOOP.
        ELSE.

          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.
      ENDIF.
* <--- S4 Migration - 16/06/2023 - JS
    ENDIF.

    "fim
    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = 'X'
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.
  ENDIF.

ENDFORM.
