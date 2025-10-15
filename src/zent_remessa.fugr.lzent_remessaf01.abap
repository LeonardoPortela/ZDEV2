*----------------------------------------------------------------------*
***INCLUDE LZENT_REMESSAF01 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    value(p_flag)
                           value(p_fnam)
                           value(p_fval).

  CLEAR t_bdcdata.
  IF NOT p_flag IS INITIAL.
    t_bdcdata-program  = p_fnam.
    t_bdcdata-dynpro   = p_fval.
    t_bdcdata-dynbegin = 'X'.
  ELSE.
    t_bdcdata-fnam = p_fnam.
    t_bdcdata-fval = p_fval.
  ENDIF.
  APPEND t_bdcdata.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Form  CRIA_ORDEM_COMPRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ENTRADA_REM  text
*      -->P_L_SUBRC  text
*----------------------------------------------------------------------*
FORM cria_ordem_compra  USING    p_wa_entrada_rem TYPE zsdt_entrada_rem
                                 p_l_subrc        TYPE sy-subrc
                                 p_vbeln          TYPE vbeln_va .

  DATA: vg_vbeln LIKE vbfa-vbeln,
        vg_dt_remessa TYPE c LENGTH 10.

  CLEAR: t_bdcdata[], t_messtab[].

  IF p_wa_entrada_rem-dt_chegada IS INITIAL.
    p_wa_entrada_rem-dt_chegada = sy-datum.
  ENDIF.

  CONCATENATE p_wa_entrada_rem-dt_chegada+6(2) '.' p_wa_entrada_rem-dt_chegada+4(2) '.' p_wa_entrada_rem-dt_chegada(4) INTO vg_dt_remessa.

  SELECT SINGLE vbeln INTO vg_vbeln FROM vbfa  WHERE vbelv EQ p_wa_entrada_rem-vbeln_s AND vbtyp_n EQ 'M'  AND vbtyp_v EQ 'J'.

  PERFORM f_bdc_field USING: 'X' 'SAPMV45A'	      '0101',
                             ' ' 'BDC_CURSOR'	    'VBAK-AUART',
                             ' ' 'BDC_OKCODE'	    '/00',
                             ' ' 'VBAK-AUART'	    'ZEFL',
                             ' ' 'VBAK-VKORG'     p_wa_entrada_rem-vkorg,
                             ' ' 'VBAK-VTWEG'     p_wa_entrada_rem-vtweg,
                             ' ' 'VBAK-SPART'     p_wa_entrada_rem-spart.

  PERFORM f_bdc_field USING: 'X' 'SAPLV45C'	      '0100',
                             ' ' 'BDC_OKCODE'	    '=UEBR',
                             ' ' 'BDC_SUBSCR'	    'SAPLV45C',
                             ' ' 'BDC_CURSOR'	    'VBRK-VBELN',
                             ' ' 'VBRK-VBELN'	    vg_vbeln.

  PERFORM f_bdc_field USING: 'X' 'SAPMV45A'	      '4001',
                             ' ' 'BDC_OKCODE'	    '/00',
                             ' ' 'BDC_SUBSCR'	    'SAPMV45A',
                             ' ' 'BDC_SUBSCR'	    'SAPMV45A',
                             ' ' 'BDC_CURSOR'	    'VBAP-WERKS(01)',
                             ' ' 'VBAP-WERKS(01)' p_wa_entrada_rem-werks_v,
                             ' ' 'BDC_CURSOR'	    'VBAP-LGORT(01)',
                             ' ' 'VBAP-LGORT(01)'	p_wa_entrada_rem-lgort,
                             ' ' 'BDC_CURSOR'	    'VBAP-ROUTE(01)',
                             ' ' 'VBAP-ROUTE(01)' 'FOB',
                             ' ' 'BDC_SUBSCR'  	  'SAPMV45A'.

  PERFORM f_bdc_field USING: 'X' 'SAPMV45A'	      '4001',
                             ' ' 'BDC_OKCODE'	    '=ITEM'.

  PERFORM f_bdc_field USING: 'X' 'SAPMV45A'	      '4003',
                             ' ' 'BDC_OKCODE'    	'/00',
                             ' ' 'BDC_SUBSCR'	    'SAPMV45A',
                             ' ' 'BDC_SUBSCR'	    'SAPMV45A',
                             ' ' 'BDC_CURSOR'	    'RV45A-ETDAT',
                             ' ' 'RV45A-ETDAT'    vg_dt_remessa,
                             ' ' 'BDC_CURSOR'     'VBAP-VKAUS',
                             ' ' 'VBAP-VKAUS'     'I'.

  PERFORM f_bdc_field USING: 'X' 'SAPMV45A'	      '4003',
                             ' ' 'BDC_OKCODE'	    '=SICH',
                             ' ' 'BDC_SUBSCR'	    'SAPMV45A',
                             ' ' 'BDC_SUBSCR'	    'SAPMV45A'.

  CALL TRANSACTION 'VA01' USING t_bdcdata MODE 'N' UPDATE 'S' MESSAGES INTO t_messtab.

  READ TABLE t_messtab WITH KEY msgtyp = 'E'.

  IF sy-subrc NE 0.
    p_l_subrc = 0.
    READ TABLE t_messtab INTO w_messtab WITH KEY msgtyp = 'S'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = w_messtab-msgv2(10)
      IMPORTING
        output = p_vbeln.
  ELSE.
    p_l_subrc = 4.
  ENDIF.

ENDFORM.                    " CRIA_ORDEM_COMPRA
*&---------------------------------------------------------------------*
*&      Form  CRIAR_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_VBELN  text
*----------------------------------------------------------------------*
FORM criar_delivery  TABLES p_deliverys STRUCTURE remessas
                      USING p_wa_entrada_rem TYPE zsdt_entrada_rem
                            p_vbeln     TYPE vbeln_va
                            wa_romaneio TYPE zsdt0001
                            p_l_subrc   TYPE sy-subrc.

  DATA: it_itens     TYPE bapidlvreftosalesorder OCCURS 0 WITH HEADER LINE,
        it_vbap      TYPE TABLE OF vbap INITIAL SIZE 0 WITH HEADER LINE,
        wa_vbak      TYPE vbak,
        wa_vbap      TYPE vbap,
        v_deliv_numb TYPE bapishpdelivnumb-deliv_numb,
        v_due_date   TYPE ledat,
        it_retorno   TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        wa_retorno   TYPE bapiret2,
        it_items     TYPE bapishpdelivnumb OCCURS 0 WITH HEADER LINE.

  CLEAR: p_l_subrc, p_deliverys[].

  SELECT SINGLE * INTO wa_vbak
    FROM vbak
  WHERE vbeln EQ p_vbeln.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE it_vbap
    FROM vbap
   WHERE vbeln EQ wa_vbak-vbeln.

  IF wa_romaneio IS INITIAL.
    IF p_wa_entrada_rem-qt_chegada IS INITIAL.
      SELECT SINGLE SUM( ntgew ) INTO p_wa_entrada_rem-qt_chegada
        FROM lips
       WHERE vbeln EQ p_wa_entrada_rem-vbeln_s.
    ENDIF.
  ELSE.
    IF p_wa_entrada_rem-qt_chegada IS INITIAL.
      p_wa_entrada_rem-qt_chegada = wa_romaneio-peso_liq.
    ENDIF.
  ENDIF.

  LOOP AT it_vbap INTO wa_vbap.

    it_itens-ref_doc        = wa_vbap-vbeln.
    it_itens-ref_item       = wa_vbap-posnr.
    "it_itens-dlv_qty        = wa_romaneio-peso_liq.
    "it_itens-sales_unit     = wa_vbap-meins.
    "it_itens-sales_unit_iso = wa_vbap-meins.
    v_due_date              = p_wa_entrada_rem-dt_chegada.
    APPEND it_itens.

"*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        ship_point        = wa_vbap-werks
        due_date          = v_due_date
      IMPORTING
        delivery          = v_deliv_numb
      TABLES
        sales_order_items = it_itens
        deliveries        = it_items
        return            = it_retorno.

    READ TABLE it_retorno INTO wa_retorno WITH KEY type = 'S'.

    IF sy-subrc NE 0.
      p_l_subrc = 4.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      p_l_subrc  = 0.
      remessas-numero   = v_deliv_numb.
      remessas-ref_doc  = wa_vbap-vbeln.
      remessas-ref_item = wa_vbap-posnr.
      APPEND remessas TO p_deliverys.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.

      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.

    ENDIF.

    LOOP AT it_retorno INTO wa_retorno.
      CLEAR: w_messtab.
      w_messtab-tcode  = sy-tcode.
      w_messtab-msgtyp = wa_retorno-type.
      w_messtab-msgnr  = '002'.
      w_messtab-msgv1  = wa_retorno-message_v1.
      w_messtab-msgv2  = wa_retorno-message_v2.
      w_messtab-msgv3  = wa_retorno-message_v3.
      w_messtab-msgv4  = wa_retorno-message_v4.
      APPEND w_messtab TO t_messtab.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " CRIAR_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  PICKING_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM picking_delivery  TABLES p_deliverys      STRUCTURE remessas
                       USING  wa_romaneio      TYPE zsdt0001
                              p_wa_entrada_rem TYPE zsdt_entrada_rem.

  DATA: sl_vbkok_wa  TYPE vbkok,
        sl_vbpok     TYPE vbpok,
        tl_vbpok     TYPE TABLE OF vbpok,
        tl_prot      TYPE TABLE OF prott,
        wa_prot      TYPE prott,
        wa_vbap      TYPE vbap.

  LOOP AT p_deliverys INTO remessas.

    SELECT SINGLE * INTO wa_vbap
      FROM vbap
     WHERE vbeln EQ remessas-ref_doc
       AND posnr EQ remessas-ref_item.

    sl_vbkok_wa-vbeln_vl    = remessas-numero.
    sl_vbkok_wa-vbeln       = remessas-numero.
    sl_vbkok_wa-wabuc       = 'X'.
    sl_vbkok_wa-wadat_ist   = p_wa_entrada_rem-dt_chegada.

    sl_vbpok-vbeln_vl       = remessas-numero.
    sl_vbpok-posnr_vl       = wa_vbap-posnr.
    sl_vbpok-vbeln          = remessas-numero.
    sl_vbpok-posnn          = wa_vbap-posnr.
    sl_vbpok-matnr          = wa_vbap-matnr.
    "sl_vbpok-pikmg          = wa_romaneio-peso_liq.
    sl_vbpok-charg          = wa_vbap-charg.
    sl_vbpok-lgort          = p_wa_entrada_rem-lgort.
    "sl_vbpok-brgew          = wa_romaneio-peso_liq.
    "sl_vbpok-ntgew          = wa_romaneio-peso_liq.
    "sl_vbpok-gewei          = wa_vbap-meins.
    sl_vbpok-vbtyp_n        = 'T'.
    APPEND sl_vbpok TO tl_vbpok.

    CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
      EXPORTING
        vbkok_wa                 = sl_vbkok_wa
        synchron                 = 'X'
        if_error_messages_send_1 = 'X'
      TABLES
        vbpok_tab                = tl_vbpok
        prot                     = tl_prot.

    IF tl_prot[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      "Forçar a liberação do documento gerado
      CALL FUNCTION 'DEQUEUE_ALL'
        EXPORTING
          _synchron = 'X'.
      remessas-l_subrc = 0.
      CLEAR: w_messtab.
      w_messtab-tcode  = sy-tcode.
      w_messtab-msgtyp = 'S'.
      w_messtab-msgnr  = '003'.
      w_messtab-msgv1  = remessas-numero.
      w_messtab-msgv2  = 'Picking efetuado com sucesso!'.
      APPEND w_messtab TO t_messtab.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      remessas-l_subrc = 4.

      LOOP AT tl_prot INTO wa_prot.
        CLEAR: w_messtab.
        w_messtab-tcode  = sy-tcode.
        w_messtab-msgtyp = wa_prot-msgty.
        w_messtab-msgnr  = '003'.
        w_messtab-msgv1  = wa_prot-msgv1.
        w_messtab-msgv2  = wa_prot-msgv2.
        w_messtab-msgv3  = wa_prot-msgv3.
        w_messtab-msgv4  = wa_prot-msgv4.
        APPEND w_messtab TO t_messtab.
      ENDLOOP.

    ENDIF.

    p_deliverys-l_subrc = remessas-l_subrc.
    MODIFY p_deliverys INDEX sy-tabix TRANSPORTING l_subrc.

  ENDLOOP.

ENDFORM.                    " PICKING_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_ORDEM_COMPRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM estornar_ordem_compra  USING p_vbeln TYPE vbeln_va.

  DATA: f_headinx   LIKE bapisdh1x,
        it_retorno  TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
        wa_retorno  TYPE bapiret2.

  CLEAR f_headinx.
  f_headinx-updateflag = 'D'.

"*---> 28/06/2023 - Migração S4 - LO --> Material não foi utilizado
  CALL FUNCTION 'BAPI_SALESORDER_CHANGE'"#EC CI_USAGE_OK[2438131]
    EXPORTING
      salesdocument    = p_vbeln
      order_header_inx = f_headinx
    TABLES
      return           = it_retorno[].

  READ TABLE it_retorno INTO wa_retorno WITH KEY type = 'E'.

  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
  ENDIF.

  LOOP AT it_retorno INTO wa_retorno.
    CLEAR: w_messtab.
    w_messtab-tcode  = sy-tcode.
    w_messtab-msgtyp = wa_retorno-type.
    w_messtab-msgnr  = '004'.
    w_messtab-msgv1  = wa_retorno-message_v1.
    w_messtab-msgv2  = wa_retorno-message_v2.
    w_messtab-msgv3  = wa_retorno-message_v3.
    w_messtab-msgv4  = wa_retorno-message_v4.
    APPEND w_messtab TO t_messtab.
  ENDLOOP.

ENDFORM.                    " ESTORNAR_ORDEM_COMPRA

*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM estornar_delivery  USING    nr_remessa TYPE vbeln_vl
                                 l_subrc    TYPE sy-subrc.

  DATA: sl_hdata     TYPE bapiobdlvhdrchg,
        sl_hcont     TYPE bapiobdlvhdrctrlchg,
        vl_delivery  TYPE bapiobdlvhdrchg-deliv_numb,
        tl_bapiret2  TYPE bapiret2_t,
        wa_bapiret2  TYPE bapiret2.

  sl_hdata-deliv_numb = nr_remessa.
  sl_hcont-deliv_numb = nr_remessa.
  sl_hcont-dlv_del    = 'X'.
  vl_delivery         = nr_remessa.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    TABLES
      return         = tl_bapiret2.

  READ TABLE tl_bapiret2 INTO wa_bapiret2 WITH KEY type = 'E'.

  IF sy-subrc NE 0.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    l_subrc = 0.
  ELSE.
    l_subrc = 4.
  ENDIF.

  LOOP AT tl_bapiret2 INTO wa_bapiret2.
    CLEAR: w_messtab.
    w_messtab-tcode  = sy-tcode.
    w_messtab-msgtyp = wa_bapiret2-type.
    w_messtab-msgnr  = '004'.
    w_messtab-msgv1  = wa_bapiret2-message_v1.
    w_messtab-msgv2  = wa_bapiret2-message_v2.
    w_messtab-msgv3  = wa_bapiret2-message_v3.
    w_messtab-msgv4  = wa_bapiret2-message_v4.
    APPEND w_messtab TO t_messtab.
  ENDLOOP.


ENDFORM.                    " ESTORNAR_DELIVERY
