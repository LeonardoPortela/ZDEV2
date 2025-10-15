FUNCTION z_sample_process_00001430.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BKDF) TYPE  BKDF OPTIONAL
*"  TABLES
*"      T_BKPF STRUCTURE  BKPF
*"      T_BSEG STRUCTURE  BSEG
*"      T_BSEC STRUCTURE  BSEC OPTIONAL
*"      T_BKPFSUB STRUCTURE  BKPF_SUBST
*"      T_BSEGSUB STRUCTURE  BSEG_SUBST
*"  CHANGING
*"     REFERENCE(I_BKDFSUB) TYPE  BKDF_SUBST OPTIONAL
*"----------------------------------------------------------------------

  DATA: vl_bkpf        LIKE bkpf,
        vl_bseg        LIKE bseg,
        at_bseg        LIKE bseg,
        at_bkpf        LIKE bkpf,
        at_bsak        LIKE bsak,
        t_ekbe         TYPE TABLE OF ekbe,
        t_bsik         TYPE TABLE OF bsik,
        w_xslc1        TYPE zmmt0037-ebeln,
        w_xslc2        TYPE zmmt0035-ebeln,
        w_ebeln_37     TYPE zmmt0037-ebeln,
        w_ebeln_35     TYPE zmmt0035-ebeln,
        wl_n_uteis     TYPE sy-index,
        wa_data        TYPE sy-datum,
        vl_hrpay(1),
        wl_sab_dom_fer TYPE TABLE OF iscal_day WITH HEADER LINE.


  DATA: w_header    TYPE zimp_cabecalho.
  DATA: w_zlspr    TYPE bseg-zlspr,
        w_lifnr    TYPE rbkp-lifnr,
        vhbkid     TYPE zglt036-hbkid,
        vbvtyp     TYPE zglt036-bvtyp,
        vdoc_lcto  TYPE zglt035-doc_lcto,
        wa_setleaf TYPE setleaf,
        it_setleaf LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.

  DATA: xdt   TYPE bseg-zfbdt,
        xdias TYPE bseg-zbd1t.

*** AMAGGI - Marcus Luciano
  LOOP AT t_bseg INTO vl_bseg.

    IF ( ( sy-tcode = 'MIR4' ) OR ( sy-tcode = 'FB02' ) OR ( sy-tcode = 'FBL1N' ) OR ( sy-tcode = 'FB03' ) OR ( sy-tcode = 'FK10N' ) ). " AND ( VL_BSEG-ZLSPR NE 'Z' ).

      CLEAR: at_bseg.

      DATA etl52c6r5705 TYPE TABLE OF bseg.
      DATA rldnr_l52c6r6581 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l52c6r6581
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr   = rldnr_l52c6r6581
            i_bukrs   = vl_bseg-bukrs
            i_belnr   = vl_bseg-belnr
            i_gjahr   = vl_bseg-gjahr
            i_buzei   = vl_bseg-buzei
          IMPORTING
            et_bseg   = etl52c6r5705
          EXCEPTIONS
            not_found = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl52c6r5705 ) = 1.
        at_bseg = etl52c6r5705[ 1 ].
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.




      IF at_bseg-zlspr EQ 'Z' OR vl_bseg-zfbdt NE at_bseg-zfbdt OR vl_bseg-zbd1t NE at_bseg-zbd1t.

        "somente se for menor que 72 horas
        wa_data = sy-datum.
                                                            "IR0077493
        ADD vl_bseg-zbd1t TO vl_bseg-zfbdt.

        ADD 3 TO wa_data.
        REFRESH wl_sab_dom_fer.
        CALL FUNCTION 'HOLIDAY_GET'
          EXPORTING
*           HOLIDAY_CALENDAR           = ' '
            factory_calendar           = 'ZF'
            date_from                  = sy-datum
            date_to                    = vl_bseg-zfbdt
          TABLES
            holidays                   = wl_sab_dom_fer
          EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            date_has_invalid_format    = 3
            date_inconsistency         = 4
            OTHERS                     = 5.

        DESCRIBE TABLE wl_sab_dom_fer LINES wl_n_uteis.

        ADD wl_n_uteis TO wa_data.
        IF  wa_data GT vl_bseg-zfbdt.
          "Opter Área de contabilidade de custos
          SELECT SINGLE * INTO wa_setleaf
            FROM setleaf
           WHERE setname EQ 'MIRO72'
             AND valfrom EQ sy-uname.

          IF NOT sy-subrc IS INITIAL.

            SELECT * INTO TABLE it_setleaf
              FROM setleaf
             WHERE setname EQ 'MIRO72'.

            IF sy-subrc IS INITIAL.
              READ TABLE it_setleaf INTO wa_setleaf INDEX 1.
              CONCATENATE space wa_setleaf-valfrom INTO wa_setleaf-valfrom SEPARATED BY space.
              MESSAGE e001(00) WITH 'Alteração de Data de Vencimento ou Desbloqueio'
                                    ' solicitar para Área de Pagadoria'.
            ELSE.
              MESSAGE e001(00) WITH 'Não existe parâmetro de permissão de usuário'
                                    'para liberar esta fatura!'.
            ENDIF.

          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

*** AMAGGI - Camila Brand
    IF ( ( sy-tcode = 'FB02' ) OR ( sy-tcode = 'FBL1N' ) OR ( sy-tcode = 'FB03' ) ). " AND VL_BSEG-BSCHL EQ '31'.
      CLEAR: at_bkpf,
             at_bseg.

*** ALRS 24/08/2015 IR 126007
      IF sy-tcode = 'FB02'.
        IF vl_bseg-zfbdt IS NOT INITIAL.
          REFRESH wl_sab_dom_fer.
          wa_data  = vl_bseg-zfbdt - 3.
          CALL FUNCTION 'HOLIDAY_GET'
            EXPORTING
*             HOLIDAY_CALENDAR           = ' '
              factory_calendar           = 'ZF'
              date_from                  = wa_data
              date_to                    = vl_bseg-zfbdt
            TABLES
              holidays                   = wl_sab_dom_fer
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.

          DESCRIBE TABLE wl_sab_dom_fer LINES wl_n_uteis.

          IF vl_bseg-zlsch NE 'D'.

            READ TABLE wl_sab_dom_fer WITH KEY date = vl_bseg-zfbdt.

            IF sy-subrc = 0.
              IF sy-cprog NE 'Z_SHDB_FB02'.
                MESSAGE e398(00) WITH 'Vencimento não é dia útil'.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
      DATA etl159c6r7485 TYPE TABLE OF bseg.
      DATA rldnr_l159c6r6108 TYPE rldnr.
      CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
        IMPORTING
          e_rldnr       = rldnr_l159c6r6108
        EXCEPTIONS
          not_found     = 1
          more_than_one = 2.
      IF sy-subrc = 0.
        CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
          EXPORTING
            i_rldnr   = rldnr_l159c6r6108
            i_bukrs   = vl_bseg-bukrs
            i_belnr   = vl_bseg-belnr
            i_gjahr   = vl_bseg-gjahr
            i_buzei   = vl_bseg-buzei
          IMPORTING
            et_bseg   = etl159c6r7485
          EXCEPTIONS
            not_found = 1.
      ENDIF.
      IF sy-subrc = 0 AND lines( etl159c6r7485 ) = 1.
        at_bseg = etl159c6r7485[ 1 ].
        sy-dbcnt = 1.
      ELSE.
        sy-subrc = 4.
        sy-dbcnt = 0.
      ENDIF.



      SELECT SINGLE * INTO at_bkpf
       FROM bkpf
      WHERE bukrs EQ vl_bseg-bukrs
        AND belnr EQ vl_bseg-belnr
        AND gjahr EQ vl_bseg-gjahr.

*-Equalização RISE x PRD - 21.08.2023 - JT - inicio
      "108343 CS2023000180 Retirar trava de alteração de dados bancários - ZGL - PSA
*      "ALRS
*      IF at_bkpf-awkey+0(5) = 'ZGL17' AND vl_bseg-koart = 'K'. " lançamentos manuais
*        IF vl_bseg-hbkid NE at_bseg-hbkid OR vl_bseg-bvtyp NE at_bseg-bvtyp.
*          MESSAGE e001(00) WITH 'Para este lançamento não pode ser'
*                                ' incluído ou alterar o Banco Empresa '
*                                ' ou Parceiro, registro criado pela ZGL016.'.
*        ENDIF.
*      ENDIF.
*-Equalização RISE x PRD - 21.08.2023 - JT - fim

      IF vl_bseg-bschl EQ '31'.
        CLEAR : xdt,
                xdias.

        xdt   =  at_bseg-zfbdt.
        xdias =  at_bseg-zbd1t.


        IF ( ( xdt NE vl_bseg-zfbdt ) OR ( xdias NE vl_bseg-zbd1t ) ).
          CLEAR  wl_n_uteis.
          REFRESH wl_sab_dom_fer.
          wa_data = sy-datum.
          ADD 3 TO wa_data.
          CALL FUNCTION 'HOLIDAY_GET'
            EXPORTING
              factory_calendar           = 'ZF'
              date_from                  = sy-datum
              date_to                    = vl_bseg-zfbdt
            TABLES
              holidays                   = wl_sab_dom_fer
            EXCEPTIONS
              factory_calendar_not_found = 1
              holiday_calendar_not_found = 2
              date_has_invalid_format    = 3
              date_inconsistency         = 4
              OTHERS                     = 5.


          DESCRIBE TABLE wl_sab_dom_fer LINES wl_n_uteis.

          ADD wl_n_uteis TO wa_data.
          CLEAR vl_hrpay.
          IF at_bkpf-awtyp   = 'HRPAY'.
            IF ( vl_bseg-zfbdt < sy-datum ).
              MESSAGE e001(00) WITH 'Data de vencimento inválida !'.
            ENDIF.
          ELSE.
            SELECT SINGLE * INTO wa_setleaf
              FROM setleaf
             WHERE setname EQ 'MAGGI_FB02_HCM72'
               AND valfrom EQ sy-uname.
            IF sy-subrc = 0.
              MESSAGE e001(00) WITH 'Sem autorização para alterar docs diferente de HRPAY !'.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.
*** AMAGGI - Camila Brand

*** PBI - 60951 - CS2020001160 ZMM0149 - Inicio - CSB
    IF ( ( sy-tcode = 'FB02' ) OR ( sy-tcode = 'FBL1N' )  ).

      SELECT SINGLE * INTO at_bsak
          FROM bsak
         WHERE bukrs EQ vl_bseg-bukrs
           AND belnr EQ vl_bseg-belnr
           AND gjahr EQ vl_bseg-gjahr.

      SELECT SINGLE ebeln FROM zmmt0037
        INTO w_xslc1
        WHERE ebeln	=	at_bsak-ebeln.

      IF w_xslc1 IS NOT INITIAL.
        SELECT SINGLE ebeln FROM zmmt0037
          INTO w_ebeln_37
          WHERE nro_sol_cp  = w_xslc1.

        IF w_ebeln_37 IS NOT INITIAL.
          SELECT *
            FROM ekbe
            INTO TABLE t_ekbe
          WHERE ebeln EQ w_ebeln_37
            AND vgabe EQ '4'.

          IF t_ekbe IS NOT INITIAL.

            SELECT *
              FROM bsik
            INTO TABLE t_bsik
              FOR ALL ENTRIES IN  t_ekbe
                WHERE bukrs EQ vl_bseg-bukrs
                 AND gjahr EQ t_ekbe-gjahr
                 AND belnr EQ t_ekbe-belnr.

            IF t_bsik IS NOT INITIAL.
              MESSAGE e001(00) WITH 'Para a Solicitação de Compras' w_xslc1
              'tem adiantamento em Aberto!'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      SELECT SINGLE ebeln FROM zmmt0035
        INTO w_xslc2
        WHERE ebeln	=	at_bsak-ebeln.

      IF w_xslc2 IS NOT INITIAL.

        SELECT SINGLE ebeln FROM zmmt0035
          INTO w_ebeln_35
          WHERE nro_sol_cp  = w_xslc2.

        IF w_ebeln_35 IS NOT INITIAL.
          SELECT  *
          FROM ekbe
            INTO TABLE t_ekbe
          WHERE ebeln EQ w_ebeln_35
          AND vgabe EQ '4'.

          IF t_ekbe IS NOT INITIAL.
            SELECT *
              FROM bsik
            INTO TABLE t_bsik
              FOR ALL ENTRIES IN  t_ekbe
                WHERE bukrs EQ vl_bseg-bukrs
                 AND gjahr EQ t_ekbe-gjahr
                 AND belnr EQ t_ekbe-belnr.

            IF t_bsik IS NOT INITIAL.
              MESSAGE e001(00) WITH 'Para a Solicitação de Compras'  w_xslc2
              'tem adiantamento em Aberto!'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*** PBI - 60951 - CS2020001160 ZMM0149 - Fim - CSB
  ENDLOOP.
*** AMAGGI - Marcus Luciano

* Modificação - Eduardo Ruttkowski Tavares - 01.06.2009 - Exit - FB02


  CLEAR: wa_setleaf.



  LOOP AT t_bseg INTO vl_bseg.


    SELECT SINGLE * INTO wa_setleaf
         FROM setleaf
        WHERE setname EQ 'MAGGI_EMPRESA_EXTERIOR'
          AND valfrom EQ vl_bseg-bukrs.

    IF ( sy-subrc EQ 0 ).
      CONTINUE.
    ELSE.
*** Rollout - Eduardo Ruttkowski Tavares - 13.08.2009 - EXIT FB02
      IF sy-tcode = 'FB02'.

        IF NOT vl_bseg-zfbdt IS INITIAL.
          IF vl_bseg-zlsch = 'D'.
            UPDATE zimp_contas_cons SET   zfbdt = vl_bseg-zfbdt
                                          zbd1t = vl_bseg-zbd1t

                                    WHERE bukrs = vl_bseg-bukrs AND
                                          belnr = vl_bseg-belnr AND
                                          gjahr = vl_bseg-gjahr.

          ENDIF.
          READ TABLE t_bkpf INTO vl_bkpf INDEX 1.
          IF vl_bkpf-blart = 'TB'.
            MESSAGE e001(00) WITH 'A data de vencimento deste documento não pode ser alterado foi criado pela ZIMP.'.
*            UPDATE ZIMP_CABECALHO     SET   ZFBDT    = VL_BSEG-ZFBDT
*
*                                    WHERE BUKRS      = VL_BSEG-BUKRS AND
*                                          BELNR      = VL_BSEG-BELNR AND
*                                          GJAHR      = VL_BSEG-GJAHR.
          ENDIF.
        ENDIF.
      ENDIF.
*** Rollout - Eduardo Ruttkowski Tavares - 13.08.2009 - EXIT FB02

      IF vl_bseg-zlspr IS INITIAL.
        DATA etl359c8r558 TYPE TABLE OF bseg.
        DATA lt_fields_l359c8r3441 TYPE fagl_t_field.
        lt_fields_l359c8r3441 = VALUE #( ( line = 'ZLSPR' )
         ).
        DATA rldnr_l359c8r5601 TYPE rldnr.
        CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
          IMPORTING
            e_rldnr       = rldnr_l359c8r5601
          EXCEPTIONS
            not_found     = 1
            more_than_one = 2.
        IF sy-subrc = 0.
          CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
            EXPORTING
              i_rldnr      = rldnr_l359c8r5601
              i_bukrs      = vl_bseg-bukrs
              i_belnr      = vl_bseg-belnr
              i_gjahr      = vl_bseg-gjahr
              i_buzei      = vl_bseg-buzei
              it_fieldlist = lt_fields_l359c8r3441
            IMPORTING
              et_bseg      = etl359c8r558
            EXCEPTIONS
              not_found    = 1.
        ENDIF.
        IF sy-subrc = 0 AND lines( etl359c8r558 ) = 1.
          w_zlspr = etl359c8r558[ 1 ]-zlspr.
          sy-dbcnt = 1.
        ELSE.
          sy-subrc = 4.
          sy-dbcnt = 0.
        ENDIF.

        IF NOT w_zlspr IS INITIAL.
          READ TABLE t_bkpf INTO vl_bkpf INDEX 1.
          IF vl_bkpf-tcode = 'MIRO'.

            SELECT SINGLE lifnr FROM rbkp INTO w_lifnr
              WHERE belnr = vl_bkpf-awkey(10) AND
                    gjahr = vl_bseg-gjahr.

            IF sy-subrc = 0.
              SELECT SINGLE bsik~lifnr
                FROM bsik
                INNER JOIN bkpf
                ON   bkpf~bukrs = bsik~bukrs
                AND  bkpf~belnr = bsik~belnr
                AND  bkpf~gjahr = bsik~gjahr
                AND  bkpf~blart NE 'VC'
                INTO w_lifnr
                WHERE bsik~bukrs = vl_bkpf-bukrs AND
                      bsik~lifnr = w_lifnr       AND
                      bsik~bschl = '29' .

              IF sy-subrc = 0.
*-Equalização RISE x PRD - 28.08.2023 - JT - inicio
                SELECT SINGLE * "CS2023000596 exceção para pagto
                  FROM zfit185
                  INTO @DATA(w_zfit0185)
                  WHERE bukrs = @vl_bseg-bukrs AND
                        belnr = @vl_bseg-belnr AND
                        gjahr = @vl_bseg-gjahr.
                IF sy-subrc NE 0.
                  MESSAGE e019(zfi)." with 'Existe adiantamento a ser compesando'.
                ENDIF.
*-Equalização RISE x PRD - 28.08.2023 - JT - fim
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
* Modificação - Eduardo Ruttkowski Tavares - 01.06.2009 - Exit - FB02

  CHECK sy-mandt = '160' OR
        sy-mandt = '300'.

  READ TABLE t_bkpf INTO vl_bkpf INDEX 1.
  READ TABLE t_bseg INTO vl_bseg INDEX 1.

  CLEAR vg_testeabap_debug.

  IF ( vg_testeabap_debug IS INITIAL ).
    CALL FUNCTION 'Z_FI_DOCUMENT_CHANGE_RET' IN BACKGROUND TASK
      DESTINATION 'NONE'
      EXPORTING
        i_bkpf = vl_bkpf
        i_bseg = vl_bseg.
  ELSE.
    CALL FUNCTION 'Z_FI_DOCUMENT_CHANGE_RET' STARTING NEW TASK 'CHG'
      EXPORTING
        i_bkpf = vl_bkpf
        i_bseg = vl_bseg.

  ENDIF.

ENDFUNCTION.
