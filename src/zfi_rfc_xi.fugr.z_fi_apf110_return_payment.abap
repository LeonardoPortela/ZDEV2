FUNCTION z_fi_apf110_return_payment.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(LAUFD) TYPE  F110V-LAUFD
*"     VALUE(LAUFI) TYPE  F110V-LAUFI
*"----------------------------------------------------------------------

  vg_datum = sy-datum.
  vg_uzeit = sy-uzeit.

*> Identifca termino do JOB de ciclo de pagamento.
  CONCATENATE 'F110-' laufd '-' laufi  INTO vg_jobname.

  DO.
    WAIT UP TO 10 SECONDS.
    SELECT SINGLE jobname
                    FROM tbtco
                    INTO vg_jobname
                   WHERE ( status  EQ 'F'        )
                     AND ( jobname EQ vg_jobname ).
    CHECK ( sy-subrc EQ 0 ).
    EXIT.
  ENDDO.

*> Seleciona dados da proposta de pagamento

  SELECT laufd laufi xvorl zbukr lifnr kunnr empfg vblnr
        FROM reguh
        INTO TABLE it_reguh
       WHERE ( laufd   EQ laufd     )
         AND ( laufi   EQ laufi     )
         AND ( xvorl   EQ c_xvorl   )
         AND ( dorigin EQ c_dorigin ).

  CHECK ( sy-subrc EQ 0 ).

  SELECT vblnr bukrs belnr gjahr buzei koart lifnr waers
        FROM regup
        INTO TABLE it_regup
         FOR ALL ENTRIES IN it_reguh
       WHERE ( laufd EQ it_reguh-laufd )
         AND ( laufi EQ it_reguh-laufi )
         AND ( xvorl EQ it_reguh-xvorl )
         AND ( zbukr EQ it_reguh-zbukr )
         AND ( lifnr EQ it_reguh-lifnr )
         AND ( kunnr EQ it_reguh-kunnr )
         AND ( empfg EQ it_reguh-empfg )
         AND ( vblnr EQ it_reguh-vblnr ).

  SORT it_regup BY vblnr bukrs belnr gjahr buzei koart lifnr waers.
  DELETE ADJACENT DUPLICATES FROM it_regup COMPARING ALL FIELDS.
  DELETE it_regup WHERE ( koart NE c_koart ).

*> Seleciona os dados necessários no documento contábil para
*> retorno ao SIGAM
  IF ( NOT it_regup[] IS INITIAL ).
    SELECT mandt bukrs belnr gjahr awkey
          FROM bkpf
          INTO TABLE it_bkpf
           FOR ALL ENTRIES IN it_regup
         WHERE ( bukrs EQ it_regup-bukrs )
           AND ( belnr EQ it_regup-belnr )
           AND ( gjahr EQ it_regup-gjahr ).

    SORT it_bkpf BY mandt bukrs belnr gjahr awkey.
    DELETE ADJACENT DUPLICATES FROM it_bkpf COMPARING ALL FIELDS.

*    select augbl bukrs belnr gjahr buzei augdt nebtr wrbtr wskto
*       from bseg
*       into table it_bseg
*        for all entries in it_regup
*      where ( bukrs eq it_regup-bukrs )
*        and ( belnr eq it_regup-belnr )
*        and ( gjahr eq it_regup-gjahr )
*        and ( buzei eq it_regup-buzei ).

    SELECT augbl bukrs belnr gjahr buzei augdt wrbtr wskto
      FROM bsak
      INTO TABLE it_docaux
       FOR ALL ENTRIES IN it_regup
     WHERE ( bukrs EQ it_regup-bukrs )
       AND ( belnr EQ it_regup-belnr )
       AND ( gjahr EQ it_regup-gjahr )
       AND ( buzei EQ it_regup-buzei ).

    LOOP AT it_docaux INTO wa_docaux.
      wa_docaux-koart = 'K'.
      wa_docaux-nebtr = wa_docaux-wrbtr - wa_docaux-wskto.
      MODIFY it_docaux FROM wa_docaux.
    ENDLOOP.
    SORT it_docaux BY augbl bukrs belnr gjahr
                      buzei augdt wrbtr wskto
                      koart nebtr.
    DELETE ADJACENT DUPLICATES FROM it_docaux COMPARING ALL FIELDS .

  ENDIF.


  SORT it_regup BY vblnr bukrs belnr gjahr buzei.
  SORT it_bkpf  BY bukrs belnr gjahr.
  SORT it_docaux  BY augbl bukrs belnr gjahr buzei.

  REFRESH it_items_payment.

  LOOP AT it_regup INTO wa_regup.

    READ TABLE it_bkpf INTO wa_bkpf
                   WITH KEY  bukrs = wa_regup-bukrs
                             belnr = wa_regup-belnr
                             gjahr = wa_regup-gjahr BINARY SEARCH.
    IF ( sy-subrc NE 0 ).
      CLEAR wa_bkpf.
    ENDIF.




    READ TABLE it_docaux INTO wa_docaux
                   WITH KEY augbl = wa_regup-vblnr
                            bukrs = wa_regup-bukrs
                            belnr = wa_regup-belnr
                            gjahr = wa_regup-gjahr
                            buzei = wa_regup-buzei BINARY SEARCH.
    IF ( sy-subrc NE 0 ). CLEAR wa_bseg. ENDIF.

    CLEAR wa_items_payment.
    wa_items_payment-obj_key        = wa_bkpf-awkey.
    wa_items_payment-belnr          = wa_regup-belnr.
    wa_items_payment-augdt          = wa_docaux-augdt.
    wa_items_payment-augbl          = wa_docaux-augbl.
    wa_items_payment-nebtr          = wa_docaux-nebtr.
    wa_items_payment-wrbtr          = wa_docaux-wrbtr.
    wa_items_payment-wskto          = wa_docaux-wskto.
    wa_items_payment-st_atualizacao = 'P'.
    wa_items_payment-dt_atualizacao = vg_datum.
    wa_items_payment-hr_atualizacao = vg_uzeit.
    wa_items_payment-cd_transacao   = 'F-43'.


    APPEND wa_items_payment TO it_items_payment.

  ENDLOOP.

  CHECK ( NOT it_items_payment[] IS INITIAL ).

*--> 24.08.2023 17:21:16 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_FI_OUTBOUND_PAYMENT_AP' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_AP'
*    AS SEPARATE UNIT
*    TABLES
*      outpayment = it_items_payment.
*
*  COMMIT WORK.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_PAYMENT_AP'.

  CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
    EXPORTING
      i_fm          = c_fm
    IMPORTING
      e_rfc         = lv_rfc
    EXCEPTIONS
      no_rfc        = 1
      no_rfc_config = 2
      OTHERS        = 3.

  IF sy-subrc EQ 0.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      DESTINATION lv_rfc
      AS SEPARATE UNIT
      TABLES
        outpayment = it_items_payment.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outpayment = it_items_payment.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 17:21:24 - Migração S4 – ML – Fim

  CLEAR wa_testetxt.
  CONCATENATE  wa_items_payment-obj_key wa_items_payment-belnr
               wa_items_payment-augdt   wa_items_payment-augbl
               wa_items_payment-st_atualizacao
               wa_items_payment-dt_atualizacao
               wa_items_payment-hr_atualizacao
               wa_items_payment-cd_transacao
               INTO wa_testetxt SEPARATED BY space.

**  CALL FUNCTION 'Z_GERALDOTESTE'
**    EXPORTING
**      registro = wa_testetxt
**      funcao   = 'Z_FI_OUTBOUND_PAYMENT'.



ENDFUNCTION.
