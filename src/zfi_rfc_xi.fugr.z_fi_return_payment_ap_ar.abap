************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...:                                                     *
* Criação     ...: Geraldo Santana                                     *
* Responsável ...: Claudionor Oliveira Junior                          *
* Data Alteração.: 08.11.2007                                          *
* Tipo de prg ...: Modulo de função                                    *
* Objetivo    ...: Função que seleciona os pagamentos e start de       *
*                  outbound com o SIGAM (AP/AR)                        *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 08.11.2007    Claudionor                                DEVK903099   *
*    Motivo: Estava ignorando registros do tipo AB (contabeis) mas     *
*           para baixa de registro de desconto, isso deve voltar para  *
*           o SIGAM                                                    *
* 12.03.2008    Michely                                                *
*    Motivo: Problema de performance.                                  *
* 02.04.2008    Michely                                   DEVK903816   *
*    Motivo: Quando compenção de AP x AR estava retornando apenas o AR *
************************************************************************

FUNCTION z_fi_return_payment_ap_ar.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_BUKRS) LIKE  T001-BUKRS
*"     VALUE(I_AUGBL) LIKE  BSEG-AUGBL
*"     VALUE(I_GJAHR) LIKE  BSEG-GJAHR
*"     VALUE(I_TCODE) LIKE  SY-TCODE
*"----------------------------------------------------------------------
  CHECK sy-mandt = '160' OR
        sy-mandt = '300'.

  DATA: vl_found  TYPE i,
        vl_table  TYPE string,
        vl_tabaux TYPE string.

  REFRESH r_gjhar.
  CLEAR r_gjhar.
  r_gjhar-sign = 'I'.
  r_gjhar-option = 'BT'.
  r_gjhar-low  = '2007'.
  r_gjhar-high = '2012'.
  APPEND r_gjhar.

  vg_datum = sy-datum.
  vg_uzeit = sy-uzeit.

*> Verifica se a primeira possibilidade já foi criada na BKPF
  CLEAR vl_found.

  SELECT SINGLE mandt bukrs belnr gjahr awkey
    FROM bkpf
      INTO wa_bkpf
    WHERE ( bukrs EQ i_bukrs )
      AND ( belnr EQ i_augbl )
      AND ( gjahr EQ i_gjahr ).

  CHECK sy-subrc IS INITIAL.

*  do 3 times.
**    wait up to 10 seconds.
*    select single mandt bukrs belnr gjahr awkey
*      from bkpf
*      into wa_bkpf
*     where ( bukrs eq i_bukrs )
*       and ( belnr eq i_augbl )
*       and ( gjahr in r_gjhar ).
*
*    check ( sy-subrc eq 0 ).
*    vl_found = 1.
*    exit.
*  enddo.
*  check ( not vl_found is initial ).

  REFRESH: it_docaux, it_bkpf, it_bseg.

*> Seleciona os dados necessários no documento contábil para
*> retorno ao SIGAM
  CLEAR vg_koart.
  DO 2 TIMES.
    IF ( sy-index EQ 1 ).
      vl_table  = 'bsak'. "Fornecedor
    ELSE.
      vl_table  = 'bsad'. "Cliente
    ENDIF.
*> Alterar para pegar um range somente no intervalo do subnro
    SELECT augbl bukrs belnr gjahr buzei augdt wrbtr wskto
      FROM (vl_table)
      APPENDING TABLE it_docaux
     WHERE ( bukrs EQ i_bukrs )
       AND ( augbl EQ i_augbl )
       AND ( gjahr EQ i_gjahr ).
    IF sy-subrc EQ 0 AND vl_table EQ 'bsak'.
      vl_tabaux = vl_table.
      vg_koart = 'K'.
    ENDIF.
    IF sy-subrc EQ 0 AND vl_table EQ 'bsad'.
      vl_tabaux = vl_table.
      vg_koart = 'D'.
    ENDIF.
    LOOP AT it_docaux INTO wa_docaux WHERE koart IS INITIAL.
      wa_docaux-koart = vg_koart.
      wa_docaux-nebtr = wa_docaux-wrbtr - wa_docaux-wskto.
      MODIFY it_docaux FROM wa_docaux.
    ENDLOOP.
  ENDDO.

  IF ( NOT it_docaux[] IS INITIAL ).

    SELECT mandt bukrs belnr gjahr awkey blart
          FROM bkpf
          INTO TABLE it_bkpf
           FOR ALL ENTRIES IN it_docaux
         WHERE ( bukrs EQ it_docaux-bukrs )
           AND ( belnr EQ it_docaux-belnr )
           AND ( gjahr EQ it_docaux-gjahr ).

    SORT it_bkpf BY mandt bukrs belnr gjahr awkey blart.
    DELETE ADJACENT DUPLICATES FROM it_bkpf COMPARING ALL FIELDS.

*    if ( i_tcode ne 'FBZ2' ).
*      delete it_bkpf where ( blart eq c_blart ).
*    endif.
    SORT it_bkpf  BY bukrs belnr gjahr.

    LOOP AT it_docaux INTO wa_docaux.
      READ TABLE it_bkpf INTO wa_bkpf
                     WITH KEY  bukrs = wa_docaux-bukrs
                               belnr = wa_docaux-belnr
                               gjahr = wa_docaux-gjahr
                                             BINARY SEARCH.
      CHECK ( sy-subrc NE 0 ).
      DELETE it_docaux.

    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM it_docaux COMPARING ALL FIELDS.
*    DELETE it_bseg WHERE ( koart NE 'K' )
*                     AND ( koart NE 'D' ).
  ENDIF.

  SORT it_docaux  BY augbl bukrs belnr gjahr buzei.

  REFRESH: it_items_payment_k, it_items_payment_d.

  LOOP AT it_docaux INTO wa_docaux.

    READ TABLE it_bkpf INTO wa_bkpf
                   WITH KEY  bukrs = wa_docaux-bukrs
                             belnr = wa_docaux-belnr
                             gjahr = wa_docaux-gjahr BINARY SEARCH.
    IF ( sy-subrc NE 0 ). CLEAR wa_bkpf. ENDIF.

    CLEAR wa_items_payment.
    wa_items_payment-obj_key        = wa_bkpf-awkey.
    wa_items_payment-belnr          = wa_docaux-belnr.
    wa_items_payment-augdt          = wa_docaux-augdt.
    wa_items_payment-augbl          = wa_docaux-augbl.
    wa_items_payment-nebtr          = wa_docaux-nebtr.
    wa_items_payment-wrbtr          = wa_docaux-wrbtr.
    wa_items_payment-wskto          = wa_docaux-wskto.
    wa_items_payment-st_atualizacao = 'P'.
    wa_items_payment-dt_atualizacao = vg_datum.
    wa_items_payment-hr_atualizacao = vg_uzeit.
    wa_items_payment-cd_transacao   = i_tcode.

    IF NOT wa_bkpf-awkey IS INITIAL.
      IF ( wa_docaux-koart EQ 'K' ).
        APPEND wa_items_payment TO it_items_payment_k.
      ELSE.
        APPEND wa_items_payment TO it_items_payment_d.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF ( NOT it_items_payment_k[] IS INITIAL ).

*--> 24.08.2023 17:21:16 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_PAYMENT_AP' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_AP'
*      AS SEPARATE UNIT
*      TABLES
*        outpayment = it_items_payment_k.
*
*    COMMIT WORK.

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
          outpayment = it_items_payment_k.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outpayment = it_items_payment_k.
    ENDIF.

    COMMIT WORK.
*<-- 24.08.2023 17:21:24 - Migração S4 – ML – Fim

  ENDIF.

** ---> S4 Migration - 09/08/2023 - GB - Inicio
*  DATA: lo_zco_z_fi_out_paymentarp TYPE REF TO zco_z_fi_outbound_payment_ar_p,
*        ls_input                   TYPE zzfi_outbound_payment_ar_inp1.
** <--- S4 Migration - 09/08/2023 - GB - Fim
*  IF ( NOT it_items_payment_d[] IS INITIAL ).
** ---> S4 Migration - 09/08/2023 - GB - Inicio
**    call function 'Z_FI_OUTBOUND_PAYMENT_AR' in background task
**      destination 'XI_SIGAM_AR'
**      as separate unit
**      tables
**        outpayment = it_items_payment_d.
*
*    CREATE OBJECT lo_zco_z_fi_out_paymentarp.
*
*    LOOP AT it_items_payment_d INTO DATA(ls_it_items_payment_d).
*
*      APPEND INITIAL LINE TO ls_input-outpayment-item ASSIGNING FIELD-SYMBOL(<fs_input>).
*
*      MOVE-CORRESPONDING ls_it_items_payment_d TO <fs_input>.
*
*    ENDLOOP.
*    TRY.
*        lo_zco_z_fi_out_paymentarp->z_fi_outbound_payment_ar(
*          EXPORTING
*            input  = ls_input ).
*      CATCH cx_ai_system_fault. " Communication Error
*    ENDTRY.
** <--- S4 Migration - 09/08/2023 - GB - Fim
*    COMMIT WORK.
*  ENDIF.

  IF ( NOT it_items_payment_d[] IS INITIAL ).
*    call function 'Z_FI_OUTBOUND_PAYMENT_AR' in background task
*      destination 'XI_SIGAM_AR'
*      as separate unit
*      tables
*        outpayment = it_items_payment_d.

    DATA: lv_rfc2 TYPE rfcdest.

    CONSTANTS: c_fm2 TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_PAYMENT_AR'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm2
      IMPORTING
        e_rfc         = lv_rfc2
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm2 IN BACKGROUND TASK
        DESTINATION lv_rfc2
        AS SEPARATE UNIT
        TABLES
          outpayment = it_items_payment_d.
    ELSE.
      CALL FUNCTION c_fm2 IN BACKGROUND TASK
        TABLES
          outpayment = it_items_payment_d.
    ENDIF.

    COMMIT WORK.
  ENDIF.

ENDFUNCTION.
