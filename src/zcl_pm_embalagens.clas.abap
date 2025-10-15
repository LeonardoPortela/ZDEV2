class ZCL_PM_EMBALAGENS definition
  public
  final
  create public .

public section.

  methods SET_DOC_MATERIAL
    importing
      !MATNR type MATNR
      !WERKS type WERKS_D
      !LGORT type LGORT_D
      !CHARG type CHARG_D .
  methods GET_MBLNR
    returning
      value(E_MBLNR) type MBLNR .
  methods GET_MJAHR
    returning
      value(E_MJAHR) type MJAHR .
  methods SET_VENCIMENTO
    importing
      !MATNR type MATNR
      !CHARG type ZDE_LOTE_FORN .
  methods GET_VENCIMENTO
    returning
      value(E_VFDAT) type VFDAT .
  methods GET_LOTE
    returning
      value(E_CHARG) type CHARG_D .
  methods SET_LOTE
    importing
      !LOTE type ZDE_LOTE_FORN .
  methods GET_PEDIDO
    returning
      value(E_EBELN) type EBELN .
  methods SET_PEDIDO
    importing
      !MATNR type MATNR
      !WERKS type WERKS_D
      !LGORT type LGORT_D
      !CHARG type CHARG_D .
  methods CHECK_LOTE
    importing
      !I_MATNR type MATNR
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT
      !I_CHARG type CHARG_D
    exporting
      !E_RETURN type CHAR01 .
  methods CREATE_BATCH
    importing
      !I_MATNR type MATNR
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT
      value(I_CHARG) type CHARG_D optional
      !I_DATE_VENC type SY-DATUM
      !I_LOTE_FABRICANTE type CHARG_D
    exporting
      !E_CHARG type CHARG_D
      value(E_MCHA) type MCHA_TTY .
protected section.
private section.

  data AT_MBLNR type MBLNR .
  data AT_MJAHR type MJAHR .
  data AT_VFDAT type VFDAT .
  data AT_CHARG type CHARG_D .
  data AT_EBELN type EBELN .
ENDCLASS.



CLASS ZCL_PM_EMBALAGENS IMPLEMENTATION.


  METHOD GET_LOTE.
    E_CHARG = AT_CHARG.
  ENDMETHOD.


  METHOD GET_MBLNR.
    E_MBLNR = AT_MBLNR.
  ENDMETHOD.


  METHOD GET_MJAHR.
    E_MJAHR = AT_MJAHR.
  ENDMETHOD.


  METHOD GET_PEDIDO.
    E_EBELN = AT_EBELN.
  ENDMETHOD.


  METHOD GET_VENCIMENTO.
    E_VFDAT = AT_VFDAT.
  ENDMETHOD.


  method set_doc_material.

    clear: at_mblnr, at_mjahr. "Ajuste referente o IR192540 / AOENNING
    select single mblnr mjahr
      from mseg
      into (at_mblnr, at_mjahr)
      where matnr eq matnr
      and werks eq werks
      and lgort eq lgort
      and charg eq charg
      and bwart eq '309'.
  endmethod.


  METHOD SET_LOTE.
    clear: AT_CHARG. "Ajuste referente o IR192540 / AOENNING
    SELECT SINGLE CHARGD FROM ZPPT0016 INTO AT_CHARG WHERE ZLICHA EQ LOTE.
    IF SY-SUBRC IS NOT INITIAL.
      WRITE LOTE TO AT_CHARG.
    ENDIF.
  ENDMETHOD.


  METHOD SET_PEDIDO.

    SELECT SINGLE MBLNR
      FROM MSEG
      INTO @DATA(_MBLNR)
      WHERE CHARG EQ @CHARG
      AND MATNR EQ @MATNR
      AND WERKS EQ @WERKS
      AND LGORT EQ @LGORT
      AND BWART EQ '101'.

    IF SY-SUBRC IS INITIAL.
      SELECT SINGLE EBELN
        FROM EKBE
        INTO AT_EBELN
        WHERE BELNR EQ _MBLNR
        AND BEWTP EQ 'E'
        AND BWART EQ '101'.

    ELSE.
      SELECT SINGLE UMCHA
        FROM MSEG
        INTO @DATA(_UMCHA)
        WHERE CHARG EQ @CHARG
          AND MATNR EQ @MATNR
          AND WERKS EQ @WERKS
          AND LGORT EQ @LGORT
          AND BWART EQ '311' .

      IF ( _UMCHA IS NOT INITIAL ).
        SELECT SINGLE MBLNR
          FROM MSEG
          INTO _MBLNR
          WHERE CHARG EQ _UMCHA
            AND MATNR EQ MATNR
            AND WERKS EQ WERKS
            AND LGORT EQ LGORT
            AND BWART EQ '101'.

        IF SY-SUBRC IS INITIAL.
          SELECT SINGLE EBELN
            FROM EKBE
            INTO AT_EBELN
            WHERE BELNR EQ _MBLNR
            AND BEWTP EQ 'E'
            AND BWART EQ '101'.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_vencimento.

    DATA:lv_matnr TYPE mara-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.


    SELECT SINGLE vfdat
      FROM mch1
      INTO at_vfdat
      WHERE matnr EQ lv_matnr
      AND charg EQ charg.

    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE chargd
        FROM zppt0016
        INTO @DATA(_chargd)
        WHERE matnr EQ @lv_matnr
        AND zlicha EQ @charg.

      IF _chargd IS NOT INITIAL.

        SELECT SINGLE vfdat
          FROM mch1
          INTO at_vfdat
          WHERE matnr EQ lv_matnr
          AND charg EQ _chargd.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  method check_lote.

    check i_matnr is not initial and i_werks is not initial and i_charg is not initial.

    select * from mcha into table @data(WA_mcha)
      where matnr eq @i_matnr
        and werks eq @i_werks
*        and lgort eq @i_lgort
        and charg eq @i_charg.
    if sy-subrc eq 0.
      e_return = abap_true.
    endif.
  endmethod.


  method create_batch.

    data return    type table of bapiret2.
    data new_batch type table of mcha.
    data lv_material type mara-matnr.
    clear: lv_material.

    call function 'CONVERSION_EXIT_MATN1_INPUT'
      exporting
        input        = i_matnr
      importing
        output       = lv_material
      exceptions
        length_error = 1
        others       = 2.



    data(_header) = value mcha( matnr = lv_material werks = i_werks charg = i_charg vfdat = i_date_venc ).

    data(_characteristics) = value clbatch_t(
      ( atnam = 'ZDEFENSIVO_DT_VALIDADE'     atwtb = i_date_venc   )
      ( atnam = 'ZDEFENSIVO_LOTE_FABRICANTE' atwtb = i_lote_fabricante )
    ).

    call function 'VB_CREATE_BATCH'
      exporting
        ymcha                        = _header
        new_lgort                    = i_lgort
        kzcla                        = '2'
        class                        = 'ZDEFENSIVO'
        no_cfc_calls                 = 'X'
      importing
        ymcha                        = _header
      tables
        char_of_batch                = _characteristics
        new_batch                    = e_mcha
        return                       = return
      exceptions
        no_material                  = 1
        no_batch                     = 2
        no_plant                     = 3
        material_not_found           = 4
        plant_not_found              = 5
        stoloc_not_found             = 6
        lock_on_material             = 7
        lock_on_plant                = 8
        lock_on_batch                = 9
        lock_system_error            = 10
        no_authority                 = 11
        batch_exist                  = 12
        stoloc_exist                 = 13
        illegal_batch_number         = 14
        no_batch_handling            = 15
        no_valuation_area            = 16
        valuation_type_not_found     = 17
        no_valuation_found           = 18
        error_automatic_batch_number = 19
        cancelled                    = 20
        wrong_status                 = 21
        interval_not_found           = 22
        number_range_not_extern      = 23
        object_not_found             = 24
        error_check_batch_number     = 25
        no_external_number           = 26
        no_customer_number           = 27
        no_class                     = 28
        error_in_classification      = 29
        inconsistency_in_key         = 30
        region_of_origin_not_found   = 31
        country_of_origin_not_found  = 32
        others                       = 33.

    if sy-subrc is initial.
      call function 'BAPI_TRANSACTION_COMMIT'.


    else.

      call function 'BALW_BAPIRETURN_GET2'
        exporting
          type   = sy-msgty
          cl     = sy-msgid
          number = sy-msgno
          par1   = sy-msgv1
          par2   = sy-msgv2
          par3   = sy-msgv3
          par4   = sy-msgv4
        importing
          return = return.
    endif.
  endmethod.
ENDCLASS.
