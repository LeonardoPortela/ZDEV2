class ZCL_COMPENSACAO_AUTOMATICA definition
  public
  final
  create public .

public section.

  class-methods SELECIONA_DADOS
    importing
      !I_BUKRS type BUKRS optional
    exporting
      !E_DADOS type ZSDT_COMPENSACAO_AUTOMATICA .
  class-methods EXECUTA_COMPENSACAO
    importing
      !I_DADOS type ZSDT_COMPENSACAO_AUTOMATICA .
protected section.
private section.
ENDCLASS.



CLASS ZCL_COMPENSACAO_AUTOMATICA IMPLEMENTATION.


  METHOD executa_compensacao.
    DATA: lt_bdcdata  TYPE TABLE OF bdcdata,
          lt_messages TYPE TABLE OF bdcmsgcoll,
          lv_mode     TYPE char1 VALUE 'N',
          lv_item     TYPE numc2,
          lv_okcode   TYPE char1.

    FREE: lt_bdcdata,
          lt_messages.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING FIELD-SYMBOL(<fs_bdcdata>).
    <fs_bdcdata>-program = 'SAPF124'.
    <fs_bdcdata>-dynpro = '1000'.
    <fs_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'BDC_OKCODE'.
    <fs_bdcdata>-fval = '=%005'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-program = 'SAPLALDB'.
    <fs_bdcdata>-dynpro = '3000'.
    <fs_bdcdata>-dynbegin = abap_true.

    DESCRIBE TABLE i_dados LINES DATA(lv_lines).

    DATA(lv_qtd_restante) = lv_lines.
    LOOP AT i_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

      ADD 1 TO lv_item.

      IF lv_item > 8 AND lv_lines > 8.
        lv_item = '02'.
        CLEAR lv_okcode.
      ENDIF.

      IF lv_qtd_restante > 8 AND lv_item = '01' AND lv_okcode IS INITIAL.
        lv_okcode = abap_true.
        APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
        <fs_bdcdata>-fnam = 'BDC_OKCODE'.
        <fs_bdcdata>-fval = '=P+'.
      ELSEIF lv_qtd_restante <= 8 AND ( lv_item = '01' OR lv_item EQ '02' ) AND lv_okcode IS INITIAL.
        lv_okcode = abap_true.
        APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
        <fs_bdcdata>-fnam = 'BDC_OKCODE'.
        <fs_bdcdata>-fval = '=ACPT'.
      ENDIF.

      APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
      <fs_bdcdata>-fnam = 'RSCSEL_255-SLOW_I(' && lv_item && ')'.
      <fs_bdcdata>-fval = <fs_dados>-belnr.

      lv_qtd_restante = lv_qtd_restante - 1.

    ENDLOOP.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-program = 'SAPF124'.
    <fs_bdcdata>-dynpro = '1000'.
    <fs_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'BDC_OKCODE'.
    <fs_bdcdata>-fval = '=ONLI'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'BUKRX-LOW'.
    <fs_bdcdata>-fval = <fs_dados>-bukrs.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'GJAHX-LOW'.
    <fs_bdcdata>-fval = <fs_dados>-gjahr.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'X_TESTL'.
    <fs_bdcdata>-fval = abap_false.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'P_RDATUM'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'X_KUNNR'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'X_LIFNR'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'X_SAKNR'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'AUGDT'.
    <fs_bdcdata>-fval = sy-datum+6(2) && sy-datum+4(2) && sy-datum(4).

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'XAUSBEL'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'XNAUSBEL'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'X_FEHLER'.
    <fs_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-program = 'SAPF124'.
    <fs_bdcdata>-dynpro = '1000'.
    <fs_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'BDC_OKCODE'.
    <fs_bdcdata>-fval = '/00'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-program = 'SAPMSSY0'.
    <fs_bdcdata>-dynpro = '0120'.
    <fs_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'BDC_OKCODE'.
    <fs_bdcdata>-fval = '=BACK'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-program = 'SAPF124'.
    <fs_bdcdata>-dynpro = '1000'.
    <fs_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <fs_bdcdata>.
    <fs_bdcdata>-fnam = 'BDC_OKCODE'.
    <fs_bdcdata>-fval = '/EE'.

    CALL TRANSACTION 'F.13' USING lt_bdcdata MESSAGES INTO lt_messages MODE lv_mode.

    SORT lt_messages BY msgtyp.

    READ TABLE lt_messages TRANSPORTING NO FIELDS
    WITH KEY msgtyp = 'E'
    BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Documentos compensados com sucesso' TYPE 'S'.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD seleciona_dados.

    DATA lt_dados TYPE zsdt_compensacao_automatica.

    IF i_bukrs IS INITIAL.
      SELECT bukrs, doc_lcto, gjahr
        FROM zglt035
        INTO TABLE @DATA(lt_035)
        WHERE prov_est = @abap_true
          AND dt_entrada = @sy-datum .
    ELSE.
      SELECT bukrs doc_lcto gjahr
        FROM zglt035
        INTO TABLE lt_035
        WHERE bukrs = i_bukrs
          AND prov_est = abap_true
          AND dt_entrada = sy-datum .
    ENDIF.

    IF sy-subrc IS INITIAL.
      DATA(lt_035_aux) = lt_035.

      SORT lt_035_aux BY bukrs.
      DELETE ADJACENT DUPLICATES FROM lt_035_aux COMPARING bukrs.

      LOOP AT lt_035 ASSIGNING FIELD-SYMBOL(<fs_035>).

        APPEND INITIAL LINE TO lt_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

        <fs_dados>-bukrs = <fs_035>-bukrs.
        <fs_dados>-objkey = 'ZGL17' && <fs_035>-doc_lcto && <fs_035>-gjahr.

        APPEND INITIAL LINE TO lt_dados ASSIGNING <fs_dados>.

        <fs_dados>-bukrs = <fs_035>-bukrs.
        <fs_dados>-objkey = 'ZGL17' && <fs_035>-doc_lcto && <fs_035>-gjahr && 'R'.

      ENDLOOP.


      SELECT *
        FROM zib_contabil_chv
        INTO TABLE @DATA(lt_contabil)
        FOR ALL ENTRIES IN @lt_dados
        WHERE bukrs = @lt_dados-bukrs
          AND obj_key = @lt_dados-objkey.
      IF sy-subrc IS INITIAL.

        LOOP AT lt_contabil ASSIGNING FIELD-SYMBOL(<fs_contabil>).

          APPEND INITIAL LINE TO e_dados ASSIGNING FIELD-SYMBOL(<fs_edados>).

          <fs_edados>-bukrs = <fs_contabil>-bukrs.
          <fs_edados>-belnr = <fs_contabil>-belnr.
          <fs_edados>-gjahr = <fs_contabil>-gjahr.

        ENDLOOP.

      ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
